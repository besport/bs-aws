open Batteries

let todo = Obj.magic @@ fun () -> failwith "TODO"

module Make (Conf : Service.CONF) = struct
  exception Parse_error
  exception ResourceInUseException

  let hostname region =
    Format.sprintf "dynamodb.%s.amazonaws.com" (Common.Region.to_string region)

  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "dynamodb"
        and host = hostname Conf.region
      end)

  module Aux = struct
    let single t_of_yojson yojson_of_t =
      ( (fun json -> t_of_yojson @@ `List [json])
      , fun t -> match yojson_of_t t with `List [x] -> x | _ -> assert false )
  end

  type attribute_value =
    | B of bytes
    | BOOL of bool
    | BS of string list
    | L of attribute_value list
    (* | M of TODO *)
    | N of float
    | NS of float list
    | NULL
    | S of string
    | SS of string list
  [@@deriving yojson, show]

  let yojson_of_attribute_value =
    let obj_of_list = function
      | `List [`String x; y] -> `Assoc [x, y]
      | _ -> raise Parse_error
    in
    obj_of_list % yojson_of_attribute_value

  let attribute_value_of_yojson =
    let list_of_obj = function
      | `Assoc [(x, y)] -> `List [`String x; y]
      | _ -> raise Parse_error
    in
    attribute_value_of_yojson % list_of_obj

  type attribute_values = (string * attribute_value) list [@@deriving show]

  let yojson_of_attribute_values l =
    `Assoc (List.map (Tuple2.map2 yojson_of_attribute_value) l)

  let attribute_values_of_yojson = function
    | `Assoc l -> List.map (Tuple2.map2 attribute_value_of_yojson) l
    | _ -> raise Not_found

  let perform ~action ~payload =
    let headers =
      [ "x-amz-target", "DynamoDB_20120810." ^ action
      ; "content-type", "application/x-amz-json-1.0" ]
    in
    let%lwt response_body, _ =
      try%lwt Service.request ~meth:`POST ~uri:"/" ~headers ~payload () with
      | Common.Error
          { code = 400
          ; typ = "com.amazonaws.dynamodb.v20120810#ResourceInUseException" } ->
          Lwt.fail ResourceInUseException
      | exn -> Lwt.fail exn
    in
    Lwt.return @@ Yojson.Safe.from_string response_body

  module GetItem = struct
    type expressionAttributeNames = (string * string) list [@@deriving show]

    let expressionAttributeNames_of_yojson = function
      | `Assoc l ->
          let expressionAttributeName_of_yojson = function
            | name, `String value -> name, value
            | _ -> raise Parse_error
          in
          List.map expressionAttributeName_of_yojson l
      | _ -> raise Parse_error

    let yojson_of_expressionAttributeNames l =
      `Assoc (List.map (fun (name, value) -> name, `String value) l)

    type request =
      { attributesToGet : string list option
            [@yojson.option] [@key "AttributesToGet"]
      ; consistentRead : bool option [@yojson.option] [@key "ConsistentRead"]
      ; expressionAttributeNames : expressionAttributeNames option
            [@key "ExpressionAttributeNames"] [@yojson.option]
      ; key : attribute_values [@key "Key"]
      ; projectionExpression : string option
            [@yojson.option] [@key "ProjectionExpression"]
      ; returnConsumedCapacity : string option
            [@yojson.option] [@key "ReturnConsumedCapacity"]
      ; tableName : string [@key "TableName"] }
    [@@deriving yojson, show]

    type consumedCapacity = Yojson.Safe.t [@@deriving show]

    let consumedCapacity_of_yojson, yojson_of_consumedCapacity =
      identity, identity

    type response =
      { consumedCapacity : consumedCapacity option
            [@yojson.option] [@key "consumedCapacity"]
      ; item : attribute_values option [@yojson.option] [@key "Item"] }
    [@@deriving yojson, show]

    let request ?attributesToGet ?consistentRead
        ?(expressionAttributeNames = []) ?projectionExpression
        ?returnConsumedCapacity ~tableName key
      =
      { attributesToGet
      ; consistentRead
      ; expressionAttributeNames =
          (match expressionAttributeNames with [] -> None | l -> Some l)
      ; key
      ; projectionExpression
      ; returnConsumedCapacity
      ; tableName }
      [@@deriving yojson, show]
  end

  let get_item ?attributesToGet ?consistentRead ?projectionExpression
      ?returnConsumedCapacity ~tableName key
    =
    let open GetItem in
    let payload =
      Yojson.Safe.to_string @@ yojson_of_request
      @@ request ?attributesToGet ?consistentRead ?projectionExpression
           ?returnConsumedCapacity ~tableName key
    in
    let%lwt response = perform ~action:"GetItem" ~payload in
    let {consumedCapacity; item} = response_of_yojson response in
    Lwt.return (consumedCapacity, Option.default [] item)

  let put_item ~tableName items =
    let item = yojson_of_attribute_values items in
    let body = ["TableName", `String tableName; "Item", item] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    perform ~action:"PutItem" ~payload

  module DescribeTable = struct
    type tableDescription =
      {itemCount : int option [@yojson.option] [@key "ItemCount"]}
    [@@deriving yojson, show]

    type result = {table : tableDescription [@key "Table"]}
    [@@deriving yojson, show]
  end

  let describe_table ~tableName =
    let body = ["TableName", `String tableName] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    let%lwt response = perform ~action:"DescribeTable" ~payload in
    Lwt.return @@ DescribeTable.result_of_yojson response

  module AttributeDefinition = struct
    type attributeType = [`S | `N | `B] [@@deriving yojson, show]

    let attributeType_of_yojson, yojson_of_attributeType =
      Aux.single attributeType_of_yojson yojson_of_attributeType

    type t =
      { attributeName : string [@key "AttributeName"]
      ; attributeType : attributeType [@key "AttributeType"] }
    [@@deriving yojson, show]
  end

  module KeySchemaElement = struct
    type keyType = [`HASH | `RANGE] [@@deriving yojson, show]

    let keyType_of_yojson, yojson_of_keyType =
      Aux.single keyType_of_yojson yojson_of_keyType

    type t =
      { attributeName : string [@key "AttributeName"]
      ; keyType : keyType [@key "KeyType"] }
    [@@deriving yojson, show]
  end

  module CreateTable = struct
    type request =
      { attributeDefinitions : AttributeDefinition.t list
            [@key "AttributeDefinitions"]
      ; keySchema : KeySchemaElement.t list [@key "KeySchema"]
      ; tableName : string [@key "TableName"]
      ; billingMode : string [@key "BillingMode"] }
    [@@deriving yojson, show]
  end

  let create_table ~tableName ~attributes ~primary_key ?sort_key () =
    let attributeDefinitions =
      let attribute_definition (attributeName, attributeType) =
        {AttributeDefinition.attributeName; attributeType}
      in
      List.map attribute_definition attributes
    and keySchema =
      let primary_schema_element =
        {KeySchemaElement.attributeName = primary_key; keyType = `HASH}
      and sort_key_element =
        sort_key
        |> Option.map @@ fun sk ->
           {KeySchemaElement.attributeName = sk; keyType = `RANGE}
      in
      match sort_key_element with
      | None -> [primary_schema_element]
      | Some ske -> [primary_schema_element; ske]
    in
    let payload =
      Yojson.Safe.to_string @@ CreateTable.yojson_of_request
      @@ { CreateTable.attributeDefinitions
         ; keySchema
         ; tableName
         ; billingMode = "PAY_PER_REQUEST" }
    in
    perform ~action:"CreateTable" ~payload

  module DeleteItem = struct
    type request =
      { key : attribute_values [@key "Key"]
      ; tableName : string [@key "TableName"] }
    [@@deriving yojson, show]
  end

  let delete_item ~tableName items =
    let payload =
      Yojson.Safe.to_string @@ DeleteItem.yojson_of_request
      @@ {DeleteItem.key = items; tableName}
    in
    perform ~action:"DeleteItem" ~payload
end
