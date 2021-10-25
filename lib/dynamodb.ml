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

  type yojson = Yojson.Safe.t [@@deriving show]

  let yojson_of_yojson = identity

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
    type expression_attribute_names = (string * string) list [@@deriving show]

    let expression_attribute_names_of_yojson = function
      | `Assoc l ->
          let expression_attribute_name_of_yojson = function
            | name, `String value -> name, value
            | _ -> raise Parse_error
          in
          List.map expression_attribute_name_of_yojson l
      | _ -> raise Parse_error

    let yojson_of_expression_attribute_names l =
      `Assoc (List.map (fun (name, value) -> name, `String value) l)

    type request =
      { attributes_to_get : string list option [@option] [@key "AttributesToGet"]
      ; consistent_read : bool option [@option] [@key "ConsistentRead"]
      ; expression_attribute_names : expression_attribute_names option
            [@key "ExpressionAttributeNames"] [@option]
      ; key : attribute_values [@key "Key"]
      ; projection_expression : string option
            [@option] [@key "ProjectionExpression"]
      ; return_consumed_capacity : string option
            [@option] [@key "ReturnConsumedCapacity"]
      ; table_name : string [@key "TableName"] }
    [@@deriving yojson, show]

    type response =
      { consumed_capacity : yojson option [@option] [@key "consumedCapacity"]
      ; item : attribute_values option [@option] [@key "Item"] }
    [@@deriving yojson, show] [@@allow_extra_fields]

    let request ?attributes_to_get ?consistent_read
        ?(expression_attribute_names = []) ?projection_expression
        ?return_consumed_capacity ~table key
      =
      { attributes_to_get
      ; consistent_read
      ; expression_attribute_names =
          (match expression_attribute_names with [] -> None | l -> Some l)
      ; key
      ; projection_expression
      ; return_consumed_capacity
      ; table_name = table }
      [@@deriving yojson, show]
  end

  let get_item ?attributes_to_get ?consistent_read ?projection_expression
      ?return_consumed_capacity ~table key
    =
    let open GetItem in
    let payload =
      Yojson.Safe.to_string @@ yojson_of_request
      @@ request ?attributes_to_get ?consistent_read ?projection_expression
           ?return_consumed_capacity ~table key
    in
    let%lwt response = perform ~action:"GetItem" ~payload in
    let {consumed_capacity; item} = response_of_yojson response in
    Lwt.return (consumed_capacity, Option.default [] item)

  let put_item ~table items =
    let item = yojson_of_attribute_values items in
    let body = ["TableName", `String table; "Item", item] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    perform ~action:"PutItem" ~payload

  module TableDescription = struct
    type t =
      { archival_summary : yojson option [@option] [@key "ArchivalSummary"]
      ; attribute_definitions : yojson option
            [@option] [@key "AttributeDefinitions"]
      ; billing_mode_summary : yojson option
            [@option] [@key "BillingModeSummary"]
      ; creation_date_time : yojson option [@option] [@key "CreationDateTime"]
      ; global_secondary_indexes : yojson option
            [@option] [@key "GlobalSecondaryIndexes"]
      ; global_table_version : yojson option
            [@option] [@key "GlobalTableVersion"]
      ; item_count : int option [@option] [@key "ItemCount"]
      ; key_schema : yojson option [@option] [@key "KeySchema"]
      ; latest_stream_arn : yojson option [@option] [@key "LatestStreamArn"]
      ; latest_stream_label : yojson option [@option] [@key "LatestStreamLabel"]
      ; local_secondary_indexes : yojson option
            [@option] [@key "LocalSecondaryIndexes"]
      ; provisioned_throughput : yojson option
            [@option] [@key "ProvisionedThroughput"]
      ; replicas : yojson option [@option] [@key "Replicas"]
      ; restore_summary : yojson option [@option] [@key "RestoreSummary"]
      ; sse_description : yojson option [@option] [@key "SSEDescription"]
      ; stream_specification : yojson option
            [@option] [@key "StreamSpecification"]
      ; table_arn : yojson option [@option] [@key "TableArn"]
      ; table_id : yojson option [@option] [@key "TableId"]
      ; table_name : yojson option [@option] [@key "TableName"]
      ; table_size_bytes : yojson option [@option] [@key "TableSizeBytes"]
      ; table_status : yojson option [@option] [@key "TableStatus"] }
    [@@deriving of_yojson, show] [@@allow_extra_fields]
  end

  module DescribeTable = struct
    type result = {table : TableDescription.t [@key "Table"]}
    [@@deriving of_yojson, show]
  end

  let describe_table table =
    let body = ["TableName", `String table] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    let%lwt response = perform ~action:"DescribeTable" ~payload in
    print_endline @@ Yojson.Safe.to_string response;
    Lwt.return @@ DescribeTable.result_of_yojson response

  module AttributeDefinition = struct
    type attribute_type = [`S | `N | `B] [@@deriving yojson, show]

    let attribute_type_of_yojson, yojson_of_attribute_type =
      Aux.single attribute_type_of_yojson yojson_of_attribute_type

    type t =
      { attribute_name : string [@key "AttributeName"]
      ; attribute_type : attribute_type [@key "AttributeType"] }
    [@@deriving yojson, show]
  end

  module KeySchemaElement = struct
    type key_type = [`HASH | `RANGE] [@@deriving yojson, show]

    let key_type_of_yojson, yojson_of_key_type =
      Aux.single key_type_of_yojson yojson_of_key_type

    type t =
      { attribute_name : string [@key "AttributeName"]
      ; key_type : key_type [@key "KeyType"] }
    [@@deriving yojson, show]
  end

  module CreateTable = struct
    type request =
      { attribute_definitions : AttributeDefinition.t list
            [@key "AttributeDefinitions"]
      ; key_schema : KeySchemaElement.t list [@key "KeySchema"]
      ; table_name : string [@key "TableName"]
      ; billing_mode : string [@key "BillingMode"] }
    [@@deriving yojson, show]
  end

  let create_table ~attributes ~primary_key ?sort_key table =
    let attribute_definitions =
      let attribute_definition (attribute_name, attribute_type) =
        {AttributeDefinition.attribute_name; attribute_type}
      in
      List.map attribute_definition attributes
    and key_schema =
      let primary_schema_element =
        {KeySchemaElement.attribute_name = primary_key; key_type = `HASH}
      and sort_key_element =
        sort_key
        |> Option.map @@ fun sk ->
           {KeySchemaElement.attribute_name = sk; key_type = `RANGE}
      in
      match sort_key_element with
      | None -> [primary_schema_element]
      | Some ske -> [primary_schema_element; ske]
    in
    let payload =
      Yojson.Safe.to_string @@ CreateTable.yojson_of_request
      @@ { CreateTable.attribute_definitions
         ; key_schema
         ; table_name = table
         ; billing_mode = "PAY_PER_REQUEST" }
    in
    let%lwt response = perform ~action:"CreateTable" ~payload in
    Lwt.return @@ TableDescription.t_of_yojson response

  module DeleteItem = struct
    type request =
      { key : attribute_values [@key "Key"]
      ; table_name : string [@key "TableName"] }
    [@@deriving yojson, show]
  end

  let delete_item ~table items =
    let payload =
      Yojson.Safe.to_string @@ DeleteItem.yojson_of_request
      @@ {DeleteItem.key = items; table_name = table}
    in
    perform ~action:"DeleteItem" ~payload
end
