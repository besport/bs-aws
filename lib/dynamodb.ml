exception Parse_error

module Make (Conf : Service.CONF) = struct
  let hostname region =
    Format.sprintf "dynamodb.%s.amazonaws.com" (Common.Region.to_string region)

  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "dynamodb"
        and host = hostname Conf.region
      end)

  type attribute_value =
    | B of string
    | BOOL of bool
    | BS of string list
    | L of attribute_value list
    (* | M of TODO *)
    | N of float
    | NS of float list
    | NULL
    | S of string
    | SS of string list
  [@@deriving yojson]

  let yojson_of_attribute_value =
    let obj_of_list = function
      | `List [`String x; y] -> `Assoc [x, y]
      | _ -> assert false
    in
    fun x -> obj_of_list @@ yojson_of_attribute_value x

  let attribute_value_of_yojson =
    let list_of_obj = function
      | `Assoc [(x, y)] -> `List [`String x; y]
      | _ -> assert false
    in
    fun x -> attribute_value_of_yojson @@ list_of_obj x

  let perform ~action ~payload =
    let headers =
      [ "x-amz-target", "DynamoDB_20120810." ^ action
      ; "content-type", "application/x-amz-json-1.0" ]
    in
    let%lwt response_body, _ =
      Service.request ~meth:`POST ~uri:"/" ~headers ~payload ()
    in
    Lwt.return @@ Yojson.Safe.from_string response_body

  module GetItem = struct
    type expressionAttributeNames = (string * string) list

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

    type attribute_values = (string * attribute_value) list

    let tuple2_map2 f (x, y) = x, f y

    let attribute_values_of_yojson = function
      | `Assoc l -> List.map (tuple2_map2 attribute_value_of_yojson) l
      | _ -> raise Not_found

    let yojson_of_attribute_values l =
      `Assoc (List.map (tuple2_map2 yojson_of_attribute_value) l)

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
    [@@deriving yojson]

    type consumedCapacity = Yojson.Safe.t

    let consumedCapacity_of_yojson, yojson_of_consumedCapacity =
      (fun x -> x), fun x -> x

    type response =
      { consumedCapacity : consumedCapacity option
            [@yojson.option] [@key "consumedCapacity"]
      ; item : attribute_values option [@yojson.option] [@key "Item"] }
    [@@deriving yojson]

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
      [@@deriving yojson]
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
    Lwt.return (consumedCapacity, Option.value ~default:[] item)

  let describe_table ~tableName =
    let body = ["TableName", `String tableName] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    perform ~action:"DescribeTable" ~payload
end
