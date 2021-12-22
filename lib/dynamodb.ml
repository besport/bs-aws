module B64 = Base64
open Batteries

let todo = Obj.magic @@ fun () -> failwith "TODO"

type yojson = Yojson.Safe.t [@@deriving show]

let yojson_of_yojson = identity

module Make (Conf : Service.CONF) = struct
  exception Parse_error
  exception Encoding_error of string
  exception Decoding_error of string
  exception ResourceInUse of string
  exception ResourceNotFound of string
  exception ConditionalCheckFailed of string
  exception ValidationException of string

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

    let parse_response ~__LOC__:loc p response =
      try Lwt.return @@ p response
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _ as exn ->
        prerr_endline @@ loc ^ ": failed to parse response: ";
        prerr_endline @@ Yojson.Safe.to_string response;
        Lwt.fail exn

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
  end

  let encode_base64 str =
    match B64.encode str with
    | Ok b -> b
    | Error (`Msg msg) -> raise @@ Encoding_error msg

  let decode_base64 str =
    match B64.decode str with
    | Ok s -> s
    | Error (`Msg msg) -> raise @@ Decoding_error msg

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
  [@@deriving yojson, show]

  let yojson_of_attribute_value = function
    | B str -> `Assoc ["B", `String (encode_base64 str)]
    | N f -> `Assoc ["N", `String (string_of_float f)]
    | a ->
        let obj_of_list = function
          | `List [`String x; y] -> `Assoc [x, y]
          | _ -> raise Parse_error
        in
        obj_of_list @@ yojson_of_attribute_value a

  let attribute_value_of_yojson = function
    | `Assoc [("B", `String str)] -> B (decode_base64 str)
    | `Assoc [("N", `String str)] -> N (float_of_string str)
    | a ->
        let list_of_obj = function
          | `Assoc [(x, y)] -> `List [`String x; y]
          | _ -> raise Parse_error
        in
        attribute_value_of_yojson @@ list_of_obj a

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
      let prerr_context () =
        prerr_endline @@ __LOC__ ^ ": error during request:";
        prerr_endline @@ action ^ " " ^ payload
      in
      try%lwt Service.request ~meth:`POST ~uri:"/" ~headers ~payload () with
      | Common.Error
          { code = 400
          ; typ = "com.amazonaws.dynamodb.v20120810#ResourceInUseException"
          ; message } ->
          Lwt.fail @@ ResourceInUse message
      | Common.Error
          { code = 400
          ; typ = "com.amazonaws.dynamodb.v20120810#ResourceNotFoundException"
          ; message } ->
          prerr_context ();
          Lwt.fail @@ ResourceNotFound message
      | Common.Error
          { code = 400
          ; typ =
              "com.amazonaws.dynamodb.v20120810#ConditionalCheckFailedException"
          ; message } ->
          Lwt.fail @@ ConditionalCheckFailed message
      | Common.Error
          { code = 400
          ; typ = "com.amazon.coral.validate#ValidationException"
          ; message } ->
          prerr_context ();
          Lwt.fail @@ ValidationException message
      | exn -> prerr_context (); Lwt.fail exn
    in
    try Lwt.return @@ Yojson.Safe.from_string response_body
    with exn ->
      prerr_endline @@ __LOC__ ^ ": error while parsing JSON:";
      prerr_endline response_body;
      Lwt.fail exn

  module GetItem = struct
    type request =
      { consistent_read : bool option [@option] [@key "ConsistentRead"]
      ; expression_attribute_names : Aux.expression_attribute_names option
            [@key "ExpressionAttributeNames"] [@option]
      ; key : attribute_values [@key "Key"]
      ; projection_expression : string option
            [@option] [@key "ProjectionExpression"]
      ; return_consumed_capacity : string option
            [@option] [@key "ReturnConsumedCapacity"]
      ; table_name : string [@key "TableName"] }
    [@@deriving yojson_of, show]

    type response =
      { consumed_capacity : yojson option [@option] [@key "consumedCapacity"]
      ; item : attribute_values option [@option] [@key "Item"] }
    [@@deriving of_yojson, show] [@@allow_extra_fields]

    let request ?consistent_read ?(expression_attribute_names = [])
        ?projection_expression ?return_consumed_capacity ~table key
      =
      { consistent_read
      ; expression_attribute_names =
          (match expression_attribute_names with [] -> None | l -> Some l)
      ; key
      ; projection_expression
      ; return_consumed_capacity
      ; table_name = table }
      [@@deriving yojson, show]
  end

  let get_item ?consistent_read ?projection_expression ?return_consumed_capacity
      ~table key
    =
    let open GetItem in
    let payload =
      Yojson.Safe.to_string @@ yojson_of_request
      @@ request ?consistent_read ?projection_expression
           ?return_consumed_capacity ~table key
    in
    let%lwt response = perform ~action:"GetItem" ~payload in
    Aux.parse_response ~__LOC__ response_of_yojson response

  module ConditionExpression = struct
    open Format

    type operand = string [@@deriving show]
    type comparator = Eq | NEq | LT | LEq | GT | GEq

    let pp_comparator f = function
      | Eq -> fprintf f "="
      | NEq -> fprintf f "<>"
      | LT -> fprintf f "<"
      | LEq -> fprintf f "<="
      | GT -> fprintf f ">"
      | GEq -> fprintf f ">="

    type funct =
      | Attribute_exists of string
      | Attribute_not_exists of string
      | Attribute_type of string * string
      | Begins_with of string * string
      | Contains of string * string
      | Size of string

    let pp_funct f = function
      | Attribute_exists path -> fprintf f "attribute_exists (%s)" path
      | Attribute_not_exists path -> fprintf f "attribute_not_exists (%s)" path
      | Attribute_type (path, t) -> fprintf f "attribute_type (%s, %s)" path t
      | Begins_with (path, substr) ->
          fprintf f "begins_with (%s, %s)" path substr
      | Contains (path, operand) ->
          fprintf f "contains (%s, %s)" path (show_operand operand)
      | Size path -> fprintf f "size (%s)" path

    type t =
      | Comparison of operand * comparator * operand
      | Between_and of operand * operand * operand
      | In of operand * operand list
      | Function of funct
      | And of t * t
      | Or of t * t
      | Not of t
      | Paren of t

    let rec pp f = function
      | Comparison (o1, c, o2) ->
          fprintf f "%a %a %a" pp_operand o1 pp_comparator c pp_operand o2
      | Between_and (o1, o2, o3) ->
          fprintf f "%a BETWEEN %a AND %a" pp_operand o1 pp_operand o2
            pp_operand o3
      | In (o, operands) ->
          fprintf f "%a IN %a" pp_operand o
            (pp_print_list ~pp_sep:todo pp_operand)
            operands
      | Function funct -> pp_funct f funct
      | And (c1, c2) -> fprintf f "%a AND %a" pp c1 pp c2
      | Or (c1, c2) -> fprintf f "%a OR %a" pp c1 pp c2
      | Not c -> fprintf f "NOT %a" pp c
      | Paren c -> fprintf f "(%a)" pp c

    let yojson_of_t t =
      `String Format.(pp str_formatter t; flush_str_formatter ())
  end

  module PutItem = struct
    type request =
      { item : attribute_values [@key "Item"]
      ; table_name : string [@key "TableName"]
      ; condition_expression : ConditionExpression.t option
            [@option] [@key "ConditionExpression"]
      ; return_values : [`NONE | `ALL_OLD] option
            [@option] [@key "ReturnValues"] }
    [@@deriving yojson_of, show]

    type response =
      { attributes : attribute_values option [@option] [@key "Attributes"]
      ; consumed_capacity : yojson option [@option] [@key "consumedCapacity"]
      ; item_collection_metrics : yojson option
            [@option] [@key "ItemCollectionMetrics"] }
    [@@deriving of_yojson, show]
  end

  let put_item ~table ?condition_expression ?return_values items =
    let open PutItem in
    let payload =
      Yojson.Safe.to_string @@ yojson_of_request
      @@ {item = items; table_name = table; condition_expression; return_values}
    in
    try%lwt
      let%lwt response = perform ~action:"PutItem" ~payload in
      Aux.parse_response ~__LOC__ response_of_yojson response
    with ConditionalCheckFailed _ ->
      Lwt.return
        { attributes = None
        ; consumed_capacity = None
        ; item_collection_metrics = None }

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
    type response = {table : TableDescription.t [@key "Table"]}
    [@@deriving of_yojson, show]
  end

  let describe_table table =
    let open DescribeTable in
    let body = ["TableName", `String table] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    let%lwt response = perform ~action:"DescribeTable" ~payload in
    Aux.parse_response ~__LOC__ response_of_yojson response

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
    [@@deriving yojson_of, show]
  end

  let create_table ~attributes ~primary_key ?sort_key table =
    let open CreateTable in
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
      Yojson.Safe.to_string @@ yojson_of_request
      @@ { attribute_definitions
         ; key_schema
         ; table_name = table
         ; billing_mode = "PAY_PER_REQUEST" }
    in
    let%lwt response = perform ~action:"CreateTable" ~payload in
    Aux.parse_response ~__LOC__ TableDescription.t_of_yojson response

  module DeleteItem = struct
    type request =
      { key : attribute_values [@key "Key"]
      ; table_name : string [@key "TableName"] }
    [@@deriving yojson_of, show]
  end

  let delete_item ~table items =
    let payload =
      Yojson.Safe.to_string @@ DeleteItem.yojson_of_request
      @@ {DeleteItem.key = items; table_name = table}
    in
    perform ~action:"DeleteItem" ~payload

  module BatchWriteItem = struct
    type delete_request = {key : attribute_value [@key "Key"]}
    [@@deriving yojson_of]

    type write_request =
      | DeleteRequest of delete_request
      | PutRequest of attribute_values

    let yojson_of_write_request = function
      | DeleteRequest r -> `Assoc ["DeleteRequest", yojson_of_delete_request r]
      | PutRequest r ->
          `Assoc ["PutRequest", `Assoc ["Item", yojson_of_attribute_values r]]

    type request_items = (string * write_request list) list

    let yojson_of_request_items request_items =
      let request_item (table, requests) =
        table, `List (List.map yojson_of_write_request requests)
      in
      `Assoc (List.map request_item request_items)

    type request =
      { request_items : request_items [@key "RequestItems"]
      ; consumed_capacity : yojson option [@option] [@key "consumedCapacity"]
      ; item_collection_metrics : yojson option }
    [@@deriving yojson_of]
  end

  let batch_write_item request_items =
    let payload =
      Yojson.Safe.to_string
      @@ BatchWriteItem.yojson_of_request
           { request_items
           ; consumed_capacity = None
           ; item_collection_metrics = None }
    in
    perform ~action:"BatchWriteItem" ~payload

  module Query = struct
    type exclusive_start_key = string * attribute_value [@@deriving show]

    let yojson_of_exclusive_start_key (key, value) =
      `Assoc [key, yojson_of_attribute_value value]

    type request =
      { consistent_read : bool option [@option] [@key "ConsistentRead"]
      ; exclusive_start_key : exclusive_start_key option
            [@option] [@key "ExclusiveStartKey"]
      ; expression_attribute_names : Aux.expression_attribute_names option
            [@option] [@key "ExpressionAttributeNames"]
      ; expression_attribute_values : attribute_values option
            [@option] [@key "ExpressionAttributeValues"]
      ; filter_expression : string option [@option] [@key "FilterExpression"]
      ; index_name : string option [@option] [@key "IndexName"]
      ; key_condition_expression : string [@key "KeyConditionExpression"]
      ; limit : int option [@option] [@key "Limit"]
      ; projection_expression : string option
            [@option] [@key "ProjectionExpression"]
      ; return_consumed_capacity : string option
            [@option] [@key "ReturnConsumedCapacity"]
      ; scan_index_forward : bool option [@option] [@key "ScanIndexForward"]
      ; select : string option [@option] [@key "Select"]
      ; table_name : string [@key "TableName"] }
    [@@deriving yojson_of, show]

    type response =
      { count : int [@key "Count"]
      ; items : attribute_values [@key "Items"]
      ; last_evaluated_key : attribute_value option
            [@option] [@key "LastEvaluatedKey"]
      ; scanned_count : int [@key "ScannedCount"] }
    [@@deriving of_yojson, show]
  end

  let query ?consistent_read ?exclusive_start_key ?expression_attribute_names
      ?expression_attribute_values ?filter_expression ?index_name
      ~key_condition_expression ?limit ?projection_expression
      ?return_consumed_capacity ?scan_index_forward ?select ~table ()
    =
    let open Query in
    let request =
      { consistent_read
      ; exclusive_start_key
      ; expression_attribute_names
      ; expression_attribute_values
      ; filter_expression
      ; index_name
      ; key_condition_expression
      ; limit
      ; projection_expression
      ; return_consumed_capacity
      ; scan_index_forward
      ; select
      ; table_name = table }
    in
    let payload = Yojson.Safe.to_string @@ yojson_of_request request in
    let%lwt response = perform ~action:"Query" ~payload in
    Aux.parse_response ~__LOC__ response_of_yojson response
end
