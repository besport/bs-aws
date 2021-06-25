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

  let describe_table ~table_name =
    let headers =
      [ "x-amz-target", "DynamoDB_20120810.DescribeTable"
      ; "content-type", "application/x-amz-json-1.1" ]
    in
    let body = ["TableName", `String table_name] in
    let payload = Yojson.Safe.to_string (`Assoc body) in
    let%lwt response_body, _ =
      Service.request ~meth:`POST ~uri:"/" ~headers ~payload ()
    in
    Lwt.return @@ Yojson.Basic.from_string response_body
end
