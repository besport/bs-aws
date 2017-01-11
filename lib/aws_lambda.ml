
let endpoint region =
  Format.sprintf "lambda.%s.amazonaws.com" (Aws_common.Region.to_string region)

let invoke
    ~credentials ~region ?client_context ?invocation_type ?log_type ?qualifier
    ?payload ~function_name () =
  let query = [] |> Aws_base.Param.string "Qualifier" qualifier in
  let uri =
    Printf.sprintf "/2015-03-31/functions/%s/invocations" function_name in
  let headers =
    let open Aws_base.Param in
    []
    |> custom "x-amz-client-context"
         (fun c -> B64.encode (Yojson.Safe.to_string  c))
         client_context
    |> custom "x-amz-invocation-type"
         (fun t ->
            match t with
            | `Event -> "Event"
            | `Request_response -> "RequestResponse"
            | `Dry_run -> "DryRun")
         invocation_type
    |> custom "x-amz-log-type"
         (fun t ->
            match t with
            | `None -> "None"
            | `Tail -> "Tail")
         log_type
  in
  let payload =
    match payload with
    | None         -> None
    | Some payload -> Some (Yojson.Safe.to_string payload)
  in
  Aws_request.perform
    ~credentials ~service:"lambda" ~region ~meth:`POST
    ~host:(endpoint region) ~uri ~query ~headers ?payload ()
