type error = Handled of string | Unhandled

let endpoint region =
  Format.sprintf "lambda.%s.amazonaws.com" (Common.Region.to_string region)

module Make (Conf : Service.CONF) = struct
  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "lambda"
        and host = endpoint Conf.region
      end)

  let invoke ?client_context ?invocation_type ?log_type ?qualifier ?payload
      ~function_name ()
    =
    let query = [] |> Base.Param.string "Qualifier" qualifier in
    let uri =
      Printf.sprintf "/2015-03-31/functions/%s/invocations" function_name
    in
    let headers =
      let open Base.Param in
      []
      |> custom "x-amz-client-context"
           (fun c -> Base64.encode_string (Yojson.Safe.to_string c))
           client_context
      |> custom "x-amz-invocation-type"
           (fun t ->
             match t with
             | `Event -> "Event"
             | `Request_response -> "RequestResponse"
             | `Dry_run -> "DryRun")
           invocation_type
      |> custom "x-amz-log-type"
           (fun t -> match t with `None -> "None" | `Tail -> "Tail")
           log_type
    in
    let payload =
      match payload with
      | None -> None
      | Some payload -> Some (Yojson.Safe.to_string payload)
    in
    let%lwt res, headers =
      Service.request ~meth:`POST ~uri ~query ~headers ?payload ()
    in
    Lwt.return
    @@
    match Cohttp.Header.get headers "x-amz-function-error" with
    | Some "Unhandled" -> Error Unhandled
    | Some "Handled" -> Error (Handled res)
    | Some _ -> assert false
    | None -> Ok res
end
