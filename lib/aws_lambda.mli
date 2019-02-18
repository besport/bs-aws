
val invoke :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  ?client_context:Yojson.Safe.t ->
  ?invocation_type:[ `Dry_run | `Event | `Request_response ] ->
  ?log_type:[ `None | `Tail ] ->
  ?qualifier:string ->
  ?payload:Yojson.Safe.t ->
  function_name:string -> unit -> string Lwt.t
