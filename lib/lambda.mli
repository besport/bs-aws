type error = Handled of string | Unhandled

module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  val invoke
    :  ?client_context:Yojson.Safe.t
    -> ?invocation_type:[`Dry_run | `Event | `Request_response]
    -> ?log_type:[`None | `Tail]
    -> ?qualifier:string
    -> ?payload:Yojson.Safe.t
    -> function_name:string
    -> unit
    -> (string, error) result Lwt.t
end
