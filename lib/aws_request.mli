val perform :
  credentials:Aws_common.credentials ->
  service:string ->
  region:[< Aws_common.Region.t] ->
  ?secure:bool ->
  meth:Aws_base.meth ->
  host:string ->
  ?port:int ->
  uri:string ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?payload:string ->
  unit -> (string * Cohttp.Header.t) Lwt.t

module type CONF = sig
  val credentials : Aws_common.credentials
  val service : string
  val region : Aws_common.Region.t
  val secure : bool option
  val host : string
end

module type NO_AWS_CONF = sig
  val secure : bool
  val host : string
  val port : int option
end

module type SERVICE = sig
  val perform :
    meth:Aws_base.meth ->
    uri:string ->
    ?query:(string * string) list ->
    ?headers:(string * string) list ->
    ?payload:string ->
    unit -> (string * Cohttp.Header.t) Lwt.t
end

module NoAws (Conf : NO_AWS_CONF) : SERVICE
module Service (Conf : CONF) : SERVICE
