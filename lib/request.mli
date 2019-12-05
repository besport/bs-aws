val perform :
  credentials:Common.credentials ->
  service:string ->
  region:[< Common.Region.t] ->
  ?secure:bool ->
  meth:Base.meth ->
  host:string ->
  ?port:int ->
  uri:string ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?payload:string ->
  unit -> (string * Cohttp.Header.t) Lwt.t

module type CONF = sig
  val credentials : Common.credentials
  val service : string
  val region : Common.Region.t
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
    meth:Base.meth ->
    uri:string ->
    ?query:(string * string) list ->
    ?headers:(string * string) list ->
    ?payload:string ->
    unit -> (string * Cohttp.Header.t) Lwt.t
end

module NoAws (Conf : NO_AWS_CONF) : SERVICE
module Service (Conf : CONF) : SERVICE
