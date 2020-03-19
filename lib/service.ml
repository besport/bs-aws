module type CONF = sig
  val credentials : Common.credentials Lwt.t
  val region : Common.Region.t
  val secure : bool
end

module type CONF_NO_AWS = sig
  val secure : bool
  val host : string
  val port : int option
end

module type NAME = sig
  val host : string
  val name : string
end

module type S = sig
  val request
    :  meth:Base.meth
    -> ?host:string
    -> uri:string
    -> ?query:(string * string) list
    -> ?headers:(string * string) list
    -> ?payload:string
    -> unit
    -> (string * Cohttp.Header.t) Lwt.t
end

module Make (Conf : CONF) (N : NAME) : S = struct
  let request ~meth ?(host = N.host) ~uri ?query ?headers ?payload () =
    let%lwt credentials = Conf.credentials in
    Request.perform ~credentials ~service:N.name ~region:Conf.region
      ~secure:Conf.secure ~meth ~host ~uri ?query ?headers ?payload ()
end

module MakeNoAws (Conf : CONF_NO_AWS) : S = struct
  let request ~meth ?(host = Conf.host) ~uri ?query ?headers ?payload () =
    Request.simple_perform ~secure:Conf.secure ~meth ~host ?port:Conf.port ~uri
      ?query ?headers ?payload ()
end
