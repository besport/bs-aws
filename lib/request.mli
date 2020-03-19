val perform
  :  credentials:Common.credentials
  -> service:string
  -> region:[< Common.Region.t]
  -> ?secure:bool
  -> meth:Base.meth
  -> host:string
  -> ?port:int
  -> uri:string
  -> ?query:(string * string) list
  -> ?headers:(string * string) list
  -> ?payload:string
  -> unit
  -> (string * Cohttp.Header.t) Lwt.t

val simple_perform
  :  secure:bool
  -> meth:Base.meth
  -> host:string
  -> ?port:int
  -> uri:string
  -> ?query:(string * string) list
  -> ?headers:(string * string) list
  -> ?payload:string
  -> unit
  -> (string * Cohttp.Header.t) Lwt.t
