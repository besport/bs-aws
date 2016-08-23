
val perform :
  credentials:Aws_common.credentials ->
  service:string ->
  region:[< Aws_common.Region.t] ->
  ?secure:bool ->
  meth:Aws_base.meth ->
  host:string ->
  uri:string ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?payload:string ->
  unit -> string Lwt.t
