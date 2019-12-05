
type key

val hash : string -> string

val signing_key :
  Common.credentials -> (*date:*)string ->
  Common.Region.t -> (*service:*)string -> key

val sign : key -> string -> string

val credential : key -> string

val sign_request :
  Common.credentials -> service:string -> [< Common.Region.t] ->
  Base.request -> Base.request

val sign_request_using_query_parameters :
  Common.credentials -> service:string -> [< Common.Region.t] ->
  expiration:int -> ?unsigned_payload:bool ->
  Base.request -> Base.request

(****)

val canonical_request :
  Base.meth -> string ->
  (string * string) list -> (string * string) list ->
  ?unsigned_payload:bool -> string -> string * string
val string_to_sign : string -> key -> string -> string

val authorization_header : key -> string -> string -> string
