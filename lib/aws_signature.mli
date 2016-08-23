
type key

val hash : string -> string

val signing_key :
  Aws_common.credentials -> (*date:*)string ->
  Aws_common.Region.t -> (*service:*)string -> key

val sign : key -> string -> string

val credential : key -> string

val sign_request :
  Aws_common.credentials -> service:string -> [< Aws_common.Region.t] ->
  Aws_base.request -> Aws_base.request

(****)

val canonical_request :
  Aws_base.meth -> string ->
  (string * string) list -> (string * string) list ->
  string -> string * string
val string_to_sign : string -> key -> string -> string

val authorization_header : key -> string -> string -> string
