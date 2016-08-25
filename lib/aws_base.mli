(* Utility functions, used internally. *)

type meth = [`GET | `POST | `PUT | `DELETE | `HEAD]

val string_of_meth : meth -> string

type request =
  { secure: bool;
    meth : meth;
    uri : string;
    query : (string * string) list;
    headers : (string * string) list;
    payload : string }

val request :
  ?secure:bool -> meth:meth -> host:string -> uri:string ->
  ?query:(string * string) list -> ?headers:(string * string) list ->
  ?payload:string -> unit ->
  request

val print_curl_request : request -> unit

val to_ISO8601 : ?extended:bool -> float -> string

val url_encode : string -> string

module Debug : sig
  val make : string -> string -> string list -> (unit -> bool)
  val enable : string -> unit
  val all : unit -> bool
end
