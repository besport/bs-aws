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
val from_ISO8601 : string -> float

val url_encode : string -> string
val encode_form_string : string -> string

module Debug : sig
  val make : string -> string -> string list -> (unit -> bool)
  val enable : string -> unit
  val all : unit -> bool
end

module Xml : sig
  val element : (Xmlm.input -> 'a) -> Xmlm.input -> string * 'a
  val repeat : (Xmlm.input -> 'a) -> Xmlm.input -> 'a list
  val record : Xmlm.input -> (string * string) list
end

module Param : sig
  val string :
    string -> string option ->
    (string * string) list -> (string * string) list
  val int :
    string -> int option ->
    (string * string) list -> (string * string) list
  val bool :
    string -> bool option ->
    (string * string) list -> (string * string) list
  val custom :
    string -> ('a -> string) -> 'a option ->
    (string * string) list -> (string * string) list
end
