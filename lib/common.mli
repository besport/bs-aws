type credentials =
  { mutable access_key_id : string
  ; mutable secret_access_key : string
  ; mutable session_token : string option }

val credentials
  :  access_key_id:string
  -> secret_access_key:string
  -> ?session_token:string
  -> unit
  -> credentials

exception Unknown_region

module Region : sig
  type t =
    [ `us_east_1
    | `us_west_1
    | `us_west_2
    | `eu_west_1
    | `eu_central_1
    | `ap_southeast_1
    | `ap_southeast_2
    | `ap_northeast_1
    | `sa_east_1 ]

  val to_string : [< t] -> string
  val from_string : string -> t
end

type error = {code : int (* HTTP error code *); typ : string; message : string}

exception Error of error

val enable_debug : string -> unit
