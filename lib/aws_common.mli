
type credentials =
  { access_key_id : string;
    secret_access_key : string }

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
end

type error =
  { request_id : string;
    code : int; (* HTTP error code *)
    typ : string;
    message : string }

exception Error of error

val enable_debug : string -> unit
