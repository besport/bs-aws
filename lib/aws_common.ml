type credentials =
  { access_key_id : string;
    secret_access_key : string }

module Region = struct
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

  let to_string r =
    match r with
    | `us_east_1      -> "us-east-1"
    | `us_west_1      -> "us-west-1"
    | `us_west_2      -> "us-west-2"
    | `eu_west_1      -> "eu-west-1"
    | `eu_central_1   -> "eu-central-1"
    | `ap_southeast_1 -> "ap-southeast-1"
    | `ap_southeast_2 -> "ap-southeast-2"
    | `ap_northeast_1 -> "ap-northeast-1"
    | `sa_east_1      -> "sa-east-1"

  let from_string r =
    match r with
    | "us-east-1"      -> `us_east_1
    | "us-west-1"      -> `us_west_1
    | "us-west-2"      -> `us_west_2
    | "eu-west-1"      -> `eu_west_1
    | "eu-central-1"   -> `eu_central_1
    | "ap-southeast-1" -> `ap_southeast_1
    | "ap-southeast-2" -> `ap_southeast_2
    | "ap-northeast-1" -> `ap_northeast_1
    | "sa-east-1"      -> `sa_east_1
    | _                -> raise Not_found
end

type error =
  { request_id : string;
    code : int; (* HTTP error code *)
    typ : string;
    message : string }

exception Error of error

let enable_debug = Aws_base.Debug.enable
