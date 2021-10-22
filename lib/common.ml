type credentials =
  { mutable access_key_id : string
  ; mutable secret_access_key : string
  ; mutable session_token : string option }

let credentials ~access_key_id ~secret_access_key ?session_token () =
  {access_key_id; secret_access_key; session_token}

exception Unknown_region

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
    | `us_east_1 -> "us-east-1"
    | `us_west_1 -> "us-west-1"
    | `us_west_2 -> "us-west-2"
    | `eu_west_1 -> "eu-west-1"
    | `eu_central_1 -> "eu-central-1"
    | `ap_southeast_1 -> "ap-southeast-1"
    | `ap_southeast_2 -> "ap-southeast-2"
    | `ap_northeast_1 -> "ap-northeast-1"
    | `sa_east_1 -> "sa-east-1"

  let from_string r =
    match r with
    | "us-east-1" -> `us_east_1
    | "us-west-1" -> `us_west_1
    | "us-west-2" -> `us_west_2
    | "eu-west-1" -> `eu_west_1
    | "eu-central-1" -> `eu_central_1
    | "ap-southeast-1" -> `ap_southeast_1
    | "ap-southeast-2" -> `ap_southeast_2
    | "ap-northeast-1" -> `ap_northeast_1
    | "sa-east-1" -> `sa_east_1
    | _ -> raise Unknown_region
end

type error = {code : int (* HTTP error code *); typ : string; message : string}

exception Error of error

let () =
  Printexc.register_printer @@ function
  | Error {code; typ; message} ->
      let msg =
        Printf.sprintf "Common.Error {code = %d; type = %s; message = %s}" code
          typ message
      in
      Some msg
  | _ -> None

let enable_debug = Base.Debug.enable
