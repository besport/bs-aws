
let endpoint region =
  Format.sprintf "mediaconvert.%s.amazonaws.com"
    (Aws_common.Region.to_string region)

let describe_endpoints
    ~credentials ~region ?next_token ?max_results ?mode () =
  let uri = "/2017-08-29/endpoints" in
  let query =
    let open Aws_base.Param in
    []
    |> string "nextToken" next_token
    |> int "maxResults" max_results
    |> custom "mode"
         (fun m ->
            match m with
            | `DEFAULT  -> "DEFAULT"
            | `GET_ONLY -> "GET_ONLY")
         mode
  in
  let%lwt (res, headers) =
    Aws_request.perform
      ~credentials ~service:"mediaconvert" ~region ~meth:`POST
      ~host:(endpoint region) ~uri ~query ()
  in
  let res = Yojson.Safe.from_string res in
  let open Yojson.Safe.Util in
  let endpoints =
    let e = member "endpoints" res in
    if e = `Null then
      []
    else
      List.map (fun u -> to_string @@ member "url" u) (to_list e)
  in
  let next_token = to_string_option @@ member "nextToken" res in
  Lwt.return (endpoints, next_token)

let endpoint_host endpoint =
  let len = String.length endpoint in
  assert (String.length endpoint >= 8 && String.sub endpoint 0 8 = "https://");
  String.sub endpoint 8 (len - 8)

type job =
  { id : string;
    status : [`SUBMITTED | `PROGRESSING | `COMPLETE | `CANCELED | `ERROR];
    job_percent_complete : int option;
    current_phase : [`PROBING | `TRANSCODING | `UPLOADING] option;
    error_code : int option;
    error_message : string option;
    user_metadata : (string * string) list }

let job_description res =
  let res = Yojson.Safe.from_string res in
  let open Yojson.Safe.Util in
  let job = member "job" res in
  let id = to_string @@ member "id" job in
  let status =
    match to_string @@ member "status" job with
    | "SUBMITTED"   -> `SUBMITTED
    | "PROGRESSING" -> `PROGRESSING
    | "COMPLETE"    -> `COMPLETE
    | "CANCELED"    -> `CANCELED
    | "ERROR"       -> `ERROR
    | _             -> assert false
  in
  let current_phase =
    match to_string_option @@ member "currentPhase" job with
    | Some "PROBING"     -> Some `PROBING
    | Some "TRANSCODING" -> Some `TRANSCODING
    | Some "UPLOADING"   -> Some `UPLOADING
    | None               -> None
    | _                  -> assert false
  in
  let error_code = to_int_option @@ member "errorCode" job in
  let error_message = to_string_option @@ member "errorMessage" job in
  let user_metadata =
    List.map (fun (k, v) -> (k, to_string v)) @@
    to_assoc @@ member "userMetadata" job
  in
  let job_percent_complete = to_int_option @@ member "jobPercentComplete" job in
  Lwt.return
    {id; status; job_percent_complete; current_phase; user_metadata;
     error_code; error_message}

let create_job ~credentials ~region ~endpoint
    ?client_request_token ?job_template ?queue ~role ~settings ?user_metadata
    ?billing_tags_source ?acceleration_settings ?status_update_interval () =
  let uri = "/2017-08-29/jobs" in
  let add k v l = (k, v) :: l in
  let opt_add k f v l =
    match v with
    | None   -> l
    | Some v -> add k (f v) l
  in
  let opt_string k v l = opt_add k (fun s -> `String s) v l in
  let payload =
    []
    |> opt_string "clientRequestToken" client_request_token
    |> opt_string "jobTemplate" job_template
    |> opt_string "queue" queue
    |> opt_string "role" (Some role)
    |> add "settings" settings
    |> opt_add "userMetadata"
      (fun l -> `Assoc (List.map (fun (k, v) -> (k, `String v)) l))
      user_metadata
    |> opt_add "billingTagsSource"
      (fun s ->
         `String
           (match s with
            | `QUEUE -> "QUEUE"
            | `PRESET -> "PRESET"
            | `JOB_TEMPLATE -> "JOB_TEMPLATE"))
      billing_tags_source
    |> opt_add "accelerationSettings"
      (fun a ->
         `String
           (match a with
            | `DISABLED -> "DISABLED"
            | `ENABLED  -> "ENABLED"))
      acceleration_settings
    |> opt_add "statusUpdateInterval"
      (fun i ->
         `String
           (match i with
            | `SECONDS_10 -> "SECONDS_10"
            | `SECONDS_12 -> "SECONDS_12"
            | `SECONDS_15 -> "SECONDS_15"
            | `SECONDS_20 -> "SECONDS_20"
            | `SECONDS_30 -> "SECONDS_30"
            | `SECONDS_60 -> "SECONDS_60"
            | `SECONDS_120 -> "SECONDS_120"
            | `SECONDS_180 -> "SECONDS_180"
            | `SECONDS_240 -> "SECONDS_240"
            | `SECONDS_300 -> "SECONDS_300"
            | `SECONDS_360 -> "SECONDS_360"
            | `SECONDS_420 -> "SECONDS_420"
            | `SECONDS_480 -> "SECONDS_480"
            | `SECONDS_540 -> "SECONDS_540"
            | `SECONDS_600 -> "SECONDS_600"))
      status_update_interval
  in
  let payload = Yojson.Safe.to_string (`Assoc payload) in
  let%lwt (res, headers) =
    Aws_request.perform
      ~credentials ~service:"mediaconvert" ~region ~meth:`POST
      ~host:(endpoint_host endpoint) ~uri ~payload ()
  in
  job_description res

let get_job ~credentials ~region ~endpoint ~id () =
  let uri = Printf.sprintf "/2017-08-29/jobs/" ^ id in
  let%lwt (res, headers) =
    Aws_request.perform
      ~credentials ~service:"mediaconvert" ~region ~meth:`GET
      ~host:(endpoint_host endpoint) ~uri ()
  in
  job_description res
