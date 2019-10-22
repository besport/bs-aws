
val describe_endpoints :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  ?next_token:string -> ?max_results:int -> ?mode:[`DEFAULT | `GET_ONLY] ->
  unit ->
  ((*endpoints:*)string list * (*next_token:*)string option) Lwt.t

type job =
  { id : string;
    status : [`SUBMITTED | `PROGRESSING | `COMPLETE | `CANCELED | `ERROR];
    job_percent_complete : int option;
    current_phase : [`PROBING | `TRANSCODING | `UPLOADING] option;
    error_code : int option;
    error_message : string option;
    user_metadata : (string * string) list }

val create_job :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  endpoint:string ->
  ?client_request_token:string ->
  ?job_template:string ->
  ?queue:string ->
  role:string ->
  settings:Yojson.Safe.t ->
  ?user_metadata:(string * string) list ->
  ?billing_tags_source:[ `JOB_TEMPLATE | `PRESET | `QUEUE ] ->
  ?acceleration_settings:[ `DISABLED | `ENABLED ] ->
  ?status_update_interval:[ `SECONDS_10
                          | `SECONDS_12
                          | `SECONDS_15
                          | `SECONDS_20
                          | `SECONDS_30
                          | `SECONDS_60
                          | `SECONDS_120
                          | `SECONDS_180
                          | `SECONDS_240
                          | `SECONDS_300
                          | `SECONDS_360
                          | `SECONDS_420
                          | `SECONDS_480
                          | `SECONDS_540
                          | `SECONDS_600 ] ->
  unit ->
  job Lwt.t

val get_job :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  endpoint:string -> id:string ->
  unit ->
  job Lwt.t