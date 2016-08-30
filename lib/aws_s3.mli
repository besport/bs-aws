
type bucket

val bucket : Aws_common.Region.t -> string -> bucket

val form :
  ?secure:bool -> credentials:Aws_common.credentials -> bucket:bucket ->
  expiration:[`Date of float | `Delay of float] ->
  key:[`Eq of string | `Prefix of string * string] ->
  ?content_length_range:int * int ->
  ?success_action_redirect:
     [`Eq of string | `Prefix of string * string] ->
  ?success_action_status:string ->
  ?other_fields:(string * [`Eq of string | `Prefix of string ]) list ->
  unit ->
  string * (string * string) list

val list :
  credentials:Aws_common.credentials ->
  region:Aws_common.Region.t ->
  unit ->
  string Lwt.t

module Bucket : sig
  val list :
    credentials:Aws_common.credentials ->
    region:Aws_common.Region.t ->
    string ->
    string Lwt.t
end

module Object : sig
  val delete :
    credentials:Aws_common.credentials ->
    region:Aws_common.Region.t ->
    bucket:string ->
    string ->
    string Lwt.t
  val put :
    credentials:Aws_common.credentials ->
    region:Aws_common.Region.t ->
    bucket:string ->
    string ->
    string ->
    string Lwt.t
  val copy :
    credentials:Aws_common.credentials ->
    region:Aws_common.Region.t ->
    src_bucket:string ->
    string ->
    dst_bucket:string ->
    string ->
    string Lwt.t
  val get :
    credentials:Aws_common.credentials ->
    region:Aws_common.Region.t ->
    bucket:string ->
    string ->
    string Lwt.t
  val head :
    credentials:Aws_common.credentials ->
    region:Aws_common.Region.t ->
    bucket:string ->
    string ->
    string Lwt.t
end
