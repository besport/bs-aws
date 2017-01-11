
val publish :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  topic:[ `Phone_number of string
        | `Target_arn of string
        | `Topic_arn of string ] ->
  message:string -> unit -> unit Lwt.t

(* XXX DEPRECATED: *)

module type SETTINGS = sig
  val credentials : Aws_common.credentials
  val region : Aws_common.Region.t
  val secure : bool
end

module Make(Settings : SETTINGS) : sig
  val publish :
    target:[ `PhoneNumber of string
           | `TargetArn of string
           | `TopicArn of string ] ->
    string -> unit Lwt.t
end
