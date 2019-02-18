
type message_attribute_value =
  [ `Binary of string | `Number of string | `String of string ]

val sms_attributes :
  ?sender_id:string -> ?max_price:float ->
  ?sms_type:[`Promotional | `Transactional] ->
  unit ->
  (string * message_attribute_value) list

val publish :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  topic:[ `Phone_number of string
        | `Target_arn of string
        | `Topic_arn of string ] ->
  message:string ->
  ?message_attributes:(string * message_attribute_value) list ->
  unit -> unit Lwt.t

val subscribe :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  endpoint:string ->
  protocol:[ `application | `email | `email_json |
             `http | `https | `lambda | `sms | `sqs ] ->
  topic_arn:string ->
  unit -> string Lwt.t

val unsubscribe :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  subscription_arn:string ->
  unit -> unit Lwt.t

val set_subscription_attributes :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  attribute:[ `Delivery_policy of Yojson.Safe.t
            | `Raw_message_delivery of bool ] ->
  subscription_arn:string -> unit ->
  unit Lwt.t

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
