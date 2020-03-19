module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  type message_attribute_value =
    [`Binary of string | `Number of string | `String of string]

  val sms_attributes
    :  ?sender_id:string
    -> ?max_price:float
    -> ?sms_type:[`Promotional | `Transactional]
    -> unit
    -> (string * message_attribute_value) list

  val publish
    :  topic:[ `Phone_number of string
             | `Target_arn of string
             | `Topic_arn of string ]
    -> message:string
    -> ?message_attributes:(string * message_attribute_value) list
    -> unit
    -> unit Lwt.t

  val subscribe
    :  endpoint:string
    -> protocol:[ `application
                | `email
                | `email_json
                | `http
                | `https
                | `lambda
                | `sms
                | `sqs ]
    -> topic_arn:string
    -> ?attributes:[ `Delivery_policy of Yojson.Safe.t
                   | `Raw_message_delivery of bool ]
                   list
    -> unit
    -> string Lwt.t

  val unsubscribe : subscription_arn:string -> unit -> unit Lwt.t

  val set_subscription_attributes
    :  attribute:[ `Delivery_policy of Yojson.Safe.t
                 | `Raw_message_delivery of bool ]
    -> subscription_arn:string
    -> unit
    -> unit Lwt.t
end
