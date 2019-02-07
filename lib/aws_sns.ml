
let sns_endpoint region =
  Printf.sprintf "sns.%s.amazonaws.com" (Aws_common.Region.to_string region)

let init_params act k v = ["Version", "2010-03-31"; "Action", act; k, v]

let decode_response res =
  let i = Xmlm.make_input ~strip:true (`String (0, res)) in
  ignore (Xmlm.input i); (* DTD *)
  ignore (Xmlm.input i); (* *Response *)
  ignore (Xmlm.input i); (* *Result *)
  i

type message_attribute_value =
  [ `Binary of string | `Number of string | `String of string ]

let string prefix s rem = (prefix, s) :: rem

let list fmt prefix l rem =
  snd @@
  List.fold_left
    (fun (i, rem) x ->
       (i + 1, fmt (prefix ^ ".entry." ^ string_of_int i) x rem))
    (1, rem) l

let entry prefix (k, v) rem =
  string (prefix ^ ".Name") k @@
  let prefix = prefix ^ ".Value" in
  match v with
  | `String s ->
      string (prefix ^ ".DataType") "String" @@
      string (prefix ^ ".StringValue") s @@
      rem
  | `Number s ->
      string (prefix ^ ".DataType") "Number" @@
      string (prefix ^ ".StringValue") s @@
      rem
  | `Binary s ->
      string (prefix ^ ".DataType") "Binary" @@
      string (prefix ^ ".BinaryValue") (Base64.encode_string s) @@
      rem

let sms_attributes ?sender_id ?max_price ?sms_type () =
  List.map (fun (k, v) -> (k, `String v)) @@
  let open Aws_base.Param in
  string "AWS.SNS.SMS.SenderID" sender_id @@
  custom "AWS.SNS.SMS.MaxPrice" string_of_float max_price @@
  custom "AWS.SNS.SMS.SMSType"
    (fun t ->
       match t with
       | `Promotional -> "Promotional"
       | `Transactional -> "Transactional")
    sms_type @@
  []

let publish ~credentials ~region ~topic ~message ?(message_attributes = []) () =
  let query =
    list entry "MessageAttributes" message_attributes @@
    (match topic with
     | `Target_arn t -> ("TargetArn", t)
     | `Topic_arn t -> ("TopicArn", t)
     | `Phone_number p -> ("PhoneNumber", p)) ::
    init_params "Publish" "Message" message
  in
  let%lwt _ =
    Aws_request.perform ~credentials ~service:"sns" ~region ~meth:`POST
      ~host:(sns_endpoint region) ~uri:"/" ~query () in
  Lwt.return ()
  (*TODO: parse XML response*)

let subscribe ~credentials ~region ~endpoint ~protocol ~topic_arn () =
  let query =
    ("Endpoint", endpoint) ::
    ("Protocol",
     match protocol with
     | `http -> "http"
     | `https -> "https"
     | `email -> "email"
     | `email_json -> "email-json"
     | `sms -> "sms"
     | `sqs -> "sqs"
     | `application -> "application"
     | `lambda -> "lambda") ::
    init_params "Subscribe" "TopicArn" topic_arn
  in
  let%lwt res =
    Aws_request.perform ~credentials ~service:"sns" ~region ~meth:`POST
      ~host:(sns_endpoint region) ~uri:"/" ~query () in
  let i = decode_response res in
  ignore (Xmlm.input i); (* SubscriptionArn *)
  match Xmlm.input i with
    `Data s -> Lwt.return s
  | _       -> assert false

let unsubscribe ~credentials ~region ~subscription_arn () =
  let query =
    init_params "Unsubscribe" "SubscriptionArn" subscription_arn
  in
  let%lwt res =
    Aws_request.perform ~credentials ~service:"sns" ~region ~meth:`POST
      ~host:(sns_endpoint region) ~uri:"/" ~query () in
  Lwt.return ()

let set_subscription_attributes ~credentials ~region
    ~attribute ~subscription_arn () =
  let query =
    ("AttributeName",
     match attribute with
       `Delivery_policy _      -> "DeliveryPolicy"
     | `Raw_message_delivery _ -> "RawMessageDelivery") ::
    ("AttributeValue",
     match attribute with
       `Delivery_policy p      -> Yojson.Safe.to_string p
     | `Raw_message_delivery b -> string_of_bool b) ::
    init_params "SetSubscriptionAttributes" "SubscriptionArn" subscription_arn
  in
  let%lwt _ =
    Aws_request.perform ~credentials ~service:"sns" ~region ~meth:`POST
      ~host:(sns_endpoint region) ~uri:"/" ~query () in
  Lwt.return ()

(* XXX DEPRECATED: *)

module type SETTINGS = sig
  val credentials : Aws_common.credentials
  val region : Aws_common.Region.t
  val secure : bool
end

module Make(Settings : SETTINGS) = struct
  let publish ~target message =
    let open Settings in
    let topic =
      match target with
      | `TargetArn t   -> `Target_arn t
      | `TopicArn t    -> `Topic_arn t
      | `PhoneNumber p -> `Phone_number p
    in
    publish ~credentials ~region ~topic ~message ()
end
