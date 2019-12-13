module Make (Conf : Service.CONF) = struct

  let sns_endpoint region =
    Printf.sprintf "sns.%s.amazonaws.com" (Common.Region.to_string region)

  module Service = Service.Make (Conf)
    (struct let name = "sns" and host = sns_endpoint Conf.region end)

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
    let open Base.Param in
    string "AWS.SNS.SMS.SenderID" sender_id @@
    custom "AWS.SNS.SMS.MaxPrice" string_of_float max_price @@
    custom "AWS.SNS.SMS.SMSType"
      (fun t ->
         match t with
         | `Promotional -> "Promotional"
         | `Transactional -> "Transactional")
      sms_type @@
    []

  let publish ~topic ~message ?(message_attributes = []) () =
    let query =
      list entry "MessageAttributes" message_attributes @@
      (match topic with
       | `Target_arn t -> ("TargetArn", t)
       | `Topic_arn t -> ("TopicArn", t)
       | `Phone_number p -> ("PhoneNumber", p)) ::
      init_params "Publish" "Message" message
    in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query () in
    Lwt.return_unit
    (*TODO: parse XML response*)

  let subscribe
        ~endpoint ~protocol ~topic_arn ?(attributes = []) () =
    let query =
      let attribute prefix attr rem =
        (prefix ^ ".key",
         match attr with
           `Delivery_policy _      -> "DeliveryPolicy"
         | `Raw_message_delivery _ -> "RawMessageDelivery") ::
          (prefix ^ ".value",
           match attr with
             `Delivery_policy p      -> Yojson.Safe.to_string p
           | `Raw_message_delivery b -> string_of_bool b) ::
            rem
      in
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
      (list attribute "Attributes" attributes @@
      init_params "Subscribe" "TopicArn" topic_arn)
    in
    let%lwt (res, _) = Service.request ~meth:`POST ~uri:"/" ~query () in
    let i = decode_response res in
    ignore (Xmlm.input i); (* SubscriptionArn *)
    match Xmlm.input i with
      `Data s -> Lwt.return s
    | _       -> assert false

  let unsubscribe ~subscription_arn () =
    let query =
      init_params "Unsubscribe" "SubscriptionArn" subscription_arn
    in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query () in
    Lwt.return_unit

  let set_subscription_attributes
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
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query () in
    Lwt.return_unit

end
