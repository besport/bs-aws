
let sns_endpoint region =
  Printf.sprintf "sns.%s.amazonaws.com" (Aws_common.Region.to_string region)

let init_params act k v = ["Version", "2010-03-31"; "Action", act; k, v]

let decode_response res =
  let i = Xmlm.make_input ~strip:true (`String (0, res)) in
  ignore (Xmlm.input i); (* DTD *)
  ignore (Xmlm.input i); (* *Response *)
  ignore (Xmlm.input i); (* *Result *)
  i

let publish ~credentials ~region ~topic ~message () =
  let query =
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
