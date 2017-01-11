
let endpoint region =
  Printf.sprintf "sns.%s.amazonaws.com" (Aws_common.Region.to_string region)

let publish ~credentials ~region ~topic ~message () =
  let topic =
    match topic with
    | `Target_arn t -> ("TargetArn", t)
    | `Topic_arn t -> ("TopicArn", t)
    | `Phone_number p -> ("PhoneNumber", p)
  in
  let%lwt _ =
    Aws_request.perform ~credentials ~service:"sns" ~region
      ~meth:`POST ~host:(endpoint region) ~uri:"/"
      ~query:[("Action", "Publish"); topic; ("Message", message)] ()
  in
  Lwt.return ()
  (*TODO: parse XML response*)

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
