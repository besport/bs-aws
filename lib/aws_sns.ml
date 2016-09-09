module Make(Settings : Aws_common.SETTINGS) = struct

  let (>>) f g = Lwt.bind f (fun _ -> g)

  let debug = Aws_base.Debug.make "sns" "Debug SNS API." ["all"]


  let request ~meth query =
    let open Settings in
    let host = Format.sprintf "sns.%s.amazonaws.com"
                 (Aws_common.Region.to_string region) in
    let uri = "/" in
    Aws_request.perform ~credentials ~service:"sns" ~region
                        ~secure:Settings.secure ~meth ~host ~uri ~query ()

  let publish ~target message =
    (*TODO: check that message has at most 262144 bytes (not characters) *)
    let target = match target with
    | `TargetArn t -> ("TargetArn", t)
    | `TopicArn t -> ("TopicArn", t)
    | `PhoneNumber p -> ("PhoneNumber", p)
    in
    request ~meth:`POST [("Action", "Publish"); target; ("Message", message)] >>
    Lwt.return ()
    (*TODO: parse XML response*)
end
