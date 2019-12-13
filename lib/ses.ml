module Make (Conf : Service.CONF) = struct

  let endpoint region =
    Printf.sprintf "email.%s.amazonaws.com" (Common.Region.to_string region)

  module Service = Service.Make (Conf)
    (struct let name = "ses" and host = endpoint Conf.region end)

  type content = { charset : string option; data : string }

  let content ?charset data = { charset; data }

  type body = { html : content option; text : content option }

  let body ?html ?text () = { html; text }

  type message = { subject : content; body : body }

  type destination =
    { bcc_addresses : string list;
      cc_addresses : string list;
      to_addresses : string list }

  let dest ?(bcc = []) ?(cc = []) ?(to_ = []) () =
    { bcc_addresses = bcc; cc_addresses = cc; to_addresses = to_ }

  let maybe f x rem = match x with Some x -> f x rem | None -> rem

  let string prefix s rem = (prefix, s) :: rem

  let list fmt prefix l rem =
    snd @@
    List.fold_left
      (fun (i, rem) x ->
         (i + 1, fmt (prefix ^ ".member." ^ string_of_int i) x rem))
      (1, rem) l

  let add_content prefix content rem =
    string (prefix ^ ".Data") content.data @@
    Base.Param.string (prefix ^ ".Charset") content.charset @@
    rem

  let add_body prefix body rem =
    maybe (add_content (prefix ^ ".Html")) body.html @@
    maybe (add_content (prefix ^ ".Text")) body.text @@
    rem

  let add_message prefix message rem =
    add_body (prefix ^ ".Body") message.body @@
    add_content (prefix ^ ".Subject") message.subject @@
    rem

  let add_destination prefix dest rem =
    list string (prefix ^ ".BccAddresses") dest.bcc_addresses @@
    list string (prefix ^ ".CcAddresses") dest.cc_addresses @@
    list string (prefix ^ ".ToAddresses") dest.to_addresses @@
    rem

  let message_tag prefix (name, value) rem =
    string (prefix ^ ".Name") name @@
    string (prefix ^ ".Value") value @@
    rem

  let action act = ["Version", "2010-12-01"; "Action", act]

  let send_email ?configuration_set_name ~destination
      ~message ?(reply_to_addresses = []) ?return_path ?return_path_arn
      ~source ?source_arn ?(tags = []) () =
    let parameters =
      action "SendEmail"
      |> Base.Param.string "ConfigurationSetName" configuration_set_name
      |> add_destination "Destination" destination
      |> add_message "Message" message
      |> list string "ReplyToAddresses" reply_to_addresses
      |> Base.Param.string "ReturnPath" return_path
      |> Base.Param.string "ReturnPathArn" return_path_arn
      |> string "Source" source
      |> Base.Param.string "SourceArn" source_arn
      |> list message_tag "Tags" tags
    in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query:parameters () in
    Lwt.return_unit

end
