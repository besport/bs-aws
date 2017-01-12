
let debug =
  Aws_base.Debug.make "credentials" "Debug credential management." ["all"]

exception Failed

let http_get host uri =
  try%lwt
    let inet_addr = Unix.inet_addr_of_string host in
    let%lwt res =
      Ocsigen_http_client.raw_request ~keep_alive:false
      ~http_method:Ocsigen_http_frame.Http_header.GET
      ~inet_addr ~host ~uri ~content:None
      () ()
    in
    let code =
      match res.Ocsigen_http_frame.frame_header with
      | { Ocsigen_http_frame.Http_header.mode =
            Ocsigen_http_frame.Http_header.Answer code } ->
          code
      | _ ->
          assert false
    in
    if code <> 200 then Lwt.fail Failed else
    match res.Ocsigen_http_frame.frame_content with
      Some content ->
        Lwt.finalize
          (fun () ->
             Ocsigen_stream.string_of_stream 16384 (Ocsigen_stream.get content))
          (fun () ->
             Ocsigen_stream.finalize content `Success)
    | None ->
        Lwt.return ""
  with _ ->
    Lwt.fail Failed

(****)

let parse_credentials credentials creds =
  let creds = Yojson.Safe.from_string creds in
  let open Aws_base.Json in
  let access_key_id = string @@ field "AccessKeyId" creds in
  let secret_access_key = string @@ field "SecretAccessKey" creds in
  let session_token = string @@ field "Token" creds in
  let expiration =
    Aws_base.(from_ISO8601 @@ string @@ field "Expiration" creds) in
  credentials.Aws_common.access_key_id <- access_key_id;
  credentials.Aws_common.secret_access_key <- secret_access_key;
  credentials.Aws_common.session_token <- Some session_token;
  if debug () then begin
    Format.eprintf "id: %s@." access_key_id;
    Format.eprintf "key: %s@." secret_access_key;
    Format.eprintf "token: %s@." session_token;
    Format.eprintf "expiration: %.0f@." expiration
  end;
  expiration

let get_instance_credentials credentials =
  if debug () then Format.eprintf "Reading instance credentials...@.";
  let host = "169.254.169.254" in
  let uri = "/latest/meta-data/iam/security-credentials/" in
  let%lwt role = http_get host uri in
  let%lwt creds = http_get host (uri ^ role) in
  if debug () then
    Format.eprintf "Credentials for %s:@.%s@." role creds;
  Lwt.return (parse_credentials credentials creds)

let get_container_credentials uri credentials =
  if debug () then Format.eprintf "Reading container credentials...@.";
  let host = "169.254.170.2" in
  let%lwt creds = http_get host uri in
  if debug () then Format.eprintf "Got credentials:@.%s@." creds;
  Lwt.return (parse_credentials credentials creds)

let refreshable_credentials get credentials =
  let rec refresh expiration =
    let t = Unix.gettimeofday () in
    let delta = t -. expiration in
    let delay =
      if delta > 6000. then 5400.
      else if delta > 300. then delta -. 270.
      else 1.
    in
    if debug () then
      Format.eprintf
        "Credentials expiring in %.0f seconds; waiting %.0f seconds@."
        delta delay;
    Lwt.try_bind
      (fun () -> get credentials)
      (fun expiration -> refresh expiration)
      (fun _ -> refresh expiration)
  in
  let%lwt expiration = get credentials in
  Lwt.async (fun () -> refresh expiration);
  Lwt.return ()

let default =
  lazy
    (let credentials =
       Aws_common.credentials ~access_key_id:"" ~secret_access_key:"" () in
     let%lwt () =
       match Sys.getenv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" with
       | uri ->
           refreshable_credentials (get_container_credentials uri) credentials
       | exception Not_found ->
           refreshable_credentials get_instance_credentials credentials
     in
     Lwt.return credentials)

let get_defaults () = Lazy.force default
