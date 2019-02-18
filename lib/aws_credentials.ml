
let debug =
  Aws_base.Debug.make "credentials" "Debug credential management." ["all"]

exception Failed

let http_get host path =
  try%lwt
    let%lwt response, body =
      Cohttp_lwt_unix.Client.call
        `GET (Uri.make ~scheme:"http" ~path ~host ()) in
    match Cohttp.Response.status response with
    | `OK -> Cohttp_lwt.Body.to_string body
    | _   -> Lwt.fail Failed
  with _ ->
    Lwt.fail Failed

(****)

let parse_credentials credentials creds =
  let creds = Yojson.Safe.from_string creds in
  let open Yojson.Safe.Util in
  let access_key_id = to_string @@ member "AccessKeyId" creds in
  let secret_access_key = to_string @@ member "SecretAccessKey" creds in
  let session_token = to_string @@ member "Token" creds in
  let expiration =
    Aws_base.(from_ISO8601 @@ to_string @@ member "Expiration" creds) in
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
    let delta = expiration -. t  in
    let delay =
      if delta > 6000. then 5400.
      else if delta > 300. then delta -. 270.
      else 1.
    in
    if debug () then
      Format.eprintf
        "Credentials expiring in %.0f seconds; waiting %.0f seconds@."
        delta delay;
    let%lwt () = Lwt_unix.sleep delay in
    Lwt.try_bind
      (fun () -> get credentials)
      (fun expiration -> refresh expiration)
      (fun _ -> refresh expiration)
  in
  let%lwt expiration = get credentials in
  Lwt.async (fun () -> refresh expiration);
  Lwt.return ()

let get_refreshable_credentials () =
  let credentials =
    Aws_common.credentials ~access_key_id:"" ~secret_access_key:"" () in
  let%lwt () =
    match Sys.getenv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" with
    | uri ->
      refreshable_credentials (get_container_credentials uri) credentials
    | exception Not_found ->
      refreshable_credentials get_instance_credentials credentials
  in
  Lwt.return credentials

let get_env v =
  try
    Some (Sys.getenv v)
  with Not_found ->
    None

let get_env_credentials () =
  match get_env "AWS_ACCESS_KEY_ID", get_env "AWS_SECRET_ACCESS_KEY" with
  | Some access_key_id, Some secret_access_key ->
    let session_token = get_env "AWS_SECURITY_TOKEN" in
    Some
      (Aws_common.credentials ()
         ~access_key_id ~secret_access_key ?session_token)
  | _, _ ->
    None

let default =
  lazy
    (match get_env_credentials () with
     | Some credentials ->
       Lwt.return credentials
     | None ->
       get_refreshable_credentials ())

let get_defaults () = Lazy.force default
