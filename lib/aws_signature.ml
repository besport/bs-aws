
let debug = Aws_base.Debug.make "signature" "Debug signing process." ["all"]

let hash s =
  if s = "" then
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  else
    Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s
    |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())

type key = {key : string; credential_scope : string; credential : string }

let signing_key credentials date region service =
  let hash v k =
    Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 k) v
  in
  let key =
    "AWS4" ^ credentials.Aws_common.secret_access_key
    |> hash (String.sub date 0 8)
    |> hash (Aws_common.Region.to_string region)
    |> hash service |> hash "aws4_request"
  in
  let credential_scope =
    String.concat "/"
      [String.sub date 0 8; Aws_common.Region.to_string region;
       service; "aws4_request"]
  in
  let credential =
    credentials.Aws_common.access_key_id ^ "/" ^ credential_scope in
  {key; credential_scope; credential}

let credential key = key.credential

let sign key sts =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key.key) sts
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())

(****)

let canonical_request
      meth uri query headers ?(unsigned_payload = false) payload =
  let args =
    List.map
      (fun (k, v) -> (k, Aws_base.url_encode v)) query
    |> List.sort
         (fun (k1, v1) (k2, v2) ->
            let c = compare k1 k2 in if c <> 0 then c else compare v1 v2)
    |> List.map (fun (k, v) -> k ^ "=" ^ v)
    |> String.concat "&"
  in
  let headers =
    List.sort (fun (k1, _) (k2, _) -> compare k1 k2) headers in
  let signed_headers = String.concat ";" (List.map fst headers) in
  let headers = List.map (fun (k, v) -> k ^ ":" ^ v) headers in
  let payload_hash =
    if unsigned_payload then "UNSIGNED-PAYLOAD" else hash payload in
  (String.concat "\n"
     (Aws_base.string_of_meth meth :: uri :: args ::
      headers @ [""; signed_headers; payload_hash]),
   signed_headers)

let string_to_sign date key canonical_request =
  String.concat "\n"
    ["AWS4-HMAC-SHA256"; date; key.credential_scope; hash canonical_request]

let authorization_header key signed_headers signature =
  Printf.sprintf
    "AWS4-HMAC-SHA256 Credential=%s, SignedHeaders=%s, Signature=%s"
    key.credential signed_headers signature

let sign_request credentials ~service region req =
  let {Aws_base.meth; uri; query; headers; payload} = req in
  let date = Aws_base.to_ISO8601 (Unix.gettimeofday ()) in
  let headers = ("x-amz-date", date) :: headers in
  let headers =
    match credentials.Aws_common.session_token with
    | Some token -> ("x-amz-security-token", token) :: headers
    | None       -> headers
  in
  let (creq, signed_headers) =
    canonical_request meth uri query headers payload in
  if debug () then Format.eprintf "Canonical request:@.%s@." creq;
  let key = signing_key credentials date region service in
  let sts = string_to_sign date key creq in
  if debug () then Format.eprintf "String to sign:%s@." sts;
  let signature = sign key sts in
  let authz = authorization_header key signed_headers signature in
  {req with Aws_base.headers = ("authorization", authz) :: headers}

let sign_request_using_query_parameters
    credentials ~service region ~expiration ?unsigned_payload req =
  let {Aws_base.meth; uri; query; headers; payload} = req in
  let date = Aws_base.to_ISO8601 (Unix.gettimeofday ()) in
  let key = signing_key credentials date region service in
  let (_, signed_headers) =
    canonical_request meth uri query headers ?unsigned_payload payload in
  let query =
    ("X-Amz-Algorithm", "AWS4-HMAC-SHA256") ::
    ("X-Amz-Credential", key.credential) ::
    ("X-Amz-Date", date) ::
    ("X-Amz-Expires", string_of_int expiration) ::
    ("X-Amz-SignedHeaders", signed_headers) ::
    query
  in
  let query =
    match credentials.Aws_common.session_token with
    | Some token -> ("X-Amz-Security-Token", token) :: query
    | None       -> query
  in
  let (creq, signed_headers) =
    canonical_request meth uri query headers ?unsigned_payload payload in
  if debug () then Format.eprintf "Canonical request:@.%s@." creq;
  let sts = string_to_sign date key creq in
  if debug () then Format.eprintf "String to sign:%s@." sts;
  let signature = sign key sts in
  let query = ("X-Amz-Signature", signature) :: query in
  {req with Aws_base.query}
