(*
XXX Non-ascii characters in JSON policy should be escaped...
*)

let debug = Base.Debug.make "s3" "Debug S3 API." ["all"]

type bucket = { region : Common.Region.t; name : string }

let bucket region name = { region; name }

let bucket_url ?(secure=true) region bucket =
  let region = Common.Region.to_string region in
  if secure then
    Format.sprintf "https://%s.s3-%s.amazonaws.com/" bucket.name region
  else
    Format.sprintf "http://%s.s3-%s.amazonaws.com/" bucket.name region

let hostname region =
  Format.sprintf "s3-%s.amazonaws.com" (Common.Region.to_string region)

let bucket_host region bucket = Format.sprintf "%s.%s" bucket (hostname region)

(****)

let exact_match field value = `Assoc [field, `String value]
let starts_with field prefix =
  `List [`String "starts-with"; `String ("$" ^field); `String prefix]
let condition field cond =
  match cond with
    `Eq value      -> exact_match field value
  | `Prefix prefix -> starts_with field prefix

let form ?secure ~credentials ~region ~bucket
    ~expiration ~key
    ?content_length_range ?success_action_redirect ?success_action_status
    ?(other_fields = []) () =
  let date = Unix.gettimeofday () in
  let expiration =
    match expiration with
      `Delay d -> date +. d
    | `Date d -> d
  in
  let date = Base.to_ISO8601 date in
  let skey = Signature.signing_key credentials date bucket.region "s3" in
  let cond c = match c with `Eq _ as c -> c | `Prefix (p, _) -> `Prefix p in
  let value c = match c with `Eq v -> v | `Prefix (_, v) -> v in
  let conditions =
    [exact_match "bucket" bucket.name;
     exact_match "x-amz-date" date;
     exact_match "x-amz-algorithm" "AWS4-HMAC-SHA256";
     exact_match "x-amz-credential" (Signature.credential skey);
     condition "key" (cond key)]
  in
  let conditions =
    match credentials.Common.session_token with
    | Some token -> exact_match "x-amz-security-token" token :: conditions
    | None       -> conditions
  in
  let conditions =
    match success_action_redirect with
      None     -> conditions
    | Some url -> condition "success_action_redirect" (cond url) :: conditions
  in
  let conditions =
    match success_action_status with
      None -> conditions
    | Some status -> exact_match "success_action_status" status :: conditions
  in
  let conditions =
    match content_length_range with
      Some (min, max) ->
        `List [`String "content-length-range";
               `String (string_of_int min); `String (string_of_int max)] ::
        conditions
    | None ->
        conditions
  in
  let conditions =
    List.fold_left
      (fun rem (f, v) -> condition f v :: rem) conditions other_fields in
  let policy =
    `Assoc ["expiration",
            `String (Base.to_ISO8601 ~extended:true expiration);
            "conditions", `List conditions]
    |> Yojson.Safe.to_string
  in
  if debug () then Format.eprintf "Policy: %s@." policy;
  let policy = Base64.encode_string policy in
  let fields =
    ["policy", policy;
     "x-amz-algorithm", "AWS4-HMAC-SHA256";
     "x-amz-credential", Signature.credential skey;
     "x-amz-date", date;
     "x-amz-signature", Signature.sign skey policy;
     "key", value key]
  in
  let fields =
    match credentials.Common.session_token with
    | Some token -> ("x-amz-security-token", token) :: fields
    | None       -> fields
  in
  let fields =
    match success_action_redirect with
      None     -> fields
    | Some url -> ("success_action_redirect", value url) :: fields
  in
  let fields =
    match success_action_status with
      None        -> fields
    | Some status -> ("success_action_status", status) :: fields
  in
  (bucket_url ?secure region bucket, fields)

let object_url ?secure ~credentials ~region ~bucket ~expiration ~key
    ?response_content_type ?response_content_language ?response_expires
    ?response_cache_control ?response_content_disposition
    ?response_content_encoding () =
  let host = bucket_host region bucket in
  let uri = "/" ^ key in
  let query =
    let add k v r = match v with Some v -> (k, v) :: r | None -> r in
    add "response-content-type" response_content_type @@
    add "response-content-language" response_content_language @@
    add "response-expires" response_expires @@
    add "response-cache-control" response_cache_control @@
    add "response-content-disposition" response_content_disposition @@
    add "response-content-encoding" response_content_encoding @@
    []
  in
  let {Base.uri; query } =
    Base.request ~meth:`GET ?secure ~host ~uri ~query ()
    |> Signature.sign_request_using_query_parameters
      credentials ~service:"s3" region ~expiration ~unsigned_payload:true
  in
  Uri.make ~scheme:"https" ~host ~path:uri
    ~query:(List.map (fun (k, v) -> (k, [v])) query) ()

let hash str = ("x-amz-content-sha256", Signature.hash str)

let request
    ~credentials ~region ?secure ~meth ?(host = hostname region) ~uri
    ?query ?(headers = []) ?(payload = "") ?(hash = hash payload) () =
  let%lwt (res, _) =
    Request.perform
      ~credentials ~service:"s3" ~region ?secure ~meth ~host ~uri
      ?query ~headers:(hash :: headers) ~payload ()
  in
  Lwt.return res


let list ~credentials ~region () =
  request ~credentials ~region ~meth:`GET ~uri:"/" ()

module Bucket = struct
  type contents = { key : string }
  type list_result =
    { is_truncated : bool;
      contents : contents list }

  let list ~credentials ~region ?prefix bucket =
    let query =
      let open Base.Param in
      [("list-type", "2")]
      |> string "prefix" prefix
    in
    let%lwt res =
      request ~credentials ~region ~meth:`GET ~query ~uri:("/" ^ bucket) () in
    let open Ezxmlm in
    let (_, d) = from_string res in
    let r = member "ListBucketResult" d in
    let is_truncated = data_to_string @@ member "IsTruncated" r = "true" in
    let contents =
      List.map (fun c -> {key = data_to_string @@ member "Key" c})
        (members "Contents" r)
    in
    Lwt.return {is_truncated; contents}
end

module Object = struct
  let delete ~credentials ~region ~bucket object_name =
    let%lwt _ =
      request ~credentials ~region ~meth:`DELETE
        ~host:(bucket_host region bucket)
        ~uri:("/" ^ object_name) ()
    in
    Lwt.return ()

  let put ~credentials ~region ~bucket object_name data =
    let%lwt _ =
        request ~credentials ~region ~meth:`PUT
          ~host:(bucket_host region bucket)
          ~uri:("/" ^ object_name) ~payload:data ()
    in
    Lwt.return ()

  let copy ~credentials ~region ~src_bucket src_object ~dst_bucket dst_object  =
    let%lwt _ =
      request ~credentials ~region ~meth:`PUT
        ~host:(bucket_host region dst_bucket)
        ~headers:[("x-amz-copy-source", "/" ^ src_bucket ^ "/" ^ src_object)]
        ~uri:("/" ^ dst_object) ()
    in
    Lwt.return ()

  let head ~credentials ~region ~bucket key =
    request ~credentials ~region ~meth:`HEAD
    ~uri:("/" ^ bucket ^ "/" ^ key)
    ()

  let get ~credentials ~region ~bucket key =
    request ~credentials ~region ~meth:`GET
    ~uri:("/" ^ bucket ^ "/" ^ key)
    ()
end
