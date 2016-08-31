(*
XXX Non-ascii characters in JSON policy should be escaped...
*)

let (>>) f g = Lwt.bind f (fun _ -> g)

let debug = Aws_base.Debug.make "s3" "Debug S3 API." ["all"]

type bucket = { region : Aws_common.Region.t; name : string }

let bucket region name = { region; name }

let bucket_url ?(secure=true) bucket =
  if secure then
    Format.sprintf "https://%s.s3.amazonaws.com/" bucket.name
  else
    Format.sprintf "http://%s.s3.amazonaws.com/" bucket.name

(****)

let exact_match field value = `Assoc [field, `String value]
let starts_with field prefix =
  `List [`String "starts-with"; `String ("$" ^field); `String prefix]
let condition field cond =
  match cond with
    `Eq value      -> exact_match field value
  | `Prefix prefix -> starts_with field prefix

let form ?secure ~credentials ~bucket
    ~expiration ~key
    ?content_length_range ?success_action_redirect ?success_action_status
    ?(other_fields = []) () =
  let date = Unix.gettimeofday () in
  let expiration =
    match expiration with
      `Delay d -> date +. d
    | `Date d -> d
  in
  let date = Aws_base.to_ISO8601 date in
  let skey = Aws_signature.signing_key credentials date bucket.region "s3" in
  let cond c = match c with `Eq _ as c -> c | `Prefix (p, _) -> `Prefix p in
  let value c = match c with `Eq v -> v | `Prefix (_, v) -> v in
  let conditions =
    [exact_match "bucket" bucket.name;
     exact_match "x-amz-date" date;
     exact_match "x-amz-algorithm" "AWS4-HMAC-SHA256";
     exact_match "x-amz-credential" (Aws_signature.credential skey);
     condition "key" (cond key)]
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
            `String (Aws_base.to_ISO8601 ~extended:true expiration);
            "conditions", `List conditions]
    |> Yojson.Safe.to_string
  in
  if debug () then Format.eprintf "Policy: %s@." policy;
  let policy = B64.encode policy in
  let fields =
    ["policy", policy;
     "x-amz-algorithm", "AWS4-HMAC-SHA256";
     "x-amz-credential", Aws_signature.credential skey;
     "x-amz-date", date;
     "x-amz-signature", Aws_signature.sign skey policy;
     "key", value key]
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
  (bucket_url ?secure bucket, fields)

let hash str = ("x-amz-content-sha256", Aws_signature.hash str)
let hostname region =
  Format.sprintf "s3-%s.amazonaws.com" (Aws_common.Region.to_string region)
let bucket_host region bucket = Format.sprintf "%s.%s" bucket (hostname region)

let request
    ~credentials ~region ?secure ~meth ?(host = hostname region) ~uri
    ?query ?(headers = []) ?(payload = "") ?(hash = hash payload) () =
  Aws_request.perform
    ~credentials ~service:"s3" ~region ?secure ~meth ~host ~uri
    ?query ~headers:(hash :: headers) ~payload ()


let list ~credentials ~region () =
  request ~credentials ~region ~meth:`GET ~uri:"/" ()

module Bucket = struct
  let list ~credentials ~region bucket =
    request ~credentials ~region ~meth:`GET
    (*~headers:[("list-type", "2")]*) (*does not work with it*)
    ~uri:("/" ^ bucket)
    ()
end

module Object = struct
  let delete ~credentials ~region ~bucket object_name = Lwt.catch
    (fun () -> request ~credentials ~region ~meth:`DELETE
                 ~host:(bucket_host region bucket)
                 ~uri:("/" ^ object_name) () >>
               Lwt.return ())
    (fun e -> match e with
       | Aws_common.Error e when e.Aws_common.code = 204 -> Lwt.return ()
       | e -> Lwt.fail e
    )

  let put ~credentials ~region ~bucket object_name data =
    request ~credentials ~region ~meth:`PUT
      ~host:(bucket_host region bucket)
      ~uri:("/" ^ object_name) ~payload:data () >>
    Lwt.return ()

  let copy ~credentials ~region ~src_bucket src_object ~dst_bucket dst_object  =
    request ~credentials ~region ~meth:`PUT
      ~host:(bucket_host region dst_bucket)
      ~headers:[("x-amz-copy-source", "/" ^ src_bucket ^ "/" ^ src_object)]
      ~uri:("/" ^ dst_object) () >>
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
