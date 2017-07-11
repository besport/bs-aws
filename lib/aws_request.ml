let debug = Aws_base.Debug.make "request" "Debug HTTP requests." ["all"]

let (>>=) = Lwt.bind

let encode_post_query req =
  match req with
  | {Aws_base.meth = `POST;
     query = (_ :: _) as query; payload = ""; headers} ->
      {req with
       Aws_base.headers =
         ("content-type", "application/x-www-form-urlencoded") :: headers;
       query = [];
       payload =
         String.concat "&"
           (List.map
              (fun (f, v) ->
                 Format.sprintf "%s=%s"
                   (Aws_base.encode_form_string f)
                   (Aws_base.encode_form_string v))
              query)}
  | {Aws_base.meth = `POST; query = _ :: _} ->
      assert false
  | _ ->
      req

module String_map = Map.Make(struct
    type t = string
    let compare = compare
  end)

let unflatten_query l =
  String_map.bindings
    (List.fold_left
       (fun acc (id, v) ->
          let id = Aws_base.url_encode id and v = Aws_base.url_encode v in
          String_map.add id
            (try v :: String_map.find id acc with Not_found -> [v]) acc)
       String_map.empty
       l)

let perform ~credentials ~service ~region
      ?secure ~meth ~host ~uri ?query ?headers ?payload () =
  let {Aws_base.secure; meth; uri; query; headers; payload } as req =
    Aws_base.request ?secure ~meth ~host ~uri ?query ?headers ?payload ()
    |> encode_post_query
    |> Aws_signature.sign_request credentials ~service region
  in
  if debug () then Aws_base.print_curl_request req;
  let headers =
    List.fold_left
      (fun acc (id, v) -> Cohttp.Header.add acc id v)
      (Cohttp.Header.init ())
      headers
  and body =
    match meth with
    | `POST | `PUT    -> Some (Cohttp_lwt_body.of_string payload)
    | `GET  | `DELETE | `HEAD -> None
  and uri =
    Uri.make ()
      ~scheme:(if secure then "https" else "http")
      ~host ~path:uri ~query:(unflatten_query query)
  in
  Cohttp_lwt_unix.Client.call ~headers ?body
    (meth :> Cohttp.Code.meth)
    uri >>= fun (response, body) ->
  let code = Cohttp.Response.status response
  and headers = Cohttp.Response.headers response in
  Cohttp_lwt_body.to_string body >>= fun body ->
  if debug () then begin
    Format.eprintf "HTTP response: %d@." (Cohttp.Code.code_of_status code);
    Cohttp.Header.iter
      (fun id l -> List.iter (Format.eprintf "%s: %s@." id) l)
      headers;
    Format.eprintf "%s@." body
  end;
  match code with
  | `OK | `Created | `No_content ->
    Lwt.return body
  | _ ->
    let error =
      {Aws_common.request_id = "";  (*XXX*)
       code = Cohttp.Code.code_of_status code;
       typ = "";
       message = body}
    in
    Lwt.fail (Aws_common.Error error)

(* XXX
- crash if cannot connect!

ocsigen http_errors...

Error:
  x-amzn-requestid
  x-amzn-errortype

  {message: ...}
   Message

code = 429 / 500 ==> retry
*)
