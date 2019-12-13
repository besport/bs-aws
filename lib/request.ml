let debug = Base.Debug.make "request" "Debug HTTP requests." ["all"]

let (>>=) = Lwt.bind

let encode_post_query req =
  match req with
  | {Base.meth = `POST;
     query = (_ :: _) as query; payload = ""; headers} ->
      {req with
       Base.headers =
         ("content-type", "application/x-www-form-urlencoded") :: headers;
       query = [];
       payload =
         String.concat "&"
           (List.map
              (fun (f, v) ->
                 Format.sprintf "%s=%s"
                   (Base.encode_form_string f)
                   (Base.encode_form_string v))
              query)}
  | _ ->
      req

module String_map = Map.Make(struct
    type t = string
    let compare = compare
  end)

let simple_perform ~secure ~meth ~host ?port ~uri
                   ?(query = []) ?(headers = []) ?(payload = "") () =
  let headers =
    List.fold_left
      (fun acc (id, v) -> Cohttp.Header.add acc id v)
      (Cohttp.Header.init ())
      headers
  and body =
    match meth with
    | `POST | `PUT    -> Some (Cohttp_lwt.Body.of_string payload)
    | `GET  | `DELETE | `HEAD -> None
  and uri =
    Uri.make ()
      ~scheme:(if secure then "https" else "http") ?port
      ~host ~path:uri ~query:(List.map (fun (k, v) -> (k, [v])) query)
  in
  Cohttp_lwt_unix.Client.call ~chunked:false ~headers ?body
    (meth :> Cohttp.Code.meth)
    uri >>= fun (response, body) ->
  let code = Cohttp.Response.status response
  and headers = Cohttp.Response.headers response in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  if debug () then begin
    Format.eprintf "HTTP response: %d@." (Cohttp.Code.code_of_status code);
    Cohttp.Header.iter
      (fun id l -> List.iter (Format.eprintf "%s: %s@." id) l)
      headers;
    Format.eprintf "%s@." body
  end;
  match code with
  | `OK | `Created | `No_content ->
    Lwt.return (body, headers)
  | _ ->
    let error =
      {Common.request_id = "";  (*XXX*)
       code = Cohttp.Code.code_of_status code;
       typ = "";
       message = body}
    in
    Lwt.fail (Common.Error error)

let perform ~credentials ~service ~region
      ?secure ~meth ~host ?port ~uri ?query ?headers ?payload () =
  let {Base.secure; meth; uri; query; headers; payload } as req =
    Base.request ?secure ~meth ~host ~uri ?query ?headers ?payload ()
    |> encode_post_query
    |> Signature.sign_request credentials ~service region
  in
  if debug () then Base.print_curl_request req;
  simple_perform
    ~secure ~meth ~host ?port ~uri ~query ~headers ~payload ()

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
