let debug = Aws_base.Debug.make "request" "Debug HTTP requests." ["all"]

let translate_meth m =
  let open Ocsigen_http_frame.Http_header in
  match m with
    `GET    -> GET
  | `POST   -> POST
  | `PUT    -> PUT
  | `DELETE -> DELETE

let (>>=) = Lwt.bind

let perform ~credentials ~service ~region
      ?secure ~meth ~host ~uri ?query ?headers ?payload () =
  let {Aws_base.secure; meth; uri; query; headers; payload } as req =
    Aws_signature.sign_request credentials ~service region
      (Aws_base.request ?secure ~meth ~host ~uri ?query ?headers ?payload ())
  in
  if debug () then Aws_base.print_curl_request req;
  let uri =
    if query = [] then uri else
      uri ^ "?" ^
      String.concat "&"
        (List.map
           (fun (f, v) ->
              Format.sprintf "%s=%s"
                (Aws_base.url_encode f) (Aws_base.url_encode v))
           query)
  in
  let headers =
    List.fold_left
      (fun h (n, v) ->
        Http_headers.add (Http_headers.name n) v h)
      Http_headers.empty headers
  in
  Ocsigen_lib.Ip_address.get_inet_addr host >>= fun inet_addr ->
  Ocsigen_http_client.raw_request ~keep_alive:false ~headers ~https:secure
    ?port:(if secure then Some 443 else None)
    ~content:(match meth with
                `POST | `PUT    -> Some (Ocsigen_stream.of_string payload)
              | `GET  | `DELETE -> None)
    ?content_length:(match meth with
                `POST | `PUT    -> Some (String.length payload |> Int64.of_int)
              | `GET  | `DELETE -> None)
    ~http_method:(translate_meth meth)
    ~host
    ~uri
    ~inet_addr
    () () >>= fun resp ->
  let { Ocsigen_http_frame.frame_header = header; frame_content = content } =
    resp in
  let (code, headers) =
    match header with
      { Ocsigen_http_frame.Http_header.mode =
          Ocsigen_http_frame.Http_header.Answer code; headers } ->
      (code, headers)
    | _ ->
      assert false
  in
  begin match content with
    Some content ->
      Lwt.finalize
        (fun () ->
           Ocsigen_stream.string_of_stream 16384 (Ocsigen_stream.get content))
        (fun () ->
           Ocsigen_stream.finalize content `Success)
  | None ->
      Lwt.return ""
  end >>= fun content ->
  let headers =
    Http_headers.fold (fun s l acc ->
    let s = Http_headers.name_to_string s in
    (List.map (fun v -> (s,v)) l) @ acc) headers []
  in
  if debug () then begin
    Format.eprintf "HTTP response: %d@." code;
    List.iter (fun (s, v) -> Format.eprintf "%s: %s@." s v) headers;
    Format.eprintf "%s@." content
  end;
  match code with
    200 | 201 ->
      Lwt.return content
  | _ ->
      let error =
        {Aws_common.request_id = "";  (*XXX*)
         code = code;
         typ = "";
         message = content}
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
