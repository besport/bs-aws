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
      {Aws_common.request_id = "";  (*XXX*)
       code = Cohttp.Code.code_of_status code;
       typ = "";
       message = body}
    in
    Lwt.fail (Aws_common.Error error)

let perform ~credentials ~service ~region
      ?secure ~meth ~host ?port ~uri ?query ?headers ?payload () =
  let {Aws_base.secure; meth; uri; query; headers; payload } as req =
    Aws_base.request ?secure ~meth ~host ~uri ?query ?headers ?payload ()
    |> encode_post_query
    |> Aws_signature.sign_request credentials ~service region
  in
  if debug () then Aws_base.print_curl_request req;
  simple_perform
    ~secure ~meth ~host ?port ~uri ~query ~headers ~payload ()

module type CONF = sig
  val credentials : Aws_common.credentials
  val service : string
  val region : Aws_common.Region.t
  val secure : bool option
  val host : string
end

module type NO_AWS_CONF = sig
  val secure : bool
  val host : string
  val port : int option
end

module type SERVICE = sig
  val perform :
    meth:Aws_base.meth ->
    uri:string ->
    ?query:(string * string) list ->
    ?headers:(string * string) list ->
    ?payload:string ->
    unit -> (string * Cohttp.Header.t) Lwt.t
end

module Service (Conf : CONF) = struct
  let perform ~meth ~uri ?query ?headers ?payload () =
    perform
      ~credentials:Conf.credentials ~service:Conf.service ~region:Conf.region
      ?secure:Conf.secure ~meth ~host:Conf.host ~uri ?query ?headers ?payload ()
end

module NoAws (Conf : NO_AWS_CONF) = struct
  let perform ~meth ~uri ?query ?headers ?payload () =
    simple_perform ~secure:Conf.secure ~meth ~host:Conf.host
                   ?port:Conf.port ~uri ?query ?headers ?payload ()
end

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
