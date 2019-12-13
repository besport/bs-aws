open Printf

let src = Logs.Src.create "bs-aws.elasticsearch" ~doc:"bs-aws.elasticsearch"
module Log = (val Logs.src_log src : Logs.LOG)

module MakeFromService (S : Service.SERVICE) = struct
  module Service : Service.SERVICE = S

  let json_headers = ["content-type", "application/json"]

  let index_exists index =
    try%lwt
      let%lwt _ = Service.request ~meth:`HEAD ~uri:("/" ^ index) () in
      Lwt.return true
    with Common.(Error {code = 404}) -> Lwt.return_false

  let put ~index json =
    let data = Yojson.Basic.to_string json in
    let headers = json_headers in
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`PUT ~payload:data ~uri:("/" ^ index) ()
    in
    Lwt.return @@ Yojson.Safe.from_string response_body

  let delete_index index =
    Log.info (fun m -> m "deleting index /%s" index);
    let%lwt response, _ = Service.request ~meth:`DELETE ~uri:("/" ^ index) () in
    Lwt.return @@ Log.info @@ fun m -> m "deleted index /%s: %s" index response

  let bulk_index ~index docs =
    Log.warn (fun m -> m "pushing %d documents to %s" (List.length docs) index);
    let format_doc (id, content) =
      let command = `Assoc ["index", `Assoc ["_id", `String id]] in
      sprintf "%s\n%s\n"
        (Yojson.Basic.to_string command)
        (Yojson.Basic.to_string content)
    in
    let data = String.concat "" @@ List.map format_doc docs in
    let headers = json_headers in
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`POST ~payload:data
        ~uri:(sprintf "/%s/_bulk" index)
        ()
    in
    let parse_response r = Yojson.Safe.Util.(to_bool @@ member "errors" r) in
    let json = Yojson.Safe.from_string response_body in
    let errors = parse_response json in
    if errors
    then (
      Log.err (fun m -> m "%s : %s" __LOC__ response_body);
      failwith "errors occurred during Elasticsearch.bulk_index")
    else Lwt.return_unit

  let reindex ?(wait_for_completion = true) source dest =
    let uri = "/_reindex" in
    let query = ["wait_for_completion", string_of_bool wait_for_completion] in
    Log.info (fun m -> m "reindexing /%s -> /%s" source dest);
    let payload =
      Yojson.Basic.to_string
      @@ `Assoc
           [ "source", `Assoc ["index", `String source]
           ; "dest", `Assoc ["index", `String dest] ]
    in
    let%lwt _ =
      Service.request ~meth:`POST ~headers:json_headers ~payload ~uri ~query ()
    in
    Lwt.return @@ Log.info (fun m -> m "reindexing launched on /%s" dest)

  let query ~index ?count ?source content =
    let size = match count with None -> [] | Some c -> ["size", `Int c] in
    let source = match source with None -> [] | Some l -> ["_source", `List (List.map (fun s -> `String s) l)] in
    let payload =
      Yojson.Basic.to_string @@ `Assoc (source @ size @ ["query", content])
    in
    let headers = json_headers in
    Log.debug (fun m -> m "/%s/_search %s" index payload);
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`POST ~payload
        ~uri:(sprintf "/%s/_search" index)
        ()
    in
    Log.debug (fun m -> m "response: %s" response_body);
    Lwt.return @@ Yojson.Safe.from_string response_body
end

module Make (Conf : Service.CONF) =
  MakeFromService (Service.Make (Conf) (struct let name = "es" end))

module MakeNoAws (Conf : Service.CONF_NO_AWS) =
  MakeFromService (Service.MakeNoAws (Conf))
