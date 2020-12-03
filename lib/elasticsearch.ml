open Printf

let src = Logs.Src.create "bs-aws.elasticsearch" ~doc:"bs-aws.elasticsearch"

module Log = (val Logs.src_log src : Logs.LOG)

module Of_json = struct
  open Yojson.Basic.Util

  let try_with transform field j =
    try transform @@ member field j
    with Type_error _ as exn ->
      prerr_string @@ "error while parsing field \"" ^ field ^ "\": \n";
      prerr_string @@ Yojson.Basic.to_string j ^ "\n";
      flush stdout;
      raise exn

  let list f = try_with (fun j -> List.map f @@ to_list j)
  let string = try_with to_string
  let strings = list to_string
  let float = try_with to_float
  let int = try_with to_int
end

module type S = sig
  module Service : Service.S

  type json = Yojson.Basic.t

  val index_exists : string -> bool Lwt.t
  val get_document : index:string -> string -> json Lwt.t
  val index_document : index:string -> doc:string -> json -> unit Lwt.t
  val update_document : index:string -> doc:string -> json -> unit Lwt.t
  val delete_document : index:string -> string -> unit Lwt.t
  val put_index : index:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
  val delete_index : string -> unit Lwt.t
  val bulk : Yojson.Basic.t list -> Yojson.Safe.t Lwt.t
  val reindex : ?wait_for_completion:bool -> string -> string -> unit Lwt.t
  val count : index:string -> Yojson.Basic.t -> int Lwt.t

  module Search : sig
    type hit =
      { _index : string
      ; _type : string
      ; _id : string
      ; _score : float
      ; _source : json }

    val query
      :  index:string
      -> ?count:int
      -> ?source:string list
      -> ?request_cache:bool
      -> Yojson.Basic.t
      -> hit list Lwt.t

    val aggs
      :  index:string
      -> ?count:int
      -> ?source:string list
      -> ?request_cache:bool
      -> Yojson.Basic.t
      -> (hit list * Yojson.Basic.t) Lwt.t

    val hit_bucket_agg
      :  index:string
      -> ?count:int
      -> ?source:string list
      -> ?request_cache:bool
      -> Yojson.Basic.t
      -> ?subagg:string
      -> (string * (string * json) list)
      -> (hit list * (json * hit list) list) Lwt.t
  end

  val template_exists : string -> bool Lwt.t
  val delete_template : string -> unit Lwt.t
  val put_template : template:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
  val put_mapping : index:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
end

module MakeFromService (Service_in : Service.S) : S = struct
  module Service : Service.S = Service_in

  type json = Yojson.Basic.t

  let json_headers = ["content-type", "application/json"]

  let index_exists index =
    try%lwt
      let%lwt _ = Service.request ~meth:`HEAD ~uri:("/" ^ index) () in
      Lwt.return true
    with Common.(Error {code = 404}) -> Lwt.return_false

  let get_document ~index doc =
    let uri = sprintf "/%s/_doc/%s" index doc in
    let%lwt response_body, _ = Service.request ~meth:`GET ~uri () in
    Lwt.return @@ Yojson.Basic.from_string response_body

  let index_document ~index ~doc json =
    let uri = sprintf "/%s/_doc/%s" index doc in
    let headers = json_headers in
    let payload = Yojson.Basic.to_string json in
    let%lwt _ = Service.request ~headers ~meth:`PUT ~uri ~payload () in
    Lwt.return_unit

  let update_document ~index ~doc json =
    let uri = sprintf "/%s/_update/%s" index doc in
    let headers = json_headers in
    let payload = Yojson.Basic.to_string json in
    let%lwt _ = Service.request ~headers ~meth:`POST ~uri ~payload () in
    Lwt.return_unit

  let delete_document ~index doc =
    let uri = sprintf "/%s/_doc/%s" index doc in
    let%lwt _ = Service.request ~meth:`DELETE ~uri () in
    Lwt.return_unit

  let put_index ~index json =
    let data = Yojson.Basic.to_string json in
    let headers = json_headers in
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`PUT ~payload:data ~uri:("/" ^ index) ()
    in
    Lwt.return @@ Yojson.Safe.from_string response_body

  let delete_index index =
    Log.info (fun m -> m "deleting index /%s" index);
    let%lwt response, _ =
      Service.request ~meth:`DELETE ~uri:("/" ^ index) ()
    in
    Lwt.return @@ Log.info @@ fun m -> m "deleted index /%s: %s" index response

  let bulk jsons =
    let lines = List.map (fun j -> Yojson.Basic.to_string j ^ "\n") jsons in
    let data = String.concat "" lines in
    let headers = json_headers in
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`POST ~payload:data ~uri:"/_bulk" ()
    in
    Lwt.return @@ Yojson.Safe.from_string response_body

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

  let count ~index content =
    let payload = Yojson.Basic.to_string @@ `Assoc ["query", content] in
    let headers = json_headers in
    Log.debug (fun m -> m "/%s/_count %s" index payload);
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`POST ~payload
        ~uri:(sprintf "/%s/_count" index)
        ()
    in
    Log.debug (fun m -> m "response: %s" response_body);
    let hits =
      let response = Yojson.Basic.from_string response_body in
      try Of_json.int "count" response
      with exn -> (*TODO: print json*) raise exn
    in
    Lwt.return hits

  module Search = struct
    type hit =
      { _index : string
      ; _type : string
      ; _id : string
      ; _score : float
      ; _source : json }

    let hit_of_json j =
      let open Of_json in
      { _index = string "_index" j
      ; _type = string "_type" j
      ; _id = string "_id" j
      ; _score = float "_score" j
      ; _source = Yojson.Basic.Util.member "_source" j }

    let request ~index ?count ?source ?(request_cache = false) content =
      let size = match count with None -> [] | Some c -> ["size", `Int c] in
      let source =
        match source with
        | None -> []
        | Some l -> ["_source", `List (List.map (fun s -> `String s) l)]
      in
      let payload =
        Yojson.Basic.to_string @@ `Assoc (source @ size @ content)
      in
      let request_cache_headers =
        if request_cache then ["request_cache", "true"] else []
      in
      let headers = request_cache_headers @ json_headers in
      Log.debug (fun m -> m "/%s/_search %s" index payload);
      let%lwt response_body, response_headers =
        Service.request ~headers ~meth:`POST ~payload
          ~uri:(sprintf "/%s/_search" index)
          ()
      in
      Log.debug (fun m -> m "response: %s" response_body);
      Lwt.return (response_body, response_headers)

    let query ~index ?count ?source ?request_cache content =
      let%lwt response_body, _ =
        request ~index ?count ?source ?request_cache ["query", content]
      in
      let hits =
        let response = Yojson.Basic.from_string response_body in
        try
          let open Yojson.Basic.Util in
          List.map hit_of_json @@ to_list @@ member "hits"
          @@ member "hits" response
        with exn -> (*TODO: print json*) raise exn
      in
      Lwt.return hits

    let aggs ~index ?count ?source ?request_cache content =
      let%lwt response_body, _ =
        request ~index ?count ?source ?request_cache ["aggs", content]
      in
      let response = Yojson.Basic.from_string response_body in
      let hits, aggregations =
        try
          let open Yojson.Basic.Util in
          let hits = List.map hit_of_json @@
                       to_list @@ member "hits" @@ member "hits" response
          and aggregations = member "aggregations" response
          in hits, aggregations
        with exn -> (*TODO: print json*) raise exn
      in
      Lwt.return (hits, aggregations)

    let hit_bucket_agg ~index ?count ?source ?request_cache query
    ?subagg (agg_name, agg) =
      let%lwt response_body, _ =
        request ~index ?count ?source ?request_cache
          ["query", query; "aggs", `Assoc [agg_name, `Assoc agg]]
      in
      let response = Yojson.Basic.from_string response_body in
      let query_hits, buckets =
        try
          let open Yojson.Basic.Util in
          let hits = List.map hit_of_json @@
                       to_list @@ member "hits" @@ member "hits" response
          and aggregations = member "aggregations" response
          in
          let parse_bucket b =
            let hit_container = match subagg with
              | None -> b
              | Some s -> member s b
            in
            let key = member "key" b
            and hits = List.map hit_of_json @@
                         to_list @@ member "hits" @@ member "hits" hit_container
            in key, hits
          in
          let buckets = List.map parse_bucket @@
                          to_list @@
                            member "buckets" @@
                            member agg_name aggregations
          in
          hits, buckets
        with exn -> (*TODO: print json*) raise exn
      in
      Lwt.return (query_hits, buckets)
  end

  let template_exists template =
    try%lwt
      let%lwt _ =
        Service.request ~meth:`HEAD ~uri:(sprintf "/_template/%s" template) ()
      in
      Lwt.return true
    with Common.(Error {code = 404}) -> Lwt.return_false

  let delete_template t =
    Log.info (fun m -> m "deleting template %s" t);
    let%lwt response, _ =
      Service.request ~meth:`DELETE ~uri:("/_template/" ^ t) ()
    in
    Lwt.return @@ Log.info @@ fun m -> m "deleted template %s: %s" t response

  let put_template ~template specification =
    let data = Yojson.Basic.to_string specification in
    let headers = json_headers in
    let uri = "/_template/" ^ template in
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`PUT ~payload:data ~uri ()
    in
    Lwt.return @@ Yojson.Safe.from_string response_body

  let put_mapping ~index mapping =
    let uri = sprintf "/%s/_mapping" index in
    let headers = json_headers in
    let payload = Yojson.Basic.to_string mapping in
    let%lwt response_body, _ =
      Service.request ~headers ~meth:`PUT ~uri ~payload ()
    in
    Lwt.return @@ Yojson.Safe.from_string response_body
end

module type CONF = sig
  val host : string
end

module Make (ServiceConf : Service.CONF) (EsConf : CONF) : S =
  MakeFromService
    (Service.Make
       (ServiceConf)
       (struct
         let name = "es"
         and host = EsConf.host
       end))

module MakeNoAws (Conf : Service.CONF_NO_AWS) : S =
  MakeFromService (Service.MakeNoAws (Conf))
