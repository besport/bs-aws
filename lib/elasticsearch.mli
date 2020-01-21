module type CONF = sig
  val host : string
end

module type S = sig
  module Service : Service.S
  type json = Yojson.Basic.t
  val index_exists : string -> bool Lwt.t
  val get_document : index:string -> string -> json Lwt.t
  val update_document : index:string -> doc:string -> json -> unit Lwt.t
  val put_index : index:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
  val delete_index : string -> unit Lwt.t
  val bulk_index : index:string -> (string * Yojson.Basic.t) list -> unit Lwt.t
  val reindex : ?wait_for_completion:bool -> string -> string -> unit Lwt.t
  val query : index:string -> ?count:int -> ?source:string list ->
              Yojson.Basic.t -> Yojson.Safe.t Lwt.t
  val template_exists : string -> bool Lwt.t
  val delete_template : string -> unit Lwt.t
  val put_template : template:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
  val put_mapping : index:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
end

module Make (ServiceConf : Service.CONF) (EsConf : CONF) : S 

module MakeNoAws (Conf : Service.CONF_NO_AWS) : S
