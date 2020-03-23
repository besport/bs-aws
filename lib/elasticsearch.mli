module type CONF = sig
  val host : string
end

module Of_json : sig
  val string : string -> Yojson.Basic.t -> string
  val strings : string -> Yojson.Basic.t -> string list
  val float : string -> Yojson.Basic.t -> float
  val int : string -> Yojson.Basic.t -> int
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
      -> Yojson.Basic.t
      -> hit list Lwt.t
  end

  val template_exists : string -> bool Lwt.t
  val delete_template : string -> unit Lwt.t
  val put_template : template:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
  val put_mapping : index:string -> Yojson.Basic.t -> Yojson.Safe.t Lwt.t
end

module Make (ServiceConf : Service.CONF) (EsConf : CONF) : S
module MakeNoAws (Conf : Service.CONF_NO_AWS) : S
