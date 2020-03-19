type endpoint = {address : string; port : int; hosted_zone_id : string}

type status_info =
  { status_type : string
  ; normal : bool
  ; status : string
  ; message : string option }

type db_instance =
  { db_instance_identifier : string
  ; endpoint : endpoint option
  ; db_instance_status : string
  ; status_infos : status_info list
  ; read_replica_db_instance_identifiers : string list }

module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  val describe_db_instances
    :  ?db_instance_identifier:string
    -> unit
    -> db_instance list Lwt.t
end
