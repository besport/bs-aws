module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  val form
    :  bucket:string
    -> expiration:[`Date of float | `Delay of float]
    -> key:[`Eq of string | `Prefix of string * string]
    -> ?content_length_range:int * int
    -> ?success_action_redirect:[`Eq of string | `Prefix of string * string]
    -> ?success_action_status:string
    -> ?other_fields:(string * [`Eq of string | `Prefix of string]) list
    -> unit
    -> (string * (string * string) list) Lwt.t

  val object_url
    :  bucket:string
    -> expiration:int
    -> key:string
    -> ?response_content_type:string
    -> ?response_content_language:string
    -> ?response_expires:string
    -> ?response_cache_control:string
    -> ?response_content_disposition:string
    -> ?response_content_encoding:string
    -> unit
    -> Uri.t Lwt.t

  val list : unit -> string Lwt.t

  module Bucket : sig
    type contents = {key : string}
    type list_result = {is_truncated : bool; contents : contents list}

    val list : ?prefix:string -> string -> list_result Lwt.t
  end

  module Object : sig
    val delete : bucket:string -> string -> unit Lwt.t
    val put : bucket:string -> string -> string -> unit Lwt.t

    val copy
      :  src_bucket:string
      -> string
      -> dst_bucket:string
      -> string
      -> unit Lwt.t

    val get : bucket:string -> string -> string Lwt.t
    val head : bucket:string -> string -> string Lwt.t
  end
end
