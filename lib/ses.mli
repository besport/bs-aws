module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  type content = {charset : string option; data : string}

  val content : ?charset:string -> string -> content

  type body = {html : content option; text : content option}

  val body : ?html:content -> ?text:content -> unit -> body

  type message = {subject : content; body : body}

  type destination =
    { bcc_addresses : string list
    ; cc_addresses : string list
    ; to_addresses : string list }

  val dest
    :  ?bcc:string list
    -> ?cc:string list
    -> ?to_:string list
    -> unit
    -> destination

  val send_email
    :  ?configuration_set_name:string
    -> destination:destination
    -> message:message
    -> ?reply_to_addresses:string list
    -> ?return_path:string
    -> ?return_path_arn:string
    -> source:string
    -> ?source_arn:string
    -> ?tags:(string * string) list
    -> unit
    -> unit Lwt.t
end
