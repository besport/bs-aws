module Make (Conf : Service.CONF) : sig
  type parameter = {
      name : string
    ; typ : [ `String | `StringList | `SecureString ]
    ; value : string
    ; version : int
    ; last_modified_date : float
    ; arn : string
    ; data_type : string
  }

  val get_parameters_by_path
    :  ?next_token:string
    -> ?with_decryption:bool
    -> path:string
    -> unit
    -> (parameter list * (*next_token:*) string option) Lwt.t
end
