(*
Get credentials from the ECS container metadata or from the EC2
instance metadata.
*)

val get_defaults : unit -> Common.credentials Lwt.t
