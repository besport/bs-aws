
(*
Get credentials from the ECS container metadata or from the EC2
instance metadata.
*)

val get_defaults : unit -> Aws_common.credentials Lwt.t
