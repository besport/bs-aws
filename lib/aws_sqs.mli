val create_queue :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  ?delay_seconds:int ->
  ?maximum_message_size:int ->
  ?message_retention_period:int ->
  ?policy:string ->
  ?receive_message_wait_time_seconds:int ->
  ?redrive_policy:string ->
  ?visibility_timeout:int ->
  ?fifo_queue:bool ->
  ?content_based_deduplication:string ->
  queue_name:string -> unit -> string Lwt.t

val delete_queue :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  queue_url:string -> unit -> unit Lwt.t

val send_message :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  queue_url:string -> message_body:string -> unit -> unit Lwt.t

type message =
  { message_id : string;
    receipt_handle : string;
    body : string }

val receive_message :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  ?max_number_of_messages:int ->
  ?visibility_timeout:int ->
  ?wait_time_seconds:int ->
  queue_url:string -> unit -> message list Lwt.t

val delete_message :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  queue_url:string -> receipt_handle:string -> unit -> unit Lwt.t
