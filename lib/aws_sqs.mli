val create_queue :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  ?delay_seconds:int ->
  ?maximum_message_size:int ->
  ?message_retention_period:int ->
  ?policy:Yojson.Safe.t ->
  ?receive_message_wait_time_seconds:int ->
  ?redrive_policy:Yojson.Safe.t ->
  ?visibility_timeout:int ->
  ?fifo_queue:bool ->
  ?content_based_deduplication:bool ->
  queue_name:string -> unit -> string Lwt.t

val set_queue_attributes :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  ?delay_seconds:int ->
  ?maximum_message_size:int ->
  ?message_retention_period:int ->
  ?policy:Yojson.Safe.t ->
  ?receive_message_wait_time_seconds:int ->
  ?redrive_policy:Yojson.Safe.t ->
  ?visibility_timeout:int ->
  ?content_based_deduplication:bool ->
  queue_url:string -> unit -> unit Lwt.t

module Attribute : sig
  type set
  type t
  val all : t
  val approximate_number_of_messages : t
  val approximate_number_of_messages_delayed : t
  val approximate_number_of_messages_not_visible : t
  val created_timestamp : t
  val delay_seconds : t
  val last_modified_timestamp : t
  val maximum_message_size : t
  val message_retention_period : t
  val policy : t
  val queue_arn : t
  val receive_message_wait_time_seconds : t
  val redrive_policy : t
  val visibility_timeout : t
  val fifo_queue : t
  val content_based_deduplication : t
  module Get : sig
    type 'a t = set -> 'a
    val approximate_number_of_messages : int t
    val approximate_number_of_messages_delayed : int t
    val approximate_number_of_messages_not_visible : int t
    val created_timestamp : float t
    val delay_seconds : int t
    val last_modified_timestamp : float t
    val maximum_message_size : int t
    val message_retention_period : int t
    val policy : Yojson.Safe.t t
    val queue_arn : string t
    val receive_message_wait_time_seconds : int t
    val redrive_policy : Yojson.Safe.t t
    val visibility_timeout : int t
    val fifo_queue : bool t
    val content_based_deduplication : bool t
  end
end

val get_queue_attributes :
  credentials:Aws_common.credentials -> region:Aws_common.Region.t ->
  attributes:Attribute.t list -> queue_url:string -> unit ->
  Attribute.set Lwt.t

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
