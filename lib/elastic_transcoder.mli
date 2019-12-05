
module Job : sig
  type 'a parameters

  type input_encryption = [`AES_CBC_PKCS7 | `AES_CTR | `AES_GCM]
  type output_encryption = [input_encryption | `AES_GCM | `S3 | `S3_AWS_KMS ]

  val encryption :
    mode:'mode -> ?key:string -> ?key_md5:string ->
    ?initialization_vector:string -> unit ->
    ([<output_encryption] as 'mode) parameters

  val input :
    key:string -> ?encryption:[<input_encryption] parameters ->
    ?frame_rate:[`auto|`_10|`_15|`_23_97|`_24|`_25|`_29_97|`_30|`_50|`_60] ->
    ?aspect_ratio:[`auto|`_16_9|`_1_1|`_3_2|`_4_3] ->
    ?interlaced:[`Auto|`False|`True] ->
    ?container:[`_3gp|`aac|`asf|`auto|`avi|`divx|`flv|`m4a|`mkv|`mov|`mp3
               |`mp4|`mpeg|`mpeg_ps|`mpeg_ts|`mxf|`ogg|`vob|`wav|`webm] ->
    unit ->
    [`input] parameters

  val output :
    key:string ->
    ?encryption:[<output_encryption] parameters ->
    ?thumbnail_pattern:string ->
    preset_id:string ->
    unit ->
    [`output] parameters

  val create :
    credentials:Common.credentials -> region:Common.Region.t ->
    input:[`input] parameters ->
    ?output_key_prefix:string -> outputs:[`output] parameters list ->
    pipeline_id:string -> unit ->
    (*Job id:*)string Lwt.t

  type input =
    { key : string }
  type output =
    { key : string;
      width : int option;
      height : int option }
  type t =
    { status : [ `Submitted | `Progressing | `Complete | `Canceled | `Error ];
      input : input;
      outputs : output list }

  val read :
    credentials:Common.credentials -> region:Common.Region.t ->
    id:string -> unit -> t Lwt.t
end
