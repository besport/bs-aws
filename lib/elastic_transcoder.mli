module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  type frame_rate =
    [`_10 | `_15 | `_23_97 | `_24 | `_25 | `_29_97 | `_30 | `_50 | `_60]

  type aspect_ratio = [`auto | `_16_9 | `_1_1 | `_3_2 | `_4_3]

  module Preset : sig
    type 'a parameters

    type sizing_policy =
      [`Fit | `Fill | `Stretch | `Keep | `ShrinkToFit | `ShrinkToFill]

    type padding_policy = [`Pad | `NoPad]

    val video
      :  codec:[< `gif | `H264 | `mpeg2 | `vp8 | `vp9]
      -> ?profile:[< `baseline | `main | `high | `_0 | `_1 | `_2 | `_3]
      -> ?level:
           [< `_1
           | `_1b
           | `_1_1
           | `_1_2
           | `_1_3
           | `_2
           | `_2_1
           | `_2_2
           | `_3
           | `_3_1
           | `_3_2
           | `_4
           | `_4_1 ]
      -> ?max_reference_frames:int
      -> ?max_bit_rate:int
      -> ?buffer_size:int
      -> ?interlaced_mode:[< `Progressive | `TopFirst | `BottomFirst | `Auto]
      -> ?color_space_conversion:
           [< `None | `Bt709ToBt601 | `Bt601ToBt709 | `Auto]
      -> ?chroma_subsampling:[< `yuv420p | `yuv422p]
      -> ?loop_count:int option
      -> ?keyframes_max_dist:int
      -> ?fixed_GOP:bool
      -> bit_rate:int option
      -> frame_rate:[< `auto | frame_rate]
      -> ?max_frame_rate:[< frame_rate]
      -> max_width:int option
      -> max_height:int option
      -> sizing_policy:[< sizing_policy]
      -> padding_policy:[< padding_policy]
      -> display_aspect_ratio:[< aspect_ratio]
      -> unit
      -> [`video] parameters

    val audio
      :  codec:[< `AAC | `flac | `mp2 | `mp3 | `pcm | `vorbis]
      -> ?profile:[< `auto | `AAC_LC | `HE_AAC | `HE_AACv2]
      -> ?bit_depth:[< `_8 | `_16 | `_24 | `_32]
      -> ?signed:[< `Signed | `Unsigned]
      -> ?bit_order:[< `LittleEndian]
      -> sample_rate:[< `auto | `_22050 | `_32000 | `_44100 | `_48000 | `_96000]
      -> bit_rate:int
      -> channels:[< `auto | `_0 | `_1 | `_2]
      -> ?audio_packing_mode:
           [< `SingleTrack
           | `OneChannelPerTrack
           | `OneChannelPerTrackWithMosTo8Tracks ]
      -> unit
      -> [`audio] parameters

    val thumbnails
      :  format:[< `jpg | `png]
      -> interval:int
      -> max_width:int option
      -> max_height:int option
      -> sizing_policy:[< sizing_policy]
      -> padding_policy:[< padding_policy]
      -> unit
      -> [`thumbnails] parameters

    val preset
      :  name:string
      -> description:string
      -> container:
           [< `flac
           | `flv
           | `fmp4
           | `gif
           | `mp2
           | `mp3
           | `mp4
           | `mpg
           | `mxf
           | `oga
           | `ogg
           | `ts
           | `wav
           | `webm ]
      -> ?audio:[`audio] parameters
      -> ?video:[`video] parameters
      -> ?thumbnails:[`thumbnails] parameters
      -> unit
      -> [`preset] parameters

    val create
      :  preset:[`preset] parameters
      -> unit
      -> (*warning:*) string option Lwt.t

    type preset = {id : string; name : string}

    val list
      :  ?page_token:string
      -> unit
      -> (preset list * (*next_page_token*) string option) Lwt.t
  end

  module Job : sig
    type 'a parameters
    type input_encryption = [`AES_CBC_PKCS7 | `AES_CTR | `AES_GCM]
    type output_encryption = [input_encryption | `AES_GCM | `S3 | `S3_AWS_KMS]

    val encryption
      :  mode:'mode
      -> ?key:string
      -> ?key_md5:string
      -> ?initialization_vector:string
      -> unit
      -> ([< output_encryption] as 'mode) parameters

    val input
      :  key:string
      -> ?encryption:[< input_encryption] parameters
      -> ?frame_rate:[< `auto | frame_rate]
      -> ?aspect_ratio:[< aspect_ratio]
      -> ?interlaced:[< `Auto | `False | `True]
      -> ?container:
           [< `_3gp
           | `aac
           | `asf
           | `auto
           | `avi
           | `divx
           | `flv
           | `m4a
           | `mkv
           | `mov
           | `mp3
           | `mp4
           | `mpeg
           | `mpeg_ps
           | `mpeg_ts
           | `mxf
           | `ogg
           | `vob
           | `wav
           | `webm ]
      -> unit
      -> [`input] parameters

    val output
      :  key:string
      -> ?encryption:[< output_encryption] parameters
      -> ?thumbnail_pattern:string
      -> preset_id:string
      -> ?rotate:[< `auto | `_0 | `_90 | `_180 | `_270]
      -> ?segment_duration:int
      -> unit
      -> [`output] parameters

    val playlist
      :  format:[< `HLSv3 | `HLSv4 | `MPEG_DASH | `Smooth]
      -> name:string
      -> output_keys:string list
      -> unit
      -> [`playlist] parameters

    val create
      :  input:[`input] parameters
      -> ?output_key_prefix:string
      -> outputs:[`output] parameters list
      -> ?playlists:[`playlist] parameters list
      -> ?user_metadata:(string * string) list
      -> pipeline_id:string
      -> unit
      -> (*Job id:*) string Lwt.t

    type input = {key : string}
    type output = {key : string; width : int option; height : int option}

    type t =
      { status : [`Submitted | `Progressing | `Complete | `Canceled | `Error]
      ; input : input
      ; outputs : output list
      ; user_metadata : (string * string) list }

    val read : id:string -> unit -> t Lwt.t
  end
end
