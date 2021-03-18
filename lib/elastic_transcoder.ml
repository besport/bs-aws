module Make (Conf : Service.CONF) = struct
  let endpoint (region : Common.Region.t) =
    Printf.sprintf "elastictranscoder.%s.amazonaws.com"
      (Common.Region.to_string region)

  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "elastictranscoder"
        and host = endpoint Conf.region
      end)

  type frame_rate =
    [`_10 | `_15 | `_23_97 | `_24 | `_25 | `_29_97 | `_30 | `_50 | `_60]

  let conv_frame_rate r =
    match r with
    | `auto -> "auto"
    | `_10 -> "10"
    | `_15 -> "15"
    | `_23_97 -> "23.97"
    | `_24 -> "24"
    | `_25 -> "25"
    | `_29_97 -> "29.97"
    | `_30 -> "30"
    | `_50 -> "50"
    | `_60 -> "60"

  type aspect_ratio = [`auto | `_16_9 | `_1_1 | `_3_2 | `_4_3]

  let conv_aspect_ratio r =
    match r with
    | `auto -> "auto"
    | `_1_1 -> "1:1"
    | `_4_3 -> "4:3"
    | `_3_2 -> "3:2"
    | `_16_9 -> "16:9"

  module Preset = struct
    type 'a parameters = Yojson.Safe.t

    type sizing_policy =
      [`Fit | `Fill | `Stretch | `Keep | `ShrinkToFit | `ShrinkToFill]

    type padding_policy = [`Pad | `NoPad]

    let audio ~codec ?profile ?bit_depth ?signed ?bit_order ~sample_rate
        ~bit_rate ~channels ?audio_packing_mode ()
      =
      let add f v g rem =
        match v with None -> rem | Some v -> (f, `String (g v)) :: rem
      in
      let a =
        [ ( "Codec"
          , `String
              (match codec with
              | `AAC -> "AAC"
              | `flac -> "flac"
              | `mp2 -> "mp2"
              | `mp3 -> "mp3"
              | `pcm -> "pcm"
              | `vorbis -> "vorbis") )
        ; ( "SampleRate"
          , `String
              (match sample_rate with
              | `auto -> "auto"
              | `_22050 -> "22050"
              | `_32000 -> "32000"
              | `_44100 -> "44100"
              | `_48000 -> "48000"
              | `_96000 -> "96000") )
        ; "BitRate", `String (string_of_int bit_rate)
        ; ( "Channels"
          , `String
              (match channels with
              | `auto -> "auto"
              | `_0 -> "0"
              | `_1 -> "1"
              | `_2 -> "2") )
        ; ( "CodecOptions"
          , `Assoc
              ([]
              |> add "Profile" profile (fun p ->
                     match p with
                     | `auto -> "auto"
                     | `AAC_LC -> "AAC-LC"
                     | `HE_AAC -> "HE-AAC"
                     | `HE_AACv2 -> "HE-AACv2")
              |> add "BitDepth" bit_depth (fun d ->
                     match d with
                     | `_8 -> "8"
                     | `_16 -> "16"
                     | `_24 -> "24"
                     | `_32 -> "32")
              |> add "Signed" signed (fun s ->
                     match s with
                     | `Signed -> "Signed"
                     | `Unsigned -> "Unsigned")
              |> add "BitOrder" bit_order (fun o ->
                     match o with `LittleEndian -> "LittleEndian")) ) ]
      in
      `Assoc
        (a
        |> add "AudioPackingMode" audio_packing_mode (fun m ->
               match m with
               | `SingleTrack -> "SingleTrack"
               | `OneChannelPerTrack -> "OneChannelPerTrack"
               | `OneChannelPerTrackWithMosTo8Tracks ->
                   "OneChannelPerTrackWithMosTo8Tracks"))

    let conv_size s = `String (Option.fold ~none:"auto" ~some:string_of_int s)

    let conv_sizing_policy p =
      `String
        (match p with
        | `Fit -> "Fit"
        | `Fill -> "Fill"
        | `Stretch -> "Stretch"
        | `Keep -> "Keep"
        | `ShrinkToFit -> "ShrinkToFit"
        | `ShrinkToFill -> "ShrinkToFill")

    let conv_padding_policy p =
      `String (match p with `Pad -> "Pad" | `NoPad -> "NoPad")

    let video ~codec ?profile ?level ?max_reference_frames ?max_bit_rate
        ?buffer_size ?interlaced_mode ?color_space_conversion
        ?chroma_subsampling ?loop_count ?keyframes_max_dist ?fixed_GOP ~bit_rate
        ~frame_rate ?max_frame_rate ~max_width ~max_height ~sizing_policy
        ~padding_policy ~display_aspect_ratio ()
      =
      let add f v g rem =
        match v with None -> rem | Some v -> (f, `String (g v)) :: rem
      in
      let a =
        [ ( "Codec"
          , `String
              (match codec with
              | `gif -> "gif"
              | `H264 -> "H.264"
              | `mpeg2 -> "mpeg2"
              | `vp8 -> "vp8"
              | `vp9 -> "vp9"
              | `flac -> "flac") )
        ; ( "BitRate"
          , `String (Option.fold ~none:"auto" ~some:string_of_int bit_rate) )
        ; "FrameRate", `String (conv_frame_rate frame_rate)
        ; "MaxWidth", conv_size max_width; "MaxHeight", conv_size max_height
        ; "SizingPolicy", conv_sizing_policy sizing_policy
        ; "PaddingPolicy", conv_padding_policy padding_policy
        ; "DisplayAspectRatio", `String (conv_aspect_ratio display_aspect_ratio)
        ; ( "CodecOptions"
          , `Assoc
              ([]
              |> add "Profile" profile (fun p ->
                     match p with
                     | `baseline -> "baseline"
                     | `main -> "main"
                     | `high -> "high"
                     | `_0 -> "0"
                     | `_1 -> "1"
                     | `_2 -> "2"
                     | `_3 -> "3")
              |> add "Level" level (fun l ->
                     match l with
                     | `_1 -> "1"
                     | `_1b -> "1b"
                     | `_1_1 -> "1.1"
                     | `_1_2 -> "1.2"
                     | `_1_3 -> "1.3"
                     | `_2 -> "2"
                     | `_2_1 -> "2.1"
                     | `_2_2 -> "2.2"
                     | `_3 -> "3"
                     | `_3_1 -> "3.1"
                     | `_3_2 -> "3.2"
                     | `_4 -> "4"
                     | `_4_1 -> "4.1")
              |> add "MaxReferenceFrames" max_reference_frames string_of_int
              |> add "MaxBitRate" max_bit_rate string_of_int
              |> add "BufferSize" buffer_size string_of_int
              |> add "InterlacedMode" interlaced_mode (fun m ->
                     match m with
                     | `Progressive -> "Progressive"
                     | `TopFirst -> "TopFirst"
                     | `BottomFirst -> "BottomFirst"
                     | `Auto -> "Auto")
              |> add "ColorSpaceConversion" color_space_conversion (fun c ->
                     match c with
                     | `None -> "None"
                     | `Bt709ToBt601 -> "Bt709ToBt601"
                     | `Bt601ToBt709 -> "Bt601ToBt709"
                     | `Auto -> "Auto")
              |> add "ChromaSubsampling" chroma_subsampling (fun s ->
                     match s with
                     | `yuv420p -> "yuv420p"
                     | `yuv422p -> "yuv422p")
              |> add "LoopCount" loop_count (fun c ->
                     match c with
                     | None -> "Infinite"
                     | Some c -> string_of_int c)) ) ]
      in
      `Assoc
        (a
        |> add "KeyframesMaxDist" keyframes_max_dist string_of_int
        |> add "FixedGOP" fixed_GOP string_of_bool
        |> add "MaxFrameRate" max_frame_rate conv_frame_rate)

    let thumbnails ~format ~interval ~max_width ~max_height ~sizing_policy
        ~padding_policy ()
      =
      `Assoc
        [ "Format", `String (match format with `jpg -> "jpg" | `png -> "png")
        ; "Interval", `String (string_of_int interval)
        ; "MaxWidth", conv_size max_width; "MaxHeight", conv_size max_height
        ; "SizingPolicy", conv_sizing_policy sizing_policy
        ; "PaddingPolicy", conv_padding_policy padding_policy ]

    let preset ~name ~description ~container ?audio ?video ?thumbnails () =
      let add f v rem = match v with None -> rem | Some v -> (f, v) :: rem in
      `Assoc
        ([ "Name", `String name; "Description", `String description
         ; ( "Container"
           , `String
               (match container with
               | `flac -> "flac"
               | `flv -> "flv"
               | `fmp4 -> "fmp4"
               | `gif -> "gif"
               | `mp2 -> "mp2"
               | `mp3 -> "mp3"
               | `mp4 -> "mp4"
               | `mpg -> "mpg"
               | `mxf -> "mxf"
               | `oga -> "oga"
               | `ogg -> "ogg"
               | `ts -> "ts"
               | `wav -> "wav"
               | `webm -> "webm") ) ]
        |> add "Audio" audio |> add "Video" video
        |> add "Thumbnails" thumbnails)

    let create ~preset () =
      let payload = Yojson.Safe.to_string preset in
      let%lwt res, _ =
        Service.request ~meth:`POST ~uri:"/2012-09-25/presets" ~payload ()
      in
      let res = Yojson.Safe.from_string res in
      let warning = Yojson.Safe.Util.(to_string (member "Warning" res)) in
      if warning = "" then Lwt.return_none else Lwt.return_some warning

    type preset = {id : string; name : string}

    let list ?page_token () =
      let query =
        match page_token with None -> [] | Some token -> ["PageToken", token]
      in
      let%lwt res, _ =
        Service.request ~meth:`GET ~uri:"/2012-09-25/presets" ~query ()
      in
      let res = Yojson.Safe.from_string res in
      let open Yojson.Safe.Util in
      let presets =
        List.map
          (fun p ->
            { id = to_string @@ member "Id" p
            ; name = to_string @@ member "Name" p })
          (to_list @@ member "Presets" res)
      in
      let next_page_token = to_string_option @@ member "NextPageToken" res in
      Lwt.return (presets, next_page_token)
  end

  module Job = struct
    type 'a parameters = Yojson.Safe.t
    type input_encryption = [`AES_CBC_PKCS7 | `AES_CTR | `AES_GCM]
    type output_encryption = [input_encryption | `AES_GCM | `S3 | `S3_AWS_KMS]

    let encryption ~mode ?key ?key_md5 ?initialization_vector () =
      let a =
        [ ( "Mode"
          , `String
              (match mode with
              | `S3 -> "S3"
              | `S3_AWS_KMS -> "S3_AWS_KMS"
              | `AES_CBC_PKCS7 -> "AES-CBC-PKCS7"
              | `AES_CTR -> "AES-CTR"
              | `AES_GCM -> "AES-GCM") ) ]
      in
      let add f v rem =
        match v with None -> rem | Some v -> (f, `String v) :: rem
      in
      `Assoc
        (a |> add "Key" key |> add "KeyMd5" key_md5
        |> add "InitializationVector" initialization_vector)

    let input ~key ?encryption ?frame_rate ?aspect_ratio ?interlaced ?container
        ()
      =
      let a = ["Key", `String key] in
      let add' f v rem = match v with None -> rem | Some v -> (f, v) :: rem in
      let add f v g rem =
        match v with None -> rem | Some v -> (f, `String (g v)) :: rem
      in
      `Assoc
        (a
        |> add' "Encryption" encryption
        |> add "FrameRate" frame_rate conv_frame_rate
        |> add "AspectRatio" aspect_ratio conv_aspect_ratio
        |> add "Interlaced" interlaced (fun i ->
               match i with
               | `Auto -> "auto"
               | `True -> "true"
               | `False -> "false")
        |> add "Container" container (fun c ->
               match c with
               | `auto -> "auto"
               | `_3gp -> "3gp"
               | `aac -> "aac"
               | `asf -> "asf"
               | `avi -> "avi"
               | `divx -> "divx"
               | `flv -> "flv"
               | `m4a -> "m4a"
               | `mkv -> "mkv"
               | `mov -> "mov"
               | `mp3 -> "mp3"
               | `mp4 -> "mp4"
               | `mpeg -> "mpeg"
               | `mpeg_ps -> "mpeg-ps"
               | `mpeg_ts -> "mpeg-ts"
               | `mxf -> "mxf"
               | `ogg -> "ogg"
               | `vob -> "vob"
               | `wav -> "wav"
               | `webm -> "webm"))

    let output ~key ?encryption ?thumbnail_pattern ~preset_id ?rotate
        ?segment_duration ()
      =
      let a = ["Key", `String key; "PresetId", `String preset_id] in
      let add' f v rem = match v with None -> rem | Some v -> (f, v) :: rem in
      let add f v g rem =
        match v with None -> rem | Some v -> (f, `String (g v)) :: rem
      in
      `Assoc
        (a
        |> add' "Encryption" encryption
        |> add "ThumbnailPattern" thumbnail_pattern (fun x -> x)
        |> add "SegmentDuration" segment_duration string_of_int
        |> add "Rotate" rotate (fun r ->
               match r with
               | `auto -> "auto"
               | `_0 -> "0"
               | `_90 -> "90"
               | `_180 -> "180"
               | `_270 -> "270"))

    let playlist ~format ~name ~output_keys () =
      `Assoc
        [ ( "Format"
          , `String
              (match format with
              | `HLSv3 -> "HLSv3"
              | `HLSv4 -> "HLSv4"
              | `MPEG_DASH -> "MPEG-DASH"
              | `Smooth -> "Smooth") ); "Name", `String name
        ; "OutputKeys", `List (List.map (fun o -> `String o) output_keys) ]

    let create ~input ?output_key_prefix ~outputs ?(playlists = [])
        ?(user_metadata = []) ~pipeline_id ()
      =
      let payload =
        let a =
          [ "Input", input; "Outputs", `List outputs
          ; "Playlists", `List playlists
          ; ( "UserMetadata"
            , `Assoc (List.map (fun (k, v) -> k, `String v) user_metadata) )
          ; "PipelineId", `String pipeline_id ]
        in
        let a =
          match output_key_prefix with
          | Some prefix -> ("OutputKeyPrefix", `String prefix) :: a
          | None -> a
        in
        let p = `Assoc a in
        Yojson.Safe.to_string p
      in
      let%lwt res, _ =
        Service.request ~meth:`POST ~uri:"/2012-09-25/jobs" ~payload ()
      in
      let res = Yojson.Safe.from_string res in
      Lwt.return Yojson.Safe.Util.(to_string (member "Id" (member "Job" res)))

    type input = {key : string}
    type output = {key : string; width : int option; height : int option}

    type t =
      { status : [`Submitted | `Progressing | `Complete | `Canceled | `Error]
      ; input : input
      ; outputs : output list
      ; user_metadata : (string * string) list }

    let read ~id () =
      let%lwt res, _ =
        Service.request ~meth:`GET ~uri:("/2012-09-25/jobs/" ^ id) ()
      in
      let res = Yojson.Safe.from_string res in
      let open Yojson.Safe.Util in
      let job = member "Job" res in
      let status =
        match to_string (member "Status" job) with
        | "Submitted" -> `Submitted
        | "Progressing" -> `Progressing
        | "Complete" -> `Complete
        | "Canceled" -> `Canceled
        | "Error" -> `Error
        | _ -> assert false
      in
      let user_metadata =
        match member "UserMetadata" job with
        | `Assoc l -> List.map (fun (k, v) -> k, to_string v) l
        | `Null -> []
        | _ -> assert false
      in
      Lwt.return
        { status
        ; input = {key = to_string (member "Key" (member "Input" job))}
        ; outputs =
            List.map
              (fun o ->
                { key = to_string (member "Key" o)
                ; width = to_int_option (member "Width" o)
                ; height = to_int_option (member "Height" o) })
              (to_list (member "Outputs" job))
        ; user_metadata }
  end
end
