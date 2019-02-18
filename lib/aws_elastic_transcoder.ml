
let (>>=) = Lwt.bind

let endpoint (region : Aws_common.Region.t) =
  Printf.sprintf "elastictranscoder.%s.amazonaws.com"
    (Aws_common.Region.to_string region)

module Job = struct
  type 'a parameters = Yojson.Safe.t

  type input_encryption = [`AES_CBC_PKCS7 | `AES_CTR | `AES_GCM]
  type output_encryption = [input_encryption | `AES_GCM | `S3 | `S3_AWS_KMS ]

  let encryption ~mode ?key ?key_md5 ?initialization_vector () =
    let a =
      ["Mode",
       `String
         (match mode with
            `S3 -> "S3"
          | `S3_AWS_KMS -> "S3_AWS_KMS"
          | `AES_CBC_PKCS7 -> "AES-CBC-PKCS7"
          | `AES_CTR -> "AES-CTR"
          | `AES_GCM -> "AES-GCM")]
    in
    let add f v rem =
      match v with None -> rem | Some v -> (f, `String v) :: rem in
    `Assoc (a |> add "Key" key |> add "KeyMd5" key_md5
              |> add "InitializationVector" initialization_vector)

  let input
        ~key ?encryption ?frame_rate ?aspect_ratio ?interlaced ?container () =
    let a = ["Key", `String key] in
    let add' f v rem = match v with None -> rem | Some v -> (f, v) :: rem in
    let add f v g rem =
      match v with None -> rem | Some v -> (f, `String (g v)) :: rem in
    `Assoc
      (a |> add' "Encryption" encryption
         |> add "FrameRate" frame_rate
              (fun r ->
                 match r with
                   `auto   -> "auto"
                 | `_10    -> "10"
                 | `_15    -> "15"
                 | `_23_97 -> "23.97"
                 | `_24    -> "24"
                 | `_25    -> "25"
                 | `_29_97 -> "29.97"
                 | `_30    -> "30"
                 | `_50    -> "50"
                 | `_60    -> "60")
         |> add "AspectRatio" aspect_ratio
              (fun r ->
                 match r with
                   `auto  -> "auto"
                 | `_1_1  -> "1:1"
                 | `_4_3  -> "4:3"
                 | `_3_2  -> "3:2"
                 | `_16_9 -> "16:9")
         |> add "Interlaced" interlaced
              (fun i ->
                 match i with
                   `Auto  -> "auto"
                 | `True  -> "true"
                 | `False -> "false")
         |> add "Container" container
           (fun c ->
              match c with
                `auto    -> "auto"
              | `_3gp    -> "3gp"
              | `aac     -> "aac"
              | `asf     -> "asf"
              | `avi     -> "avi"
              | `divx    -> "divx"
              | `flv     -> "flv"
              | `m4a     -> "m4a"
              | `mkv     -> "mkv"
              | `mov     -> "mov"
              | `mp3     -> "mp3"
              | `mp4     -> "mp4"
              | `mpeg    -> "mpeg"
              | `mpeg_ps -> "mpeg-ps"
              | `mpeg_ts -> "mpeg-ts"
              | `mxf     -> "mxf"
              | `ogg     -> "ogg"
              | `vob     -> "vob"
              | `wav     -> "wav"
              | `webm    -> "webm"))

  let output ~key ?encryption ?thumbnail_pattern ~preset_id () =
    let a = ["Key", `String key; "PresetId", `String preset_id] in
    let add' f v rem = match v with None -> rem | Some v -> (f, v) :: rem in
    let add f v rem =
      match v with None -> rem | Some v -> (f, `String v) :: rem in
    `Assoc (a |> add' "Encryption" encryption
              |> add "ThumbnailPattern" thumbnail_pattern)

  let create
        ~credentials ~region ~input ?output_key_prefix ~outputs
        ~pipeline_id () =
    let payload =
      let a =
        ["Input", input;
         "Outputs", `List outputs; "PipelineId", `String pipeline_id] in
      let a =
        match output_key_prefix with
          Some prefix -> ("OutputKeyPrefix", `String prefix) :: a
        | None        -> a
      in
      let p = `Assoc a in
      Yojson.Safe.to_string p
    in
    Aws_request.perform ~credentials ~service:"elastictranscoder" ~region
      ~meth:`POST ~host:(endpoint region) ~uri:"/2012-09-25/jobs" ~payload ()
      >>= fun res ->
    let res = Yojson.Safe.from_string res in
    Lwt.return Yojson.Safe.Util.(to_string (member "Id" (member "Job" res)))

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

  let read ~credentials ~region ~id () =
    Aws_request.perform ~credentials ~service:"elastictranscoder" ~region
      ~meth:`GET ~host:(endpoint region) ~uri:("/2012-09-25/jobs/" ^ id) ()
    >>= fun res ->
  let res = Yojson.Safe.from_string res in
  let open Yojson.Safe.Util in
  let job = member "Job" res in
  let status =
    match to_string (member "Status" job) with
      "Submitted"   -> `Submitted
    | "Progressing" -> `Progressing
    | "Complete"    -> `Complete
    | "Canceled"    -> `Canceled
    | "Error"       -> `Error
    | _             -> assert false
  in
  Lwt.return
    { status;
      input = {key = to_string (member "Key" (member "Input" job))};
      outputs =
        List.map
          (fun o ->
             { key = to_string (member "Key" o);
               width = to_int_option (member "Width" o);
               height = to_int_option (member "Height" o) })
          (to_list (member "Outputs" job)) }
end
