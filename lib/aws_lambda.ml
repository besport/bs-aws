let (>>=) = Lwt.bind

let client_context = `Assoc [
	("client.installation_id", `String "undefined1");
	("client.app_version_code", `String "undefined2");
	("client.app_version_name", `String "undefined3");
	("client.app_package_name", `String "com.besport.local");
	("client.app_title", `String "BeSport");
	("env.platform_version", `String "undefined4");
	("env.platform", `String "undefined5");
	("env.make", `String "undefined6");
	("env.model", `String "undefined7");
	("env.locale", `String "undefined8")
]

let invoke ~credentials ~region ~function_name ?qualifier ?payload () =

  let qualifier_string = match qualifier with
  | None -> ""
  | Some s -> Printf.sprintf "?Qualifier=%s" s
  in
  let uri = Printf.sprintf "/2015-03-31/functions/%s/invocations%s"
    function_name qualifier_string in

  let host = Printf.sprintf "lambda.%s.amazonaws.com"
    (Aws_common.Region.to_string region) in

  let headers = [
    ("x-amz-invocation-type", "RequestResponse");
    (*("x-amz-log-type", LogType);*) (*optional*)
    ("x-amz-client-context", B64.encode @@ Yojson.Safe.to_string client_context)
  ] in

  let payload = match payload with
  | None -> None
  | Some payload -> Some (Yojson.Safe.to_string payload)
  in

  Aws_request.perform ~credentials ~service:"lambda" ~region ~meth:`POST
                      ~host ~uri ~headers ?payload ()
