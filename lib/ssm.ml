module Make (Conf : Service.CONF) = struct
  let endpoint region =
    Format.sprintf "ssm.%s.amazonaws.com" (Common.Region.to_string region)

  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "ssm"
        and host = endpoint Conf.region
      end)

  type parameter = {
      name : string
    ; typ : [ `String | `StringList | `SecureString ]
    ; value : string
    ; version : int
    ; last_modified_date : float
    ; arn : string
    ; data_type : string
  }

  let parse_type t =
    match t with
    | "String" -> `String
    | "StringList" -> `StringList
    | "SecureString" -> `SecureString
    | _ -> assert false

  let parse_parameter p =
    let open Yojson.Safe.Util in
    {
      name = to_string (member "Name" p)
    ; typ = parse_type (to_string (member "Type" p))
    ; value = to_string (member "Value" p)
    ; version = to_int (member "Version" p)
    ; last_modified_date = to_float (member "LastModifiedDate" p)
    ; arn = to_string (member "ARN" p)
    ; data_type = to_string (member "DataType" p)
    }

  let get_parameters_by_path ?next_token ?with_decryption ~path () =
    let uri = "/" in
    let add k v l = (k, v) :: l in
    let opt_add k f v l = match v with None -> l | Some v -> add k (f v) l in
    let payload =
      []
      |> add "Path" (`String path)
      |> opt_add "WithDecryption" (fun b -> `Bool b) with_decryption
      |> opt_add "NextToken" (fun s -> `String s) next_token
    in
    let payload = Yojson.Safe.to_string (`Assoc payload) in
    let headers = [ "X-Amz-Target", "AmazonSSM.GetParametersByPath" ] in
    let%lwt res, _ = Service.request ~meth:`POST ~uri ~headers ~payload () in
    let res = Yojson.Safe.from_string res in
    let open Yojson.Safe.Util in
    Lwt.return
      ( List.map parse_parameter (to_list (member "Parameters" res))
      , to_string_option (member "NextToken" res) )
end
