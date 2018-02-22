
let endpoint region =
  Format.sprintf "rds.%s.amazonaws.com" (Aws_common.Region.to_string region)

let decode_response res =
  let i = Xmlm.make_input (`String (0, res)) in
  ignore (Xmlm.input i); (* DTD *)
  ignore (Xmlm.input i); (* *Response *)
  ignore (Xmlm.input i); (* *Result *)
  i

let init_params act = ["Version", "2014-10-31"; "Action", act]

type endpoint =
  { address : string;
    port : int;
    hosted_zone_id : string }

type status_info =
  { status_type : string;
    normal : bool;
    status : string;
    message : string option }

type db_instance =
  { endpoint : endpoint;
    status_infos : status_info list;
    read_replica_db_instance_identifiers : string list }

let parse_endpoint e =
  let open Ezxmlm in
  { address = data_to_string (member "Address" e);
    port = int_of_string (data_to_string (member "Port" e));
    hosted_zone_id = data_to_string (member "HostedZoneId" e) }

let parse_status_info i =
  let open Ezxmlm in
  { status_type = data_to_string (member "StatusType" i);
    normal = data_to_string (member "Normal" i) = "true";
    status = data_to_string (member "Status" i);
    message =
      if has_member "Message" i then
        Some (data_to_string (member "Message" i))
      else
        None }

let describe_db_instances ~credentials ~region ?db_instance_identifier () =
  let query =
    init_params "DescribeDBInstances"
    |> Aws_base.Param.string "DBInstanceIdentifier" db_instance_identifier
  in
  let%lwt res =
    Aws_request.perform
      ~credentials ~service:"rds" ~region ~meth:`POST ~host:(endpoint region)
      ~uri:"/" ~query ()
  in
  let open Ezxmlm in
  let (_, res) = from_string res in
  let l =
    res
    |> member "DescribeDBInstancesResponse"
    |> member "DescribeDBInstancesResult"
    |> member "DBInstances"
    |> members "DBInstance"
    |> List.map
      (fun i ->
         { endpoint = parse_endpoint (member "Endpoint" i);
           status_infos =
             if has_member "StatusInfos" i then
               List.map parse_status_info
                 (members "DBInstanceStatusInfo"
                    (member "StatusInfos" i))
             else
               [];
           read_replica_db_instance_identifiers =
             List.map data_to_string
               (members "ReadReplicaDBInstanceIdentifier"
                  (member "ReadReplicaDBInstanceIdentifiers" i)) })
  in
  Lwt.return l
