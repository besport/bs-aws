
let endpoint region =
  Format.sprintf "sqs.%s.amazonaws.com" (Aws_common.Region.to_string region)

let decode_response res =
  let i = Xmlm.make_input (`String (0, res)) in
  ignore (Xmlm.input i); (* DTD *)
  ignore (Xmlm.input i); (* *Response *)
  ignore (Xmlm.input i); (* *Result *)
  i

let init_params act k v = ["Version", "2012-11-05"; "Action", act; k, v]

let create_queue ~credentials ~region
    ?delay_seconds ?maximum_message_size ?message_retention_period
    ?policy ?receive_message_wait_time_seconds ?redrive_policy
    ?visibility_timeout ?fifo_queue ?content_based_deduplication
    ~queue_name () =
  let parameters =
    let open Aws_base.Param in
    init_params "CreateQueue" "QueueName" queue_name
    |> string "Policy" policy
    |> int "VisibilityTimeout" visibility_timeout
    |> int "MaximumMessageSize" maximum_message_size
    |> int "MessageRetentionPeriod" message_retention_period
    |> int "DelaySeconds" delay_seconds
    |> int "ReceiveMessageWaitTimeSeconds" receive_message_wait_time_seconds
    |> string "RedrivePolicy" redrive_policy
    |> bool "FifoQueue" fifo_queue
    |> string "ContentBasedDeduplication" content_based_deduplication
  in
  let%lwt res =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`POST ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  let i = decode_response res in
  ignore (Xmlm.input i); (* QueueUrl *)
  match Xmlm.input i with
    `Data s -> Lwt.return s
  | _       -> assert false

let delete_queue ~credentials ~region ~queue_url () =
  let parameters = init_params "DeleteQueue" "QueueUrl" queue_url in
  let%lwt _ =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`POST ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return ()

let send_message ~credentials ~region ~queue_url ~message_body () =
  let parameters =
    ("MessageBody", message_body) ::
    init_params "SendMessage" "QueueUrl" queue_url
  in
  let%lwt _ =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`POST ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return ()

type message = {
  message_id : string ;
  receipt_handle : string ;
  body : string
}

let receive_message ~credentials ~region
    ?max_number_of_messages ?visibility_timeout ?wait_time_seconds
    ~queue_url () =
  let parameters =
    let open Aws_base.Param in
    init_params "ReceiveMessage" "QueueUrl" queue_url
    |> int "MaxNumberOfMessages" max_number_of_messages
    |> int "VisibilityTimeout" visibility_timeout
    |> int "WaitTimeSeconds" wait_time_seconds
  in
  let%lwt res =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`POST ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return
    (res |> decode_response |> Aws_base.Xml.(repeat (element record))
     |> List.map
          (fun (_, r) ->
             { message_id = List.assoc "MessageId" r;
               receipt_handle = List.assoc "ReceiptHandle" r;
               body = List.assoc "Body" r }))

let delete_message ~credentials ~region ~queue_url ~receipt_handle () =
  let parameters =
    ("ReceiptHandle", receipt_handle) ::
    init_params "DeleteMessage" "QueueUrl" queue_url
  in
  let%lwt _ =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`POST ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return ()
