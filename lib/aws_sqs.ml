
let endpoint region =
  Format.sprintf "sqs.%s.amazonaws.com" (Aws_common.Region.to_string region)

let decode_response res =
  let i = Xmlm.make_input (`String (0, res)) in
  ignore (Xmlm.input i); (* DTD *)
  ignore (Xmlm.input i); (* *Response *)
  ignore (Xmlm.input i); (* *Result *)
  i

module Xml = struct

  let text i =
    match Xmlm.peek i with
    | `Data d -> ignore (Xmlm.input i); d
    | `El_end -> ""
    | _       -> assert false


  let repeat f i =
    let rec repeat_rec f i acc =
      match Xmlm.peek i with
        `El_end -> List.rev acc
      | _       -> repeat_rec f i (f i :: acc)
    in
    repeat_rec f i []

  let element_end i =
    match Xmlm.input i with
    | `El_end -> ()
    | _       -> assert false

  let element f i =
    match Xmlm.input i with
    | `El_start ((_, nm), _) ->
      let x = f i in
      element_end i;
      (nm, x)
    | _ ->
      assert false

  let field i = element text i

  let record = repeat field

end

let p_string k v rem =
  match v with
    Some v -> (k, v) :: rem
  | _      -> rem

let p_int k v rem =
  match v with
    Some v -> (k, string_of_int v) :: rem
  | _      -> rem

let p_bool k v rem =
  match v with
    Some true -> (k, "true") :: rem
  | _         -> rem

let create_queue ~credentials ~region
    ?delay_seconds ?maximum_message_size ?message_retention_period
    ?policy ?receive_message_wait_time_seconds ?redrive_policy
    ?visibility_timeout ?fifo_queue ?content_based_deduplication
    ~queue_name () =
  let parameters =
    ["Action", "CreateQueue";
     "QueueName", queue_name]
    |> p_string "Policy" policy
    |> p_int "VisibilityTimeout" visibility_timeout
    |> p_int "MaximumMessageSize" maximum_message_size
    |> p_int "MessageRetentionPeriod" message_retention_period
    |> p_int "DelaySeconds" delay_seconds
    |> p_int "ReceiveMessageWaitTimeSeconds" receive_message_wait_time_seconds
    |> p_string "RedrivePolicy" redrive_policy
    |> p_bool "FifoQueue" fifo_queue
    |> p_string "ContentBasedDeduplication" content_based_deduplication
  in
  let%lwt res =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`GET ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  let i = decode_response res in
  ignore (Xmlm.input i); (* QueueUrl *)
  match Xmlm.input i with
    `Data s -> Lwt.return s
  | _       -> assert false

let delete_queue ~credentials ~region ~queue_url () =
  let parameters =
    ["Action", "DeleteQueue";
     "QueueUrl", queue_url]
  in
  let%lwt _ =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`GET ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return ()

let send_message ~credentials ~region ~queue_url ~message_body () =
  let parameters =
    ["Action", "SendMessage";
     "QueueUrl", queue_url;
     "MessageBody", message_body]
  in
  let%lwt _ =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`GET ~host:(endpoint region)
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
    ["Action", "ReceiveMessage";
     "QueueUrl", queue_url]
    |> p_int "MaxNumberOfMessages" max_number_of_messages
    |> p_int "VisibilityTimeout" visibility_timeout
    |> p_int "WaitTimeSeconds" wait_time_seconds
  in
  let%lwt res =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`GET ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return
    (res |> decode_response |> Xml.(repeat (element record))
     |> List.map
          (fun (_, r) ->
             { message_id = List.assoc "MessageId" r;
               receipt_handle = List.assoc "ReceiptHandle" r;
               body = List.assoc "Body" r }))

let delete_message ~credentials ~region ~queue_url ~receipt_handle () =
  let parameters =
    ["Action", "DeleteMessage";
     "QueueUrl", queue_url;
     "ReceiptHandle", receipt_handle]
  in
  let%lwt _ =
    Aws_request.perform
      ~credentials ~service:"sqs" ~region ~meth:`GET ~host:(endpoint region)
      ~uri:"/" ~query:parameters ()
  in
  Lwt.return ()
