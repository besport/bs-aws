module Make (Conf : Service.CONF) = struct
  let endpoint region =
    Format.sprintf "sqs.%s.amazonaws.com" (Common.Region.to_string region)

  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "sqs"
        and host = endpoint Conf.region
      end)

  let decode_response res =
    let i = Xmlm.make_input (`String (0, res)) in
    ignore (Xmlm.input i);
    (* DTD *)
    ignore (Xmlm.input i);
    (* *Response *)
    ignore (Xmlm.input i);
    (* *Result *)
    i

  let init_params act k v = ["Version", "2012-11-05"; "Action", act; k, v]

  let create_queue ?delay_seconds ?maximum_message_size
      ?message_retention_period ?policy ?receive_message_wait_time_seconds
      ?redrive_policy ?visibility_timeout ?fifo_queue
      ?content_based_deduplication ~queue_name ()
    =
    let attributes =
      let open Base.Param in
      []
      |> custom "Policy" Yojson.Safe.to_string policy
      |> int "VisibilityTimeout" visibility_timeout
      |> int "MaximumMessageSize" maximum_message_size
      |> int "MessageRetentionPeriod" message_retention_period
      |> int "DelaySeconds" delay_seconds
      |> int "ReceiveMessageWaitTimeSeconds" receive_message_wait_time_seconds
      |> custom "RedrivePolicy" Yojson.Safe.to_string redrive_policy
      |> bool "FifoQueue" fifo_queue
      |> bool "ContentBasedDeduplication" content_based_deduplication
      |> List.mapi (fun i (k, v) ->
             [ Printf.sprintf "Attribute.%d.Name" (i + 1), k
             ; Printf.sprintf "Attribute.%d.Value" (i + 1), v ])
      |> List.flatten
    in
    let parameters =
      init_params "CreateQueue" "QueueName" queue_name @ attributes
    in
    let%lwt res, _ =
      Service.request ~meth:`POST ~uri:"/" ~query:parameters ()
    in
    let i = decode_response res in
    ignore (Xmlm.input i);
    (* QueueUrl *)
    match Xmlm.input i with `Data s -> Lwt.return s | _ -> assert false

  let set_queue_attributes ?delay_seconds ?maximum_message_size
      ?message_retention_period ?policy ?receive_message_wait_time_seconds
      ?redrive_policy ?visibility_timeout ?content_based_deduplication
      ~queue_url ()
    =
    let attributes =
      let open Base.Param in
      []
      |> custom "Policy" Yojson.Safe.to_string policy
      |> int "VisibilityTimeout" visibility_timeout
      |> int "MaximumMessageSize" maximum_message_size
      |> int "MessageRetentionPeriod" message_retention_period
      |> int "DelaySeconds" delay_seconds
      |> int "ReceiveMessageWaitTimeSeconds" receive_message_wait_time_seconds
      |> custom "RedrivePolicy" Yojson.Safe.to_string redrive_policy
      |> bool "ContentBasedDeduplication" content_based_deduplication
      |> List.mapi (fun i (k, v) ->
             [ Printf.sprintf "Attribute.%d.Name" (i + 1), k
             ; Printf.sprintf "Attribute.%d.Value" (i + 1), v ])
      |> List.flatten
    in
    let parameters =
      init_params "SetQueueAttributes" "QueueUrl" queue_url @ attributes
    in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query:parameters () in
    Lwt.return ()

  module Attribute = struct
    type set = (string * string) list
    type t = string

    let attribute name _ = name
    let all = "All"

    let approximate_number_of_messages =
      attribute "ApproximateNumberOfMessages" int_of_string

    let approximate_number_of_messages_delayed =
      attribute "ApproximateNumberOfMessagesDelayed" int_of_string

    let approximate_number_of_messages_not_visible =
      attribute "ApproximateNumberOfMessagesNotVisible" int_of_string

    let created_timestamp = attribute "CreatedTimestamp" float_of_string
    let delay_seconds = attribute "DelaySeconds" int_of_string

    let last_modified_timestamp =
      attribute "LastModifiedTimestamp" float_of_string

    let maximum_message_size = attribute "MaximumMessageSize" int_of_string

    let message_retention_period =
      attribute "MessageRetentionPeriod" int_of_string

    let policy = attribute "Policy" Yojson.Safe.from_string
    let queue_arn = attribute "QueueArn" (fun x -> x)

    let receive_message_wait_time_seconds =
      attribute "ReceiveMessageWaitTimeSeconds" int_of_string

    let redrive_policy = attribute "RedrivePolicy" Yojson.Safe.from_string
    let visibility_timeout = attribute "VisibilityTimeout" int_of_string
    let fifo_queue = attribute "FifoQueue" bool_of_string

    let content_based_deduplication =
      attribute "ContentBasedDeduplication" bool_of_string

    module Get = struct
      type 'a t = set -> 'a

      let attribute name extract l = extract (List.assoc name l)

      let approximate_number_of_messages =
        attribute "ApproximateNumberOfMessages" int_of_string

      let approximate_number_of_messages_delayed =
        attribute "ApproximateNumberOfMessagesDelayed" int_of_string

      let approximate_number_of_messages_not_visible =
        attribute "ApproximateNumberOfMessagesNotVisible" int_of_string

      let created_timestamp = attribute "CreatedTimestamp" float_of_string
      let delay_seconds = attribute "DelaySeconds" int_of_string

      let last_modified_timestamp =
        attribute "LastModifiedTimestamp" float_of_string

      let maximum_message_size = attribute "MaximumMessageSize" int_of_string

      let message_retention_period =
        attribute "MessageRetentionPeriod" int_of_string

      let policy = attribute "Policy" Yojson.Safe.from_string
      let queue_arn = attribute "QueueArn" (fun x -> x)

      let receive_message_wait_time_seconds =
        attribute "ReceiveMessageWaitTimeSeconds" int_of_string

      let redrive_policy = attribute "RedrivePolicy" Yojson.Safe.from_string
      let visibility_timeout = attribute "VisibilityTimeout" int_of_string
      let fifo_queue = attribute "FifoQueue" bool_of_string

      let content_based_deduplication =
        attribute "ContentBasedDeduplication" bool_of_string
    end
  end

  let get_queue_attributes ~attributes ~queue_url () =
    let parameters =
      init_params "GetQueueAttributes" "QueueUrl" queue_url
      @ List.mapi
          (fun i nm -> Printf.sprintf "AttributeName.%d" (i + 1), nm)
          attributes
    in
    let%lwt res, _ =
      Service.request ~meth:`POST ~uri:"/" ~query:parameters ()
    in
    Lwt.return
      (res |> decode_response
      |> Base.Xml.(repeat (element record))
      |> List.map (fun (_, r) -> List.assoc "Name" r, List.assoc "Value" r))

  let delete_queue ~queue_url () =
    let parameters = init_params "DeleteQueue" "QueueUrl" queue_url in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query:parameters () in
    Lwt.return ()

  let send_message ~queue_url ~message_body () =
    let parameters =
      ("MessageBody", message_body)
      :: init_params "SendMessage" "QueueUrl" queue_url
    in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query:parameters () in
    Lwt.return ()

  type message = {message_id : string; receipt_handle : string; body : string}

  let receive_message ?max_number_of_messages ?visibility_timeout
      ?wait_time_seconds ~queue_url ()
    =
    let parameters =
      let open Base.Param in
      init_params "ReceiveMessage" "QueueUrl" queue_url
      |> int "MaxNumberOfMessages" max_number_of_messages
      |> int "VisibilityTimeout" visibility_timeout
      |> int "WaitTimeSeconds" wait_time_seconds
    in
    let%lwt res, _ =
      Service.request ~meth:`POST ~uri:"/" ~query:parameters ()
    in
    Lwt.return
      (res |> decode_response
      |> Base.Xml.(repeat (element record))
      |> List.map (fun (_, r) ->
             { message_id = List.assoc "MessageId" r
             ; receipt_handle = List.assoc "ReceiptHandle" r
             ; body = List.assoc "Body" r }))

  let delete_message ~queue_url ~receipt_handle () =
    let parameters =
      ("ReceiptHandle", receipt_handle)
      :: init_params "DeleteMessage" "QueueUrl" queue_url
    in
    let%lwt _ = Service.request ~meth:`POST ~uri:"/" ~query:parameters () in
    Lwt.return ()
end
