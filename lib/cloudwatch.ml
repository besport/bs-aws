let endpoint region =
  Printf.sprintf "monitoring.%s.amazonaws.com" (Common.Region.to_string region)

let action act = ["Version", "2010-08-01"; "Action", act]

module Make (Conf : Service.CONF) = struct
  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "monitoring"
        and host = endpoint Conf.region
      end)

  type stat = SampleCount | Average | Sum | Minimum | Maximum

  let show_stat = function
    | SampleCount -> "SampleCount"
    | Average -> "Average"
    | Sum -> "Sum"
    | Minimum -> "Minimum"
    | Maximum -> "Maximum"

  let get_metric_statistics ~namespace ~metric_name ?(dimensions = [])
      ~statistics ~start_time ~end_time ~period
    =
    assert (List.mem period [1; 5; 10; 30] || period mod 60 = 0);
    let statistic_to_json i s =
      let idx = string_of_int (i + 1) in
      "Statistics.member." ^ idx, show_stat s
    in
    let dimension_to_json i (d, v) =
      let idx = string_of_int (i + 1) in
      [ "Dimensions.member." ^ idx ^ ".Name", d
      ; "Dimensions.member." ^ idx ^ ".Value", v ]
    in
    let query =
      [ "Namespace", namespace; "MetricName", metric_name
      ; "StartTime", start_time; "EndTime", end_time
      ; "Period", string_of_int period ]
      @ action "GetMetricStatistics"
      @ List.flatten (List.mapi dimension_to_json dimensions)
      @ List.mapi statistic_to_json statistics
    in
    let%lwt res, _ = Service.request ~meth:`POST ~uri:"/" ~query () in
    let tree =
      let el ((_, tag), _) children = `Tag (tag, children)
      and data d = `Data d in
      snd
      @@ Xmlm.input_doc_tree ~el ~data
      @@ Xmlm.make_input (`String (0, res))
    in
    let rec filter_map f l =
      match l with
      | [] -> []
      | x :: r -> (
        match f x with None -> filter_map f r | Some v -> v :: filter_map f r)
    in
    let parse_response =
      let field = function `Tag (t, [`Data d]) -> Some (t, d) | _ -> None in
      let member = function
        | `Tag ("member", xs) -> Some (filter_map field xs)
        | _ -> None
      in
      let datapoints = function
        | `Tag ("Datapoints", xs) -> Some (filter_map member xs)
        | _ -> None
      in
      let result = function
        | `Tag ("GetMetricStatisticsResult", xs) ->
            Some (List.flatten @@ filter_map datapoints xs)
        | _ -> None
      in
      function
      | `Tag ("GetMetricStatisticsResponse", xs) ->
          List.flatten @@ filter_map result xs
      | _ -> assert false
    in
    Lwt.return @@ parse_response tree
end
