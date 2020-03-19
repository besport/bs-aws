module Make (Conf : Service.CONF) : sig
  module Service : Service.S

  type stat = SampleCount | Average | Sum | Minimum | Maximum

  val get_metric_statistics
    :  namespace:string
    -> metric_name:string
    -> ?dimensions:(string * string) list
    -> statistics:stat list
    -> start_time:string
    -> end_time:string
    -> period:int
    -> (string * string) list list Lwt.t
end
