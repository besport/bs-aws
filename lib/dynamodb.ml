module Make (Conf : Service.CONF) = struct
  let hostname region =
    Format.sprintf "dynamodb.%s.amazonaws.com" (Common.Region.to_string region)

  module Service =
    Service.Make
      (Conf)
      (struct
        let name = "dynamodb"
        and host = hostname Conf.region
      end)
end
