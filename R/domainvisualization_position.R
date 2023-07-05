JAOPuTo_domainvisualization_positions <- function(DateTime,
                                                  BiddingZone1,
                                                  BiddingZone2,
                                                  Reference = "MCP") {

  # bidding zone abbreviations
  BiddingZoneAbb1 = CoreBZ$BiddingZoneAbb[CoreBZ$BiddingZone == BiddingZone1]
  BiddingZoneAbb2 = CoreBZ$BiddingZoneAbb[CoreBZ$BiddingZone == BiddingZone2]


  # define reference positions for zones (where to slice)
  if (Reference == "MCP") {

    df_slice <- JAOPuTo::JAOPuTo_netpositions(DateTime, DateTime + lubridate::hours(1)) %>%
      dplyr::mutate(NetPosition = dplyr::case_when(BiddingZone %in% c(BiddingZone1, BiddingZone2) ~ 0,
                                                   TRUE ~ NetPosition))

  } else if (Reference == "Zero-balanced") {

    df_slice$NetPosition <- 0

  }

  # download presolved final domains
  df_finaldomain <- JAOPuTo::JAOPuTo_finaldomain(DateTime, DateTime + lubridate::hours(1)) %>%
    dplyr::filter(cneEic != "NA")

  df_finaldomain <- df_finaldomain %>%
    mutate(Intercept = (df_finaldomain$ram - df_finaldomain[["ptdf_ALBE"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "ALBE"]
                        - df_finaldomain[["ptdf_ALDE"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "ALDE"]
                        - df_finaldomain[["ptdf_AT"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "AT"]
                        - df_finaldomain[["ptdf_BE"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "BE"]
                        - df_finaldomain[["ptdf_HR"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "HR"]
                        - df_finaldomain[["ptdf_CZ"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "CZ"]
                        - df_finaldomain[["ptdf_DE"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "DE"]
                        - df_finaldomain[["ptdf_FR"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "FR"]
                        - df_finaldomain[["ptdf_HU"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "HU"]
                        - df_finaldomain[["ptdf_NL"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "NL"]
                        - df_finaldomain[["ptdf_PL"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "PL"]
                        - df_finaldomain[["ptdf_RO"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "RO"]
                        - df_finaldomain[["ptdf_SK"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "SK"]
                        - df_finaldomain[["ptdf_SI"]] * df_slice$NetPosition[df_slice$BiddingZoneAbb == "SI"])
           / df_finaldomain[[paste0("ptdf_", BiddingZoneAbb2)]],
           Slope = - df_finaldomain[[paste0("ptdf_", BiddingZoneAbb1)]] /
             df_finaldomain[[paste0("ptdf_", BiddingZoneAbb2)]]) %>%
    mutate(CNEC = paste0(TSO, ": ", cneName, " | ", contName, " - ", direction),
           Type = "Presolved",
           Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
           RAM = ram / fmax) %>%
    select(CNEC, TSO, Location, Type, Intercept, Slope, RAM) %>%
    filter(!Intercept %in% c("Inf", "NaN", NA))

  # download Core net positions
  df_netpositions <- JAOPuTo::JAOPuTo_netpositions(DateTime, DateTime + lubridate::hours(1))

  ggplot() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_abline(data = df_finaldomain,
                mapping = aes(intercept = Intercept, slope = Slope, group = CNEC),
                linewidth = .25, colour = "grey") +
    geom_point(data = df_netpositions,
               mapping = aes(x = NetPosition[BiddingZone == BiddingZone1],
                             y = NetPosition[BiddingZone == BiddingZone2])) +
    coord_cartesian(xlim = c(-10000, 10000),
                    ylim = c(-10000, 10000)) +
    theme_void()



}
