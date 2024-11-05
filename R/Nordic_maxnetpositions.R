#' Nordic - Nordic Net Positions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the minimum and maximum net positions for all Nordic bidding zones
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @export
#'
#' @examples JAOPuTo_Nordic_maxnetpositions(as.POSIXct("2024-10-30 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-10-30 23:00", "Europe/Brussels"))
JAOPuTo_Nordic_maxnetpositions <- function(StartDateTime,
                                           EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/maxNetPos",
                       query = list(
                         FromUtc = format(lubridate::with_tz(StartDateTime, "UTC"),
                                          "%Y-%m-%dT%H:%M:%S.0000Z"),
                         ToUtc = format(lubridate::with_tz(EndDateTime + lubridate::hours(1), "UTC"),
                                        "%Y-%m-%dT%H:%M:%S.0000Z")
                       )) %>%
    httr::content(as = "text")%>%
    jsonlite::fromJSON()

  df <- API_GET$data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(DateTime = lubridate::with_tz(as.POSIXct(dateTimeUtc, tz = "UTC",
                                                           format = "%Y-%m-%dT%H:%M:%SZ"),
                                                "Europe/Brussels")) %>%
    dplyr::select(-id, -dateTimeUtc, -lastModifiedOn) %>%
    tidyr::pivot_longer(-DateTime,
                        names_to = "Variable",
                        values_to = "MaxNetPosition") %>%
    dplyr::mutate(BiddingZoneAbb = substr(Variable, 5, length(Variable)),
                  Direction = dplyr::case_match(substr(Variable, 1, 3), "min" ~ "Import", "max" ~ "Export"),
                  BiddingZone = dplyr::case_match(BiddingZoneAbb,
                                                  "DK1" ~ "Denmark West",
                                                  "DK2" ~ "Denmark East",
                                                  "FI" ~ "Finland",
                                                  "NO1" ~ "Norway 1",
                                                  "NO2" ~ "Norway 2",
                                                  "NO3" ~ "Norway 3",
                                                  "NO4" ~ "Norway 5",
                                                  "NO5" ~ "Norway 5",
                                                  "SE1" ~ "Sweden 1",
                                                  "SE2" ~ "Sweden 2",
                                                  "SE3" ~ "Sweden 3",
                                                  "SE4" ~ "Sweden 4",
                                                  "NO2_SK" ~ "Norway 2 – Skagerak",
                                                  "DK1_SK" ~ "Denmark West - Skagerak",
                                                  "DK1_SB" ~ "Denmark West - Storebaelt",
                                                  "DK2_SB" ~ "Denmark East - Storebaelt",
                                                  "SE3_FS" ~ "Sweden 3 - Fennoskan",
                                                  "DK1_KS" ~ "Denmark West - Kontiskan",
                                                  "SE3_KS" ~ "Sweden 3 - Kontiskan",
                                                  "FI_FS" ~ "Finland - Fennoskan",
                                                  "SE4_SP" ~ "Sweden 4 - SwePol",
                                                  "SE4_NB" ~ "Sweden 4 - Nordbelt",
                                                  "SE4_BC" ~ "Sweden 4 - Baltic Cable",
                                                  "FI_EL" ~ "Finland - Estlink",
                                                  "DK1_DE" ~ "Denmark West - Germany",
                                                  "DK2_KO" ~ "Denmark East - Kontek",
                                                  "DK1_CO" ~ "Denmark West - COBRA Cable",
                                                  "NO2_ND" ~ "Norway 2 - NordNed",
                                                  "NO2_NK" ~ "Norway 2 - NordLink",
                                                  "SE3_SWL" ~ "Sweden 3 - SWL",
                                                  "SE4_SWL" ~ "Sweden 4 - SWL")) %>%
    dplyr::select(DateTime, BiddingZoneAbb, BiddingZone, Direction, MaxNetPosition) %>%
    return()

}
