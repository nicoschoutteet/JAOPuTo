#' Core - ID1 - Net Positions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the Core net positions for all Core bidding zones
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @export
#'
#' @examples JAOPuTo_CoreID_ID1_netpositions(as.POSIXct("2024-07-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-07-01 23:00", "Europe/Brussels"))
JAOPuTo_CoreID_ID1_netpositions <- function(StartDateTime,
                                            EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/coreid/api/data/ID1_netPos",
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
    dplyr::select(-id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime,
                        names_to = "Variable",
                        values_to = "NetPosition") %>%
    dplyr::mutate(BiddingZoneAbb = substr(Variable, 5, length(Variable)),
                  BiddingZone = dplyr::case_match(BiddingZoneAbb,
                                                  "ALBE" ~ "ALEGrO Belgium",
                                                  "ALDE" ~ "ALEGrO Germany",
                                                  "AT" ~ "Austria",
                                                  "BE" ~ "Belgium",
                                                  "CZ" ~ "Czech Republic",
                                                  "DE" ~ "Germany/Luxembourg",
                                                  "FR" ~ "France",
                                                  "HR" ~ "Croatia",
                                                  "HU" ~ "Hungary",
                                                  "NL" ~ "Netherlands",
                                                  "PL" ~ "Poland",
                                                  "RO" ~ "Romania",
                                                  "SI" ~ "Slovenia",
                                                  "SK" ~ "Slovakia")) %>%
    dplyr::select(DateTime, BiddingZoneAbb, BiddingZone, NetPosition) %>%
    return()

}
