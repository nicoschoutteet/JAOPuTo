#' CoreID - IDCCb - Maximum Bilateral exchanges
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the maximum bilateral exchanges ("BEX") between each combination of Core bidding zones
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_CoreID_IDCCb_maxexchanges(as.POSIXct("2024-07-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-07-01 23:00", "Europe/Brussels"))
JAOPuTo_CoreID_IDCCb_maxexchanges <- function(StartDateTime,
                                      EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/coreid/api/data/IDCCB_maxExchanges",
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
                        values_to = "MaxExchange") %>%
    dplyr::mutate(BiddingZoneAbbFrom = substr(Variable, 8, 9),
                  BiddingZoneAbbTo = substr(Variable, 11, 12),
                  BiddingZoneFrom = dplyr::case_match(BiddingZoneAbbFrom,
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
                                                      "SK" ~ "Slovakia"),
                  BiddingZoneTo = dplyr::case_match(BiddingZoneAbbTo,
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
    dplyr::select(DateTime, BiddingZoneAbbFrom, BiddingZoneAbbTo, BiddingZoneFrom, BiddingZoneTo, MaxExchange) %>%
    return()

}
