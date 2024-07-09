#' CoreID - IDCC(a) - Final NTCs for SIDC
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the final leftover net transmission capacity after the day-ahead market after TSO validation for defined borders in both directions in MW.
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
#' @examples JAOPuTo_CoreID_IDCCa_finalNTC(as.POSIXct("2024-07-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-07-01 23:00", "Europe/Brussels"))
JAOPuTo_CoreID_IDCCa_finalNTC <- function(StartDateTime,
                                            EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/coreid/api/data/IDCCA_intradayNtc",
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
                        values_to = "ATC") %>%
    dplyr::mutate(BiddingZoneAbbFrom = stringr::str_split(Variable, "_", simplify = TRUE)[, 2],
                  BiddingZoneAbbTo = stringr::str_split(Variable, "_", simplify = TRUE)[, 3],
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
                                                      "SK" ~ "Slovakia",
                                                      .default = BiddingZoneAbbFrom),
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
                                                    "SK" ~ "Slovakia",
                                                    .default = BiddingZoneAbbTo)) %>%
    dplyr::select(DateTime, BiddingZoneAbbFrom, BiddingZoneAbbTo, BiddingZoneFrom, BiddingZoneTo, ATC) %>%
    return()

}
