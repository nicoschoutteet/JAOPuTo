#' Core - D2CF
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the aggregated assumptions from the grid model (D2CF)
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
#' @examples JAOPuTo_Core_D2CF(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_D2CF <- function(StartDateTime,
                              EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/d2CF",
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
                        names_to = "TempVariable",
                        values_to = "Value") %>%
    dplyr::mutate(Variable = dplyr::case_match(stringr::str_extract(TempVariable, "^[^_]+"),
                                               "verticalLoad" ~ "Vertical load",
                                               "generation" ~ "Generation",
                                               "coreNetPosition" ~ "Core net position"),
                  TempHub = stringr::str_extract(TempVariable, "(?<=_).*"),
                  Scope = dplyr::case_when(TempHub %in% c("50Hertz", "Amprion", "Creos", "TennetGmbh", "Transnet") ~ "TSO",
                                           TRUE ~ "Bidding Zone"),
                  BiddingZoneAbb = dplyr::case_when(Scope == "Bidding Zone" ~ TempHub,
                                                    TRUE ~ "DE"),
                  BiddingZone =dplyr::case_match(BiddingZoneAbb,
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
                  TSO = dplyr::case_when(Scope == "TSO" ~ case_match(TempHub,
                                                                     "TennetGmbh" ~ "TenneT GmbH",
                                                                     "Transnet" ~ "TransnetBW",
                                                                     .default = TempHub),
                                         TRUE ~ dplyr::case_match(BiddingZoneAbb,
                                                                  "AT" ~ "APG",
                                                                  "BE" ~ "Elia",
                                                                  "CZ" ~ "CEPS",
                                                                  "HR" ~ "HOPS",
                                                                  "FR" ~ "RTE",
                                                                  "HU" ~ "MAVIR",
                                                                  "NL" ~ "TenneT BV",
                                                                  "PL" ~ "PSE",
                                                                  "RO" ~ "Transelectrica",
                                                                  "SK" ~ "SEPS",
                                                                  "SI" ~ "ELES"))) %>%
    dplyr::select(DateTime, BiddingZoneAbb, BiddingZone, TSO, Scope, Value) %>%
    return()

}
