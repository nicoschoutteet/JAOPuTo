#' Nordic - D2CF
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
#' @examples JAOPuTo_Nordic_D2CF(as.POSIXct("2024-10-30 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-10-30 23:00", "Europe/Brussels"))
JAOPuTo_Nordic_D2CF <- function(StartDateTime,
                                EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/cgmForeCast",
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
                        names_to = "TempVariable",
                        values_to = "Value") %>%
    dplyr::mutate(Variable = dplyr::case_match(stringr::str_extract(TempVariable, "^[^_]+"),
                                               "verticalLoad" ~ "Vertical load",
                                               "generation" ~ "Generation"),
                  TempHub = stringr::str_extract(TempVariable, "(?<=_).*"),
                  Scope = dplyr::case_when(TempHub %in% c("Energinet", "Fingrid", "Statnett", "SvK") ~ "TSO",
                                           TRUE ~ "Bidding Zone"),
                  BiddingZoneAbb = dplyr::case_when(Scope == "Bidding Zone" ~ TempHub,
                                                    TempHub == "Energinet" ~ "DK",
                                                    TempHub == "Fingrid" ~ "FI",
                                                    TempHub == "Statnett" ~ "NO",
                                                    TempHub == "SvK" ~ "SE"),
                  BiddingZone =dplyr::case_match(BiddingZoneAbb,
                                                 "DK1" ~ "Denmark West",
                                                 "DK2" ~ "Denmark East",
                                                 "FI" ~ "Finland",
                                                 "NO1" ~ "Norway 1",
                                                 "NO2" ~ "Norway 2",
                                                 "NO3" ~ "Norway 3",
                                                 "NO4" ~ "Norway 4",
                                                 "NO5" ~ "Norway 5",
                                                 "SE1" ~ "Sweden 1",
                                                 "SE2" ~ "Sweden 2",
                                                 "SE3" ~ "Sweden 3",
                                                 "SE4" ~ "Sweden 4",
                                                 "DK" ~ "Denmark",
                                                 "NO" ~ "Norway",
                                                 "SE" ~ "Sweden"),
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
