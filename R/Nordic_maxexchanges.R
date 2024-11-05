#' Nordic - Maximum Bilateral exchanges
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the maximum bilateral exchanges ("BEX") between each combination of Nordic bidding zones
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_Nordic_maxexchanges(as.POSIXct("2024-10-30 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-10-30 23:00", "Europe/Brussels"))
JAOPuTo_Nordic_maxexchanges <- function(StartDateTime,
                                        EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/maxExchanges",
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
                        values_to = "MaxExchange") %>%
    dplyr::mutate(BiddingZoneAbbFrom = stringr::str_split(Variable, "_", simplify = TRUE)[, 2],
                  BiddingZoneAbbTo = stringr::str_split(Variable, "_", simplify = TRUE)[, 3],
                  BiddingZoneFrom = dplyr::case_match(BiddingZoneAbbFrom,
                                                      "DE" ~ "Germany/Luxembourg",
                                                      "DK1" ~ "Denmark West",
                                                      "DK2" ~ "Denmark East",
                                                      "EE" ~ "Estonia",
                                                      "FI" ~ "Finland",
                                                      "LT" ~ "Lithuania",
                                                      "NL" ~ "Netherlands",
                                                      "NO1" ~ "Norway 1",
                                                      "NO2" ~ "Norway 2",
                                                      "NO3" ~ "Norway 3",
                                                      "NO4" ~ "Norway 4",
                                                      "NO5" ~ "Norway 5",
                                                      "PL" ~ "Poland",
                                                      "SE1" ~ "Sweden 1",
                                                      "SE2" ~ "Sweden 2",
                                                      "SE3" ~ "Sweden 3",
                                                      "SE3SWL" ~ "Sweden 3 - SWL",
                                                      "SE4" ~ "Sweden 4",
                                                      "SE4SWL" ~ "Sweden 4 - SWL"),
                  BiddingZoneTo = dplyr::case_match(BiddingZoneAbbTo,
                                                    "DE" ~ "Germany/Luxembourg",
                                                    "DK1" ~ "Denmark West",
                                                    "DK2" ~ "Denmark East",
                                                    "EE" ~ "Estonia",
                                                    "FI" ~ "Finland",
                                                    "LT" ~ "Lithuania",
                                                    "NL" ~ "Netherlands",
                                                    "NO1" ~ "Norway 1",
                                                    "NO2" ~ "Norway 2",
                                                    "NO3" ~ "Norway 3",
                                                    "NO4" ~ "Norway 4",
                                                    "NO5" ~ "Norway 5",
                                                    "PL" ~ "Poland",
                                                    "SE1" ~ "Sweden 1",
                                                    "SE2" ~ "Sweden 2",
                                                    "SE3" ~ "Sweden 3",
                                                    "SE3SWL" ~ "Sweden 3 - SWL",
                                                    "SE4" ~ "Sweden 4",
                                                    "SE4SWL" ~ "Sweden 4 - SWL")) %>%
    dplyr::select(DateTime, BiddingZoneAbbFrom, BiddingZoneAbbTo, BiddingZoneFrom, BiddingZoneTo, MaxExchange) %>%
    return()

}
