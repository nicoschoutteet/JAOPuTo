#' Core net positions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the Core net positions for all 14 Core bidding zones
#' @export
#' @import magrittr
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import httr
#' @import jsonlite
#' @import tibble
#' @import tidyselect
#'
#' @examples df <- JAOPuTo_netpositions(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_netpositions <- function(StartDateTime,
                                 EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/netPos",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = lubridate::with_tz(as.POSIXct(dateTimeUtc,
                                                           format = "%Y-%m-%dT%H:%M:%S",
                                                           tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = 'BiddingZone', values_to = 'NetPosition') %>%
    dplyr::mutate(BiddingZoneAbb = substr(BiddingZone, 5, nchar(BiddingZone)),
                  BiddingZone = dplyr::recode(BiddingZone,
                                              "hub_ALBE" = "ALEGrO Belgium",
                                              "hub_ALDE" = "ALEGrO Germany",
                                              "hub_AT" = "Austria",
                                              "hub_BE" = "Belgium",
                                              "hub_CZ" = "Czech Republic",
                                              "hub_DE" = "Germany/Luxembourg",
                                              "hub_FR" = "France",
                                              "hub_HR" = "Croatia",
                                              "hub_HU" = "Hungary",
                                              "hub_NL" = "Netherlands",
                                              "hub_PL" = "Poland",
                                              "hub_RO" = "Romania",
                                              "hub_SI" = "Slovenia",
                                              "hub_SK" = "Slovakia")) %>%
    select(DateTime, BiddingZone, BiddingZoneAbb, NetPosition) %>%
    return()

  }
