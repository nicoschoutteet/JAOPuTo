#' Core min-max positions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the Core minimum and maximum net positions for all 14 Core bidding zones
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
#' @examples df <- JAOPuTo_maxnetpositions(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_maxnetpositions <- function(StartDateTime,
                                 EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/maxNetPos",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = lubridate::with_tz(as.POSIXct(dateTimeUtc,
                                                           format = "%Y-%m-%dT%H:%M:%S",
                                                           tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = 'Variable', values_to = 'NetPosition') %>%
    dplyr::mutate(Direction = dplyr::recode(substr(Variable, 1, 3),
                                            "min" = "import",
                                            "max" = "export"),
                  BiddingZoneAbb = substr(Variable, 4, nchar(Variable)),
                  BiddingZone = dplyr::recode(BiddingZoneAbb,
                                              "ALBE" = "ALEGrO Belgium",
                                              "ALDE" = "ALEGrO Germany",
                                              "AT" = "Austria",
                                              "BE" = "Belgium",
                                              "CZ" = "Czech Republic",
                                              "DE" = "Germany/Luxembourg",
                                              "FR" = "France",
                                              "HR" = "Croatia",
                                              "HU" = "Hungary",
                                              "NL" = "Netherlands",
                                              "PL" = "Poland",
                                              "RO" = "Romania",
                                              "SI" = "Slovenia",
                                              "SK" = "Slovakia")) %>%
    dplyr::select(DateTime, BiddingZone, BiddingZoneAbb, Direction, NetPosition) %>%
    return()

}
