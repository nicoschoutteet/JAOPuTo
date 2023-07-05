#' Max exchanges
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the max bilateral exchanges between Core bidding zones
#' @export
#' @import magrittr
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import tibble
#' @import tidyselect
#'
#' @examples df <- JAOPuTo_maxexchanges(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_maxexchanges <- function(StartDateTime,
                                 EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/maxExchanges",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = 'Variable', values_to = 'MaxExchanges') %>%
    dplyr::mutate(Border = stringr::str_replace(substr(Variable, 8, nchar(Variable)), "_", "-")) %>%
    dplyr::select(DateTime, Border, MaxExchanges) %>%
    return()

}
