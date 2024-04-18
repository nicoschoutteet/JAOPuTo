#' Congestion Income
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the Congestion Income generated on in Core hubs
#' @export
#' @import magrittr
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import httr
#' @import jsonlite
#' @import tibble
#' @import tidyselect
#' @import stringr
#'
#' @examples df <- JAOPuTo_congestionincome(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_congestionincome <- function(StartDateTime,
                                     EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/congestionIncome",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime + lubridate::hours(1), "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = 'Variable', values_to = 'CongestionIncome') %>%
    dplyr::mutate(Type = stringr::str_match(Variable, "gross|net")[,1],
                  Scope = tolower(stringr::str_match(Variable, "Hub|Tso|Border")[,1]),
                  Area =str_split(Variable, "_", n = 2) %>% sapply(`[[`, 2)) %>%
    dplyr::select(DateTime, Type, Scope, Area, CongestionIncome) %>%
    return()

}
