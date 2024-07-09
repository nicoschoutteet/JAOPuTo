#' Core - Congestion Income
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the gross and net congestion income per Core hub, TSO and border
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
#' @examples JAOPuTo_Core_congestionincome(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_congestionincome <- function(StartDateTime,
                                 EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/congestionIncome",
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
                        values_to = "CongestionIncome") %>%
    dplyr::mutate(Type = dplyr::case_match(stringr::str_extract(TempVariable, "^[^_]+"),
                                               "grossHub" ~ "Gross",
                                               "grossTso" ~ "Gross",
                                               "grossBorder" ~ "Gross",
                                           "netHub" ~ "Net",
                                           "netTso" ~ "Net",
                                           "netBorder" ~"Net"),
                  Scope = dplyr::case_match(stringr::str_extract(TempVariable, "^[^_]+"),
                                            "grossHub" ~ "Hub",
                                            "grossTso" ~ "TSO",
                                            "grossBorder" ~ "Border",
                                            "netHub" ~ "Hub",
                                            "netTso" ~ "TSO",
                                            "netBorder" ~"Border"),
                 Variable = stringr::str_extract(TempVariable, "(?<=_).*")) %>%
    dplyr::select(DateTime, Scope, Variable, Type, CongestionIncome) %>%
    return()

}
