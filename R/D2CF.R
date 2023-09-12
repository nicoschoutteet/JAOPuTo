#' D2CF
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the D2CF (Two Day-Ahead Congestion Forecast) for Core bidding zones
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
#' @examples df <- JAOPuTo_D2CF(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_D2CF <- function(StartDateTime,
                         EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/d2CF",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime + lubridate::hours(1), "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = "Variable", values_to = "Value") %>%
    tidyr::separate_wider_delim(cols = Variable, delim = "_", names = c("Type", "Who")) %>%
    dplyr::mutate(Type = recode(Type,
                                "verticalLoad" = "Vertical load",
                                "generation" = "Generation",
                                "coreNetPosition" = "Core net position"),
                  Scope = case_when(Who %in% c("AT", "BE", "CZ", "DE", "FR", "HR", "HU", "NL", "PL", "RO", "SI", "SK") ~ "Bidding Zone",
                                   Who %in% c("50Hertz", "Amprion", "Creos", "TennetGmbh", "Transnet") ~ "TSO"),
                  Zone = recode(Who,
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
    dplyr::select(DateTime, Scope, Zone, Type, Value) %>%
    return()

}
