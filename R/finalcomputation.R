#' Final Computation
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the final computation flow-based domain for Core TSOs
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
#' @examples df <- JAOPuTo_finaldomain(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_finaldomain <- function(StartDateTime,
                                EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/finalComputation?Filter=%7B'Presolved'%3Atrue%7D&",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    dplyr::mutate(TSO = recode(tso,
                               "APG" = "AT_APG",
                               "ELIA" = "BE_Elia",
                               "CEPS" = "CZ_CEPS",
                               "TENNETGMBH" = "DE_TenneTGmbH",
                               "TRANSNETBW" = "DE_TransnetBW",
                               "AMPRION" = "DE_Amprion",
                               "50HERTZ" = "DE_50Hertz",
                               "RTE" = "FR_RTE",
                               "HOPS" = "HR_HOPS",
                               "MAVIR" = "HU_MAVIR",
                               "TENNETBV" = "NL_TenneTBV",
                               "PSE" = "PL_PSE",
                               "TRANSELECTRICA" = "RO_Transelectrica",
                               "ELES" = "SI_ELES",
                               "SEPS" = "SK_SEPS")) %>%
    tidyr::separate_wider_delim(cols = TSO,
                                delim = "_",
                                names = c("BiddingZoneAbb", "TSO"),
                                too_few = "align_start") %>%
    dplyr::select(DateTime, TSO, BiddingZoneAbb,
                  everything(), -tso) %>%
    return()

}
