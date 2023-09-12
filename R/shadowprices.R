#' Shadow Prices
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the shadow prices per active constraint for Core TSOs
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
#' @examples df <- JAOPuTo_shadowprices(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_shadowprices <- function(StartDateTime,
                                 EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/shadowPrices",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime + hours(1), "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    dplyr::mutate(TSO = recode(tso,
                               "Apg" = "AT_APG",
                               "Elia" = "BE_Elia",
                               "Ceps" = "CZ_CEPS",
                               "TennetGmbh" = "DE_TenneTGmbH",
                               "TransnetBw" = "DE_TransnetBW",
                               "Amprion" = "DE_Amprion",
                               "50Hertz" = "DE_50Hertz",
                               "Rte" = "FR_RTE",
                               "Hops" = "HR_HOPS",
                               "Mavir" = "HU_MAVIR",
                               "TennetBv" = "NL_TenneTBV",
                               "Pse" = "PL_PSE",
                               "Transelectrica" = "RO_Transelectrica",
                               "Eles" = "SI_ELES",
                               "Seps" = "SK_SEPS")) %>%
    tidyr::separate_wider_delim(cols = TSO,
                                delim = "_",
                                names = c("BiddingZoneAbb", "TSO"),
                                too_few = "align_start") %>%
    dplyr::select(DateTime, TSO, BiddingZoneAbb,
                  everything(), -tso) %>%
    return()

}
