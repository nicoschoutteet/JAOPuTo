#' Final Bilateral Exchange Restrictions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the final bilateral exchange restrictions on borders between Core bidding zones
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
#' @examples df <- JAOPuTo_finalBEXrestrictions(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_finalBEXrestrictions <- function(StartDateTime,
                                         EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/bexRestrictions",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime + lubridate::hours(1), "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = "Variable", values_to = "FinalBEXRestrictions") %>%
    tidyr::separate_wider_delim(cols = Variable, delim = "_", names = c("Type", "BZ1", "BZ2")) %>%
    dplyr::mutate(Border = paste0(BZ1, "-", BZ2)) %>%
    dplyr::select(DateTime, Border, FinalBEXRestrictions) %>%
    return()

}
