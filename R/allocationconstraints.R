#' Allocation Constraints
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the allocation constraints for Core bidding zones
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
#' @examples df <- JAOPuTo_allocationconstraints(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
JAOPuTo_allocationconstraints <- function(StartDateTime,
                                          EndDateTime) {

  df <- tibble::tibble()

  list <- httr::GET("https://publicationtool.jao.eu/core/api/data/allocationConstraint",
                    query = list(FromUtc = format(with_tz(StartDateTime, "UTC"), "%Y-%m-%dT%H:%M:%S.000Z"),
                                 ToUtc = format(with_tz(EndDateTime + lubridate::hours(1), "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")))%>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- list$data %>%
    dplyr::mutate(DateTime = with_tz(as.POSIXct(dateTimeUtc,
                                                format = "%Y-%m-%dT%H:%M:%S",
                                                tz = "UTC"), "CET")) %>%
    dplyr::select(DateTime, tidyselect::everything(), -id, -dateTimeUtc) %>%
    tidyr::pivot_longer(-DateTime, names_to = "Type", values_to = "AllocationConstraint") %>%
    tidyr::separate_wider_delim(cols = Type, delim = "_", names = c("Direction", "BiddingZoneAbb")) %>%
    dplyr::mutate(Direction = recode(Direction, "limitDown" = "Import", "limitUp" = "Export"),
                  BiddingZone = recode(BiddingZoneAbb,
                                       "PL" = "Poland",
                                       "BE" = "Belgium")) %>%
    dplyr::select(DateTime, BiddingZone, BiddingZoneAbb, Direction, AllocationConstraint) %>%
    return()

}
