#' Core - Allocation constraints
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the allocation constraints applicable to selected Core bidding zones' net positions
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_Core_allocationconstraints(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_allocationconstraints <- function(StartDateTime,
                                                 EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/allocationConstraint",
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
                        names_to = "Variable",
                        values_to = "AllocationConstraint") %>%
    dplyr::mutate(BiddingZoneAbb = substr(Variable, nchar(Variable) - 1, nchar(Variable)),
                  BiddingZone = dplyr::case_match(BiddingZoneAbb,
                                                  "PL" ~ "Poland",
                                                  "BE" ~ "Belgium"),
                  Direction = dplyr::case_match(substr(Variable, 1, nchar(Variable) - 3),
                                                "limitDown" ~ "Import",
                                                "limitUp" ~ "Export")) %>%
    dplyr::select(DateTime, BiddingZoneAbb, BiddingZone, Direction, AllocationConstraint) %>%
    return()

}
