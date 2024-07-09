#' Core ID - Monitoring Tool
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe reflecting the data completeness for each of the data items (parameters) that can be consulted through the Publication Tool
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import lubridate
#' @export
#'
#' @examples JAOPuTo_CoreID_monitoring(as.POSIXct("2024-07-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-07-01 23:00", "Europe/Brussels"))
JAOPuTo_CoreID_monitoring <- function(StartDateTime,
                                    EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/coreid/api/system/monitoring",
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
    dplyr::mutate(DateTime = as.POSIXct(`businessDayUtc`, tz = "UTC",
                                        format = "%Y-%m-%dT%H:%M:%SZ")) %>%
    dplyr::select(DateTime, Parameter = page, Status = status, FollowUpActionInitiated = followUpActionInitiated) %>%
    return()

}
