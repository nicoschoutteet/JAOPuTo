#' Core - Spanning / DFP
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
#' @examples JAOPuTo_Core_spanningDFP(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_spanningDFP <- function(StartDateTime,
                                    EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/spanningDefaultFBP",
                       query = list(
                         FromUtc = format(lubridate::with_tz(StartDateTime, "UTC"),
                                          "%Y-%m-%dT%H:%M:%S.0000Z"),
                         ToUtc = format(lubridate::with_tz(EndDateTime + lubridate::hours(1), "UTC"),
                                        "%Y-%m-%dT%H:%M:%S.0000Z")
                       )) %>%
    httr::content(as = "text")%>%
    jsonlite::fromJSON()

  if(length(API_GET$data > 0)) {

    df <- API_GET$data %>%
      tibble::as_tibble() %>%
      dplyr::mutate(DateTime = lubridate::with_tz(as.POSIXct(dateTimeUtc, tz = "UTC",
                                                             format = "%Y-%m-%dT%H:%M:%SZ"),
                                                  "Europe/Brussels")) %>%
      dplyr::select(DateTime, Computation = computation, Type = type) %>%
      return()

  } else {

    print("No Spanning / DFP found for selected timestamps")
  }

}
