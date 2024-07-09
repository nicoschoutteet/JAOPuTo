#' Core - Reference Net Positions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the reference net positions for non-Core bidding zones
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @export
#'
#' @examples JAOPuTo_Core_refnetpositions(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_refnetpositions <- function(StartDateTime,
                                         EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/referenceNetPosition",
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
                        values_to = "ReferenceNetPosition") %>%
    dplyr::mutate(BiddingZoneAbb = stringr::str_extract(Variable, "(?<=_).*"),
                  BiddingZone = dplyr::case_match(BiddingZoneAbb,
                                                  "AL" ~ "Albania",
                                                  "BA" ~ "Bosnia",
                                                  "BG" ~ "Bulgaria",
                                                  "CH" ~ "Switzerland",
                                                  "DK1" ~ "Denmark 1",
                                                  "ES" ~ "Spain",
                                                  "GR" ~ "Greece",
                                                  "IT" ~ "Italy",
                                                  "ME" ~ "Montenegro",
                                                  "MK" ~ "Macedonia",
                                                  "PT" ~ "Portugal",
                                                  "RS" ~ "Serbia",
                                                  "TR" ~ "Turkey",
                                                  "UA" ~ "Ukraine")) %>%
    dplyr::select(DateTime, BiddingZoneAbb, BiddingZone, ReferenceNetPosition) %>%
    return()

}
