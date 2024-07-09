#' Core - Price Spreads
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the price spreads (in €/MWh) on Core and non-Core bidding zone borders
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
#' @examples JAOPuTo_Core_pricespreads(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_pricespreads <- function(StartDateTime,
                                           EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/priceSpread",
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
                        values_to = "PriceSpread") %>%
    dplyr::mutate(BiddingZoneAbbFrom =  stringr::str_split(Variable, "_", simplify = TRUE)[, 2],
                  BiddingZoneAbbTo = stringr::str_split(Variable, "_", simplify = TRUE)[, 3],
                  BiddingZoneFrom = dplyr::case_match(BiddingZoneAbbFrom,
                                                      "ALBE" ~ "ALEGrO Belgium",
                                                      "ALDE" ~ "ALEGrO Germany",
                                                      "AT" ~ "Austria",
                                                      "BE" ~ "Belgium",
                                                      "CZ" ~ "Czech Republic",
                                                      "DE" ~ "Germany/Luxembourg",
                                                      "FR" ~ "France",
                                                      "HR" ~ "Croatia",
                                                      "HU" ~ "Hungary",
                                                      "NL" ~ "Netherlands",
                                                      "PL" ~ "Poland",
                                                      "RO" ~ "Romania",
                                                      "SI" ~ "Slovenia",
                                                      "SK" ~ "Slovakia",
                                                      "BG" ~ "Bulgaria",
                                                      "BA" ~ "Bosnia",
                                                      "CH" ~ "Switzerland",
                                                      "DK" ~ "Denmark",
                                                      "ES" ~ "Spain",
                                                      "PT" ~ "Portugal",
                                                      "GR" ~ "Greece",
                                                      "IT" ~ "Italy",
                                                      "ME" ~ "Montenegro",
                                                      "MK" ~ "Macedonia",
                                                      "RS" ~ "Serbia",
                                                      "UA" ~ "Ukraine",
                                                      "UK" ~ "United Kingdom",
                                                      "TR" ~ "Turkey",
                                                      "NO1" ~ "Norway 1",
                                                      "NO2" ~ "Norway 2",
                                                      "NO3" ~ "Norway 3",
                                                      "NO4" ~ "Norway 4",
                                                      "SE1" ~ "Sweden 1",
                                                      "SE2" ~ "Sweden 2",
                                                      "DK1" ~ "Denmark 1",
                                                      "DK2" ~ "Denmark 2",
                                                      "LT1" ~ "Lithuania 1",
                                                      "LT2" ~ "Lithuania 2",
                                                      "SE" ~ "Sweden",
                                                      "XK" ~ "Kosovo",
                                                      .default = BiddingZoneAbbFrom),
                  BiddingZoneTo = dplyr::case_match(BiddingZoneAbbTo,
                                                    "ALBE" ~ "ALEGrO Belgium",
                                                    "ALDE" ~ "ALEGrO Germany",
                                                    "AT" ~ "Austria",
                                                    "BE" ~ "Belgium",
                                                    "CZ" ~ "Czech Republic",
                                                    "DE" ~ "Germany/Luxembourg",
                                                    "FR" ~ "France",
                                                    "HR" ~ "Croatia",
                                                    "HU" ~ "Hungary",
                                                    "NL" ~ "Netherlands",
                                                    "PL" ~ "Poland",
                                                    "RO" ~ "Romania",
                                                    "SI" ~ "Slovenia",
                                                    "SK" ~ "Slovakia",
                                                    "BG" ~ "Bulgaria",
                                                    "BA" ~ "Bosnia",
                                                    "CH" ~ "Switzerland",
                                                    "DK" ~ "Denmark",
                                                    "ES" ~ "Spain",
                                                    "PT" ~ "Portugal",
                                                    "GR" ~ "Greece",
                                                    "IT" ~ "Italy",
                                                    "ME" ~ "Montenegro",
                                                    "MK" ~ "Macedonia",
                                                    "RS" ~ "Serbia",
                                                    "UA" ~ "Ukraine",
                                                    "UK" ~ "United Kingdom",
                                                    "TR" ~ "Turkey",
                                                    "NO1" ~ "Norway 1",
                                                    "NO2" ~ "Norway 2",
                                                    "NO3" ~ "Norway 3",
                                                    "NO4" ~ "Norway 4",
                                                    "SE1" ~ "Sweden 1",
                                                    "SE2" ~ "Sweden 2",
                                                    "DK1" ~ "Denmark 1",
                                                    "DK2" ~ "Denmark 2",
                                                    "LT1" ~ "Lithuania 1",
                                                    "LT2" ~ "Lithuania 2",
                                                    "SE" ~ "Sweden",
                                                    "XK" ~ "Kosovo",
                                                    .default = BiddingZoneAbbTo)) %>%
    dplyr::select(DateTime, BiddingZoneAbbFrom, BiddingZoneAbbTo, BiddingZoneFrom, BiddingZoneTo, PriceSpread) %>%
    return()

}
