#' Core - Remedial Actions (Preventive)
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the preventive remedial actions applied by Core TSOs
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_Core_remedialactionspreventive(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_remedialactionspreventive <- function(StartDateTime,
                                                   EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/pra",
                       query = list(
                         FromUtc = format(with_tz(StartDateTime, "UTC"),
                                          "%Y-%m-%dT%H:%M:%S.0000Z"),
                         ToUtc = format(with_tz(EndDateTime + lubridate::hours(1), "UTC"),
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
    dplyr::mutate(Type = "Preventive",
                  TSO = dplyr::case_match(tso,
                                          "Apg" ~ "APG",
                                          "TennetGmbh" ~ "TenneT GmbH",
                                          "Pse" ~ "PSE",
                                          "Ceps" ~ "CEPS",
                                          "TransnetBw" ~ "TransnetBW",
                                          "TennetBv" ~ "TenneT BV",
                                          "Eles" ~ "ELES",
                                          "Rte" ~ "RTE",
                                          "Mavir" ~ "MAVIR",
                                          "Seps" ~ "SEPS",
                                          .default = tso),
                  BiddingZoneAbb = dplyr::case_match(TSO,
                                                     "APG" ~ "AT",
                                                     "Elia" ~ "BE",
                                                     "HOPS" ~ "HR",
                                                     "CEPS" ~ "CZ",
                                                     "RTE" ~ "FR",
                                                     "Amprion" ~ "DE",
                                                     "TransnetBW" ~ "DE",
                                                     "50Hertz" ~ "DE",
                                                     "TenneT GmbH" ~ "DE",
                                                     "MAVIR" ~ "HU",
                                                     "TenneT BV" ~ "NL",
                                                     "PSE" ~ "PL",
                                                     "Transelectrica" ~ "RO",
                                                     "SEPS" ~ "SK",
                                                     "ELES" ~ "SI"),
                  BiddingZone = dplyr::case_match(BiddingZoneAbb,
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
                                                  "SK" ~ "Slovakia")) %>%
    dplyr::select(DateTime, TSO, BiddingZoneAbb, BiddingZone, Type, RemedialAction = name, PSTBeforeNRAO = baseline, PSTAfterNRAO = afterNrao, Justification = reason) %>%
    return()

}
