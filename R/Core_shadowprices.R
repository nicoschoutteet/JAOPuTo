#' Core - Shadow Prices
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the binding constraints (CNECs) after Market Coupling, with its shadow price.
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_Core_shadowprices(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_shadowprices <- function(StartDateTime,
                                      EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/shadowPrices",
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
    select(-id, -dateTimeUtc) %>%
    dplyr::mutate(TSO = dplyr::case_match(tso,
                                          "Mavir" ~ "MAVIR",
                                          "TennetBv" ~ "TenneT BV",
                                          "Pse" ~ "PSE",
                                          "Seps" ~ "SEPS",
                                          "Apg" ~ "APG",
                                          "Rte" ~ "RTE",
                                          "TennetGmbh" ~ "TenneT GmbH",
                                          "Hops" ~ "HOPS",
                                          "TransnetBw" ~ "TransnetBW",
                                          "Ceps" ~ "CEPS",
                                          "Eles" ~ "Eles",
                                          .default = tso)) %>%
    select(DateTime, TSO, CNE_Name = cnecName, CNE_EIC = cnecEic, Direction = direction,
           BiddingZoneAbbFrom = hubFrom, BiddingZoneAbbTo = hubTo,
           C_Name = contName, C_EIC = branchEic, ShadowPrice = shadowPrice,
           RAM = ram, Imax = imax, Fmax = fmax, FRM = frm, Fref = fref, F0Core = f0core,
           F0all = f0all, Fuaf = fuaf, AMR = amr, LTAMargin = ltaMargin, CVA = cva, IVA = iva, FtotalLTN = ftotalLtn,
           starts_with("hub_")) %>%
    rename_with(~ gsub("^hub_", "ptdf_", .), starts_with("hub_")) %>%
    left_join(read_csv(system.file("extdata", "Core SGM 6th release.csv", package = "JAOPuTo"))) %>%
    return()

}
