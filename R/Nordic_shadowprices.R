#' Nordic - Shadow Prices
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
#' @examples JAOPuTo_Nordic_shadowprices(as.POSIXct("2024-10-30 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-10-30 23:00", "Europe/Brussels"))
JAOPuTo_Nordic_shadowprices <- function(StartDateTime,
                                        EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/fbDomainShadowPrice",
                       query = list(
                         Skip = 0,
                         Take=99999,
                         FromUtc = format(lubridate::with_tz(StartDateTime, "UTC"),
                                          "%Y-%m-%dT%H:%M:%S.0000Z"),
                         ToUtc = format(lubridate::with_tz(EndDateTime + lubridate::hours(1), "UTC"),
                                        "%Y-%m-%dT%H:%M:%S.0000Z")
                       )) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  df <- API_GET$data %>%
    tibble::as_tibble() %>%
    tidyr::unnest(cols = c(contingencies),
                  names_sep = "_") %>%
    dplyr::mutate(DateTime = lubridate::with_tz(as.POSIXct(dateTimeUtc, tz = "UTC",
                                                           format = "%Y-%m-%dT%H:%M:%SZ"),
                                                "Europe/Brussels")) %>%
    select(-id, -dateTimeUtc) %>%
    dplyr::mutate(TSO = dplyr::case_match(tso,
                                          "FINGRID" ~ "Fingrid",
                                          "SVK" ~ "Svenska kraftnät",
                                          "ENERGINET" ~ "Energinet",
                                          "STATNETT" ~ "Statnett",
                                          .default = tso),
                  ElementType = dplyr::case_match(cnecType,
                                                  "BRANCH" ~ "CNEC",
                                                  "ALLOCATION_CONSTRAINT" ~ "Allocation constraint",
                                                  .default = cnecType)) %>%
    select(DateTime, TSO, CNE_Name = cneName, CNE_EIC = cneEic, ElementType,
           BiddingZoneAbbFrom = biddingZoneFrom, BiddingZoneAbbTo = biddingZoneTo, SubstationFrom = substationFrom, SubstationTo = substationTo,
           ElementType, C_Name = contName, C_EIC = contEic, Presolved = nonRedundant, Significant = significant,
           RAM = ram, MinFlow = minFlow, MaxFlow = maxFlow, Imax = imax, U = u, Fmax = fmax, FRM = frm, Fnrao = fnrao, Fref = fref,
           Fall = fall, AMR = amr, AAC = aac, IVA = iva, ShadowPrice = shadowPrice,
           starts_with("ptdf_")) %>%
    filter(!is.na(ShadowPrice) & ShadowPrice > 0) %>%
    return()

}
