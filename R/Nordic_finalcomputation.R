#' Nordic - Final Computation
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#' @param Presolved Boolean variable (TRUE/FALSE) to determine whether only presolved network elements are included (defaults to TRUE)
#'
#' @return a dataframe containing the final flow-based parameters of the selected business day and MTU following long term nominations (Ltnom balanced).
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_Nordic_finalcomputation(as.POSIXct("2024-10-30 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-10-30 23:00", "Europe/Brussels"))
JAOPuTo_Nordic_finalcomputation <- function(StartDateTime,
                                            EndDateTime,
                                            Presolved = TRUE) {

  df <- tibble::tibble()

  if(Presolved == TRUE) {

    API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/finalComputation",
                         query = list(
                           Filter = '{"NonRedundant":true}',
                           Skip = 0,
                           Take=99999,
                           FromUtc = format(lubridate::with_tz(StartDateTime, "UTC"),
                                            "%Y-%m-%dT%H:%M:%S.0000Z"),
                           ToUtc = format(lubridate::with_tz(EndDateTime + lubridate::hours(1), "UTC"),
                                          "%Y-%m-%dT%H:%M:%S.0000Z")
                         )) %>%
      httr::content(as = "text")%>%
      jsonlite::fromJSON()

  } else if (Presolved == FALSE) {

    API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/finalComputation",
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

  }

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
           Fall = fall, AMR = amr, AAC = aac, IVA = iva,
           starts_with("ptdf_")) %>%
    return()

}
