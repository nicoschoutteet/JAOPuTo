#' Core - Final Computation
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
#' @examples JAOPuTo_Core_finalcomputation(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
JAOPuTo_Core_finalcomputation <- function(StartDateTime,
                                               EndDateTime,
                                               Presolved = TRUE) {

  df <- tibble::tibble()

  if(Presolved == TRUE) {

    API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/finalComputation",
                         query = list(
                           Filter = '{"Presolved":true}',
                           FromUtc = format(lubridate::with_tz(StartDateTime, "UTC"),
                                            "%Y-%m-%dT%H:%M:%S.0000Z"),
                           ToUtc = format(lubridate::with_tz(EndDateTime + lubridate::hours(1), "UTC"),
                                          "%Y-%m-%dT%H:%M:%S.0000Z")
                         )) %>%
      httr::content(as = "text")%>%
      jsonlite::fromJSON()

  } else if (Presolved == FALSE) {

    API_GET <- httr::GET("https://publicationtool.jao.eu/core/api/data/finalComputation",
                         query = list(
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
                                          "ELIA" ~ "Elia",
                                          "TENNETGMBH" ~ "TenneT GmbH",
                                          "AMPRION" ~ "Amprion",
                                          "50HERTZ" ~ "50Hertz",
                                          "TENNETBV" ~ "TenneT BV",
                                          "TRANSELECTRICA" ~ "Transelectrica",
                                          "TRANSNETBW" ~ "TransnetBW",
                                          .default = tso),
                  TSO_C = dplyr::case_match(contTso,
                                            "ELIA" ~ "Elia",
                                            "TENNETGMBH" ~ "TenneT GmbH",
                                            "AMPRION" ~ "Amprion",
                                            "50HERTZ" ~ "50Hertz",
                                            "TENNETBV" ~ "TenneT BV",
                                            "TRANSELECTRICA" ~ "Transelectrica",
                                            "TRANSNETBW" ~ "TransnetBW",
                                            .default = contTso)) %>%
    select(DateTime, TSO, CNE_Name = cneName, CNE_EIC = cneEic, Direction = direction,
           BiddingZoneAbbFrom = hubFrom, BiddingZoneAbbTo = hubTo, SubstationFrom = substationFrom, SubstationTo = substationTo,
           ElementType = elementType, FmaxType = fmaxType, TSO_C, C_Name = contingencies_branchName, C_EIC = contingencies_branchEic, Presolved = presolved,
           RAM = ram, Imax = imax, U = u, Fmax = fmax, FRM = frm, FrefInit = frefInit, Fnrao = fnrao, Fref = fref, FCore = fcore,
           Fall = fall, Fuaf = fuaf, AMR = amr, LTAMargin = ltaMargin, CVA = cva, IVA = iva, FtotalLTN = ftotalLtn,
           starts_with("ptdf_")) %>%
    return()

}
