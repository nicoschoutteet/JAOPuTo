#' Nordic - Validation Reductions
#'
#' @param StartDateTime POSIXct-class variable containing the start datetime for data download
#' @param EndDateTime POSIXct-class variable containing the end datetime for data download
#'
#' @return a dataframe containing the network elements for which capacity has been reduced as an outcome of the validation processes, including a justification for this reduction
#' @import tibble
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @export
#'
#' @examples JAOPuTo_Nordic_validationreductions(as.POSIXct("2024-11-05 00:00", "Europe/Brussels"),
#' as.POSIXct("2024-11-05 23:00", "Europe/Brussels"))
JAOPuTo_Nordic_validationreductions <- function(StartDateTime,
                                              EndDateTime) {

  df <- tibble::tibble()

  API_GET <- httr::GET("https://publicationtool.jao.eu/nordic/api/data/validationReductions",
                       query = list(
                         Skip = 0,
                         Take=99999,
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
    select(-id, -dateTimeUtc, -lastModifiedOn) %>%
    dplyr::mutate(TSO = dplyr::case_match(tso,
                                          "FINGRID" ~ "Fingrid",
                                          "SVK" ~ "Svenska kraftnät",
                                          "ENERGINET" ~ "Energinet",
                                          "STATNETT" ~ "Statnett",
                                          .default = tso)) %>%
    select(DateTime, TSO, CNEC_Name = cnecName,
           IVA = iva, CVA = cva, Justification = justification) %>%
    return()

}
