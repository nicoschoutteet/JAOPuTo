#' Core - Congestion Income
#'
#' @description
#' Download congestion income generated in Core DA FBMC.
#'
#' @param start Start datetime; (POSIXct, Date, or character convertible to POSIXct)
#' @param end End datetime; (POSIXct, Date, or character convertible to POSIXct)
#'
#' @export
#'
#' @return A tibble containing the combined data returned by all API requests.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' JAOPuTo_Core_congestionincome(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_Core_congestionincome <- function(start,
                                          end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/congestionIncome",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("gross"),
                  tidyselect::starts_with("net")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "CongestionIncome") |>
    tidyr::separate(.data$Variable, "_", into = c("Calculation", "Scope"), extra = "merge") |>
    dplyr::mutate(Calculation = dplyr::case_match(.data$Calculation,
                                                  "grossHub" ~ "Gross Hub",
                                                  "grossTso" ~ "Gross TSO",
                                                  "netHub" ~ "Net Hub",
                                                  "netTso" ~ "Net TSO",
                                                  "grossBorder" ~ "Gross Border"),
                  Scope = dplyr::case_match(.data$Scope,
                                            "Apg" ~ "APG",
                                            "Rte" ~ "RTE",
                                            "TennetBv" ~ "TenneT BV",
                                            "TennetGmbh" ~ "TenneT GmbH",
                                            "Transnet" ~ "TransnetBW",
                                            "Eles" ~ "ELES",
                                            "Hops" ~ "HOPS",
                                            "Mavir" ~ "MAVIR",
                                            "Pse" ~ "PSE",
                                            "Seps" ~ "SEPS",
                                            "Ceps"~ "CEPS",
                                            "DE_DK1" ~ "DE - DK1",
                                            "DK1_DE" ~ "DK1 - DE",
                                            "ES_FR" ~ "ES - FR",
                                            "FR_ES" ~ "FR - ES",
                                            "DK1_NL" ~ "DK1 - NL",
                                            "NL_DK1" ~ "NL - DK1",
                                            .default = .data$Scope)) |>
    dplyr::select(.data$DateTime,
                  .data$Calculation,
                  .data$Scope,
                  .data$CongestionIncome)
}
