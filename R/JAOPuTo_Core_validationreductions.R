#' Core - Validation Reductions
#'
#' @description
#' Download validation reductions applied in Core DA FBMC.
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
#' JAOPuTo_Core_validationreductions(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-01 01:00"
#' )
#' }
JAOPuTo_Core_validationreductions <- function(start, end) {

  JAOPuTo_get(
    dataset  = "core",
    endpoint = "api/data/validationReductions",
    start    = start,
    end      = end,
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::mutate(TSO = dplyr::case_match(.data$tso,
                                          "ELIA" ~ "Elia",
                                          "TENNETGMBH" ~ "TenneT GmbH",
                                          "TRANSNETBW" ~ "TransnetBW",
                                          "AMPRION" ~ "Amprion",
                                          "50HERTZ" ~ "50Hertz",
                                          "TENNETBV" ~ "TenneT BV",
                                          "TRANSELECTRICA" ~ "Transelectrica",
                                          "NA" ~ NA,
                                          .default = .data$tso)) |>
    dplyr::select(.data$DateTime,
                  .data$TSO,
                  "CNEC_Name" = .data$cnecName,
                  "Direction" = .data$direction,
                  "IVA" = .data$iva,
                  "IVA_share" = .data$ivaShare,
                  "Justification" = .data$justification,
                  "Overloaded_TSO" = .data$overloadedTso,
                  "Overloaded_CNEC" = .data$overloadedName,
                  "Overloaded_EIC" = .data$overloadedEic,
                  "Overloaded_From" = .data$overloadedFromUct,
                  "Overloaded_To" = .data$overloadedToUct,
                  "IsReturnedBranch" = .data$isReturnedBranch,
                  "FallbackApplied" = .data$fallbackApplied,
                  tidyr::starts_with("coreNp_")
    )
}
