#' Core - Remedial Actions Preventive
#'
#' @description
#' Download preventive remedial actions applied in Core DA FBMC.
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
#' JAOPuTo_Core_rapreventive(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }11
JAOPuTo_Core_rapreventive <- function(start,
                                            end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/pra",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels"),
                  TSO = dplyr::case_match(.data$tso,
                                          "TennetGmbh" ~ "TenneT GmbH",
                                          "TransnetBw" ~ "TransnetBW",
                                          "TennetBv" ~ "TenneT BV",
                                          "Apg" ~ "APG",
                                          "Pse" ~ "PSE",
                                          "Ceps" ~ "CEPS",
                                          "Seps" ~ "SEPS",
                                          "Rte" ~ "RTE",
                                          .default = .data$tso)) |>
    dplyr::select(.data$DateTime,
                  "CNE_Name" = .data$name,
                  .data$TSO,
                  "Baseline" = .data$baseline,
                  "AfterNRA" = .data$afterNrao,
                  "Reason" = .data$reason)
}

