#' Core - Active LTA Constraints
#'
#' @description
#' Download active LTA constraints in Core DA FBMC.
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
#' JAOPuTo_Core_activeltaconstraints(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-01 01:00"
#' )
#' }
JAOPuTo_Core_activeltaconstraints <- function(start, end) {

  JAOPuTo_get(
    dataset  = "core",
    endpoint = "api/data/activeLtaConstraint",
    start    = start,
    end      = end,
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("shadowPrice"),
                  tidyselect::starts_with("qty")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "Value") |>
    tidyr::separate(.data$Variable, "_", into = c("Variable", "BiddingZoneFromAbb", "BiddingZoneToAbb")) |>
    tidyr::pivot_wider(names_from = .data$Variable,
                       values_from = .data$Value) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneFromAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneFrom = .data$BiddingZone)) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneToAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneTo = .data$BiddingZone)) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZoneFrom,
                  .data$BiddingZoneFromAbb,
                  .data$BiddingZoneTo,
                  .data$BiddingZoneToAbb,
                  "ShadowPrice" = .data$shadowPrice,
                  "LTAQuantity" = .data$qty)

}
