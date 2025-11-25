#' Core IDA3 - Net Position
#'
#' @description
#' Download Core net position for selected bidding zones in Core intraday auction 3.
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
#' JAOPuTo_Core_IDA3_netposition(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_Core_IDA3_netposition <- function(start,
                                          end) {
  # access helper function
  JAOPuTo_get(

    dataset = "coreID",
    endpoint = "api/data/ID3_netPos",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("hub"),
                  tidyselect::starts_with("delta")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "NetPosition") |>
    tidyr::separate(.data$Variable, "_", into = c("hub", "BiddingZoneAbb")) |>
    dplyr::left_join(CoreBiddingZones) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZone,
                  .data$BiddingZoneAbb,
                  .data$NetPosition)
}
