#' Core - Intraday ATC
#'
#' @description
#' Download Intraday ATCs on bidding zone borders in Core DA FBMC.
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
#' JAOPuTo_Core_intradayatc(
#'   start = "2024-01-01 00:00",
#'   end = "2024-01-10 23:00"
#' )
#' }
JAOPuTo_Core_intradayatc <- function(start,
                                            end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/intradayATC",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("initial"),
                  tidyselect::starts_with("delta")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "ATC") |>
    tidyr::separate(.data$Variable, "_", into = c("Calculation", "BiddingZoneFromAbb", "BiddingZoneToAbb")) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneFromAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneFrom = .data$BiddingZone)) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneToAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneTo = .data$BiddingZone)) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZoneFrom,
                  .data$BiddingZoneFromAbb,
                  .data$BiddingZoneTo,
                  .data$BiddingZoneToAbb,
                  .data$Calculation,
                  .data$ATC)
}
