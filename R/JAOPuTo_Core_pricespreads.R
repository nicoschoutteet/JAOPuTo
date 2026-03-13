#' Core - Price Spreads
#'
#' @description
#' Download price spreads between selected bidding zones in Core DA FBMC.
#' Note that the sign is inverted: JAO Publication Tool defines a negative price spread on BZ1-BZ2 when price(BZ1) > price(BZ2)
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
#' JAOPuTo_Core_pricespreads(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_Core_pricespreads <- function(start,
                                      end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/priceSpread",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("border")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "PriceSpread") |>
    dplyr::mutate(BiddingZoneFromAbb = substr(.data$Variable, 8,9),
                  BiddingZoneToAbb = substr(.data$Variable, 11, 12),
                  PriceSpread = -.data$PriceSpread) |> # note that sign is reversed (see documentation)
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneFromAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneFrom = .data$BiddingZone)) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneToAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneTo = .data$BiddingZone)) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZoneFrom,
                  .data$BiddingZoneFromAbb,
                  .data$BiddingZoneTo,
                  .data$BiddingZoneToAbb,
                  .data$PriceSpread) |>
    dplyr::filter(.data$BiddingZoneFromAbb %in% c("ALBE", "ALDE", "AT", "BE", "CZ", "DE", "HR", "HU", "FR", "NL", "PL", "RO", "SI", "SK") &
                    .data$BiddingZoneToAbb %in% c("ALBE", "ALDE", "AT", "BE", "CZ", "DE", "HR", "HU", "FR", "NL", "PL", "RO", "SI", "SK"))
}
