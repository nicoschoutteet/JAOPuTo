#' Core IDCC(b) - Reference Program
#'
#' @description
#' Download reference programs of Core and non-Core bidding zone borders in Core IDCC(b).
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
#' JAOPuTo_CoreID_IDCCb_refprog(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_CoreID_IDCCb_refprog <- function(start,
                                         end) {
  # access helper function
  JAOPuTo_get(

    dataset = "coreID",
    endpoint = "api/data/IDCCB_refprog",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("border")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "ReferenceProgram") |>
    tidyr::separate(.data$Variable, "_", into = c("global", "BiddingZoneFromAbb", "BiddingZoneToAbb")) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneFromAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneFrom = .data$BiddingZone)) |>
    dplyr::left_join(CoreBiddingZones |> dplyr::rename(BiddingZoneToAbb = .data$BiddingZoneAbb,
                                                       BiddingZoneTo = .data$BiddingZone)) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZoneFrom,
                  .data$BiddingZoneFromAbb,
                  .data$BiddingZoneTo,
                  .data$BiddingZoneToAbb,
                  .data$ReferenceProgram)
}
