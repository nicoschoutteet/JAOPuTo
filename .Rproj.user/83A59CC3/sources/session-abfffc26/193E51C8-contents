#' Core - Reference Net Positions
#'
#' @description
#' Download reference net positions of non-Core bidding zone borders in Core DA FBMC.
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
#' JAOPuTo_Core_refnetpos(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_Core_refnetpos <- function(start,
                                   end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/referenceNetPosition",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("globalNetPosition")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "ReferenceNetPosition") |>
    tidyr::separate(.data$Variable, "_", into = c("global", "BiddingZoneAbb")) |>
    dplyr::left_join(CoreBiddingZones) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZone,
                  .data$BiddingZoneAbb,
                  .data$ReferenceNetPosition)
}
