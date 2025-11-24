#' Core - Max Net Positions
#'
#' @description
#' Download maximum import and export position for selected bidding zones in Core DA FBMC.
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
#' JAOPuTo_Core_maxnetpos(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_Core_maxnetpos <- function(start,
                                   end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/maxNetPos",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("min"),
                  tidyselect::starts_with("max")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "MaxNetPosition") |>
    dplyr::mutate(Direction = dplyr::case_when(substr(.data$Variable, 1, 3) == "min" ~ "Import",
                                               substr(.data$Variable, 1, 3) == "max" ~ "Export"),
                  BiddingZoneAbb = substr(.data$Variable, 4, nchar(.data$Variable))) |>
    dplyr::left_join(CoreBiddingZones) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZone,
                  .data$BiddingZoneAbb,
                  .data$Direction,
                  .data$MaxNetPosition)
}
