#' Core - Spanning / Default Flow-based Parameters
#'
#' @description
#' Download status of spanning or default parameter application in Core DA FBMC.
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
#' JAOPuTo_Core_spanningdfp(
#'   start = "2025-06-25 00:00",
#'   end = "2025-06-25 23:00"
#' )
#' }
JAOPuTo_Core_spanningdfp <- function(start,
                                     end) {
  # access helper function
  JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/spanningDefaultFBP",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime, "Computation" = .data$computation, "Type" = .data$type)
}
