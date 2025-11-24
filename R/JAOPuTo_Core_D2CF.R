#' Core - Two-Day Ahead Congestion Forecast (D2CF)
#'
#' @description
#' Download D2CF for Core bidding zone borders in Core DA FBMC.
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
#' JAOPuTo_Core_D2CF(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-10 23:00"
#' )
#' }
JAOPuTo_Core_D2CF <- function(start,
                              end) {
  # access helper function
  df <- JAOPuTo_get(

    dataset = "core",
    endpoint = "api/data/d2CF",
    start = start,
    end = end
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::select(.data$DateTime,
                  tidyselect::starts_with("verticalLoad"),
                  tidyselect::starts_with("generation"),
                  tidyselect::starts_with("coreNetPosition")) |>
    tidyr::pivot_longer(cols = -.data$DateTime,
                        names_to = "Variable",
                        values_to = "Value") |>
    tidyr::separate(.data$Variable, "_", into = c("Variable", "BiddingZoneAbb")) |>
    dplyr::left_join(CoreBiddingZones) |>
    dplyr::mutate(Variable = dplyr::case_when(.data$Variable == "verticalLoad" ~ "Load",
                                              .data$Variable == "generation" ~ "Generation",
                                              .data$Variable == "coreNetPosition" ~ "Core Net Position"),
                  BiddingZone = dplyr::case_when(is.na(.data$BiddingZone) ~ paste0("Germany - ", .data$BiddingZoneAbb),
                                                 TRUE ~ .data$BiddingZone),
                  BiddingZoneAbb = dplyr::case_when(stringr::str_detect(.data$BiddingZone, "Germany - ") ~ "DE",
                                                    TRUE ~ BiddingZoneAbb)) |>
    dplyr::select(.data$DateTime,
                  .data$BiddingZone,
                  .data$BiddingZoneAbb,
                  .data$Variable,
                  .data$Value)
}
