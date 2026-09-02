#' Core - Active Flow-Based Constraints
#'
#' @description
#' Download active constraints in Core DA FBMC.
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
#' JAOPuTo_Core_activeconstraints(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-01 01:00"
#' )
#' }
JAOPuTo_Core_activeconstraints <- function(start, end) {

  JAOPuTo_get(
    dataset  = "core",
    endpoint = "api/data/shadowPrices",
    start    = start,
    end      = end,
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    dplyr::mutate(TSO = dplyr::case_match(.data$tso,
                                          "Ceps" ~ "CEPS",
                                          "Mavir" ~ "MAVIR",
                                          "TennetBv" ~ "TenneT BV",
                                          "Apg" ~ "APG",
                                          "TransnetBw" ~ "TransnetBW",
                                          "Pse" ~ "PSE",
                                          "TennetGmbh" ~ "TenneT GmbH",
                                          "Seps" ~ "SEPS",
                                          "Hops" ~ "HOPS",
                                          "Rte" ~ "RTE",
                                          "Eles" ~ "ELES",
                                          "NA" ~ NA,
                                          .default = .data$tso)) |>
    dplyr::left_join(read_core_sgm() |>
                       dplyr::select(.data$CNE_EIC, "CNE_Lat" = .data$lat, "CNE_Lng" = .data$lng),
                     by = c("cnecEic" = "CNE_EIC")) |>
    dplyr::select(.data$DateTime,
                  .data$TSO,
                  "CNE_Name" = .data$cnecName,
                  "CNE_EIC" = .data$cnecEic,
                  "Direction" = .data$direction,
                  "HubFrom" = .data$hubFrom,
                  "HubTo" = .data$hubTo,
                  .data$CNE_Lat,
                  .data$CNE_Lng,
                  "C_Name" = .data$contName,
                  "C_EIC" = .data$branchEic,
                  "ShadowPrice" = .data$shadowPrice,
                  "RAM" = .data$ram,
                  "RAM_MCP" = .data$ramMcp,
                  "minRAM_actual" = .data$minRamFactor,
                  "Imax" = .data$imax,
                  "Fmax" = .data$fmax,
                  "FRM" = .data$frm,
                  "Fref" = .data$fref,
                  "FCore" = .data$f0core,
                  "Fall" = .data$f0all,
                  "Fuaf" = .data$fuaf,
                  "AMR" = .data$amr,
                  "LTAmargin" = .data$ltaMargin,
                  "CVA" = .data$cva,
                  "IVA" = .data$iva,
                  "Ftotal_ltn" = .data$ftotalLtn,
                  "Maxz2zPTDF" = .data$maxZ2ZPtdf,
                  tidyr::starts_with("hub_")) |>
dplyr::rename_with(~ stringr::str_replace(.x, "^hub_", "ptdf_"),
                   dplyr::starts_with("hub_"))
}
