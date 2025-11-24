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
    dplyr::left_join(readr::read_csv("inst/extdata/core_sgm_6th_release.csv") |>
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
                  "ptdf_ALBE" = .data$hub_ALBE,
                  "ptdf_ALDE" = .data$hub_ALDE,
                  "ptdf_AT" = .data$hub_AT,
                  "ptdf_BE" = .data$hub_BE,
                  "ptdf_CZ" = .data$hub_CZ,
                  "ptdf_DE" = .data$hub_DE,
                  "ptdf_FR" = .data$hub_FR,
                  "ptdf_HR" = .data$hub_HR,
                  "ptdf_HU" = .data$hub_HU,
                  "ptdf_NL" = .data$hub_NL,
                  "ptdf_PL" = .data$hub_PL,
                  "ptdf_RO" = .data$hub_RO,
                  "ptdf_SI" = .data$hub_SI,
                  "ptdf_SK" = .data$hub_SK
    )
}
