#' Core - Initial Computation Computation (Virgin Domain)
#'
#' @description
#' Download pre-final flow-based parameters (early publication) in Core DA FBMC.
#'
#' @param start Start datetime; (POSIXct, Date, or character convertible to POSIXct)
#' @param end End datetime; (POSIXct, Date, or character convertible to POSIXct)
#' @param presolved Boolean; filter only presolved CNECs (default = TRUE)
#'
#' @export
#'
#' @return A tibble containing the combined data returned by all API requests.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' JAOPuTo_Core_initialcomputation(
#'   start = "2025-01-01 00:00",
#'   end = "2025-01-01 01:00"
#' )
#' }
JAOPuTo_Core_initialcomputation <- function(start, end,
                                             presolved = TRUE) {

  # Filter JSON opbouwen
  filter_json <- jsonlite::toJSON(
    list(Presolved = presolved),
    auto_unbox = TRUE
  )

  df <- JAOPuTo_get(
    dataset  = "core",
    endpoint = "api/data/initialComputation",
    start    = start,
    end      = end,
    Filter   = filter_json
  ) |> # endpoint-specific data transformations
    dplyr::mutate(dateTimeUtc = lubridate::ymd_hms(.data$dateTimeUtc, tz = "UTC"),
                  DateTime = lubridate::with_tz(.data$dateTimeUtc, "Europe/Brussels")) |>
    tidyr::unnest(.data$contingencies, names_sep = "cont_") |>
    dplyr::mutate(TSO = dplyr::case_match(.data$tso,
                                          "ELIA" ~ "Elia",
                                          "TENNETGMBH" ~ "TenneT GmbH",
                                          "TRANSNETBW" ~ "TransnetBW",
                                          "AMPRION" ~ "Amprion",
                                          "50HERTZ" ~ "50Hertz",
                                          "TENNETBV" ~ "TenneT BV",
                                          "TRANSELECTRICA" ~ "Transelectrica",
                                          "NA" ~ NA,
                                          .default = .data$tso),
                  C_TSO = dplyr::case_match(.data$contTso,
                                            "ELIA" ~ "Elia",
                                            "TENNETGMBH" ~ "TenneT GmbH",
                                            "TRANSNETBW" ~ "TransnetBW",
                                            "AMPRION" ~ "Amprion",
                                            "50HERTZ" ~ "50Hertz",
                                            "TENNETBV" ~ "TenneT BV",
                                            "TRANSELECTRICA" ~ "Transelectrica",
                                            "NA" ~ NA,
                                            .default = .data$contTso),
                  minRAM_actual = .data$minRamFactor / .data$fmax) |>
    dplyr::left_join(read_core_sgm() |>
                       dplyr::select(.data$CNE_EIC, "CNE_Lat" = .data$lat, "CNE_Lng" = .data$lng),
                     by = c("cneEic" = "CNE_EIC")) |>
    dplyr::select(.data$DateTime,
                  .data$TSO,
                  "CNE_Name" = .data$cneName,
                  "CNE_EIC" = .data$cneEic,
                  "CNE_Status" = .data$cneStatus,
                  "CNE_Type" = .data$elementType,
                  "Direction" = .data$direction,
                  "HubFrom" = .data$hubFrom,
                  "HubTo" = .data$hubTo,
                  "SubstationFrom" = .data$substationFrom,
                  "SubstationTo" = .data$substationTo,
                  .data$CNE_Lat,
                  .data$CNE_Lng,
                  "Fmax_type" = .data$fmaxType,
                  "C_Number" = .data$contingenciescont_number,
                  "C_TSO",
                  "C_Name" = .data$contName,
                  "C_EIC" = .data$contingenciescont_branchEic,
                  "C_Type" = .data$contingenciescont_elementType,
                  "C_HubFrom" = .data$contingenciescont_hubFrom,
                  "C_HubTo" = .data$contingenciescont_hubTo,
                  "C_SubstationFrom" = .data$contingenciescont_substationFrom,
                  "C_SubstationTo" = .data$contingenciescont_substationTo,
                  "Presolved" = .data$presolved,
                  "RAM" = .data$ram,
                  .data$minRAM_actual,
                  "minRAM_target" = .data$minRamTarget,
                  "minRAM_justification" = .data$justification,
                  "Imax" = .data$imax,
                  "U" = .data$u,
                  "Fmax" = .data$fmax,
                  "FRM" = .data$frm,
                  "Fref_init" = .data$frefInit,
                  "Fnrao" = .data$fnrao,
                  "Fref" = .data$fref,
                  "FCore" = .data$fcore,
                  "Fall" = .data$fall,
                  "Fuaf" = .data$fuaf,
                  "AMR" = .data$amr,
                  "LTAmargin" = .data$ltaMargin,
                  "CVA" = .data$cva,
                  "IVA" = .data$iva,
                  "Ftotal_ltn" = .data$ftotalLtn,
                  tidyr::starts_with("ptdf_")
    )
}
