CoreBiddingZones <- tibble::tibble(
  BiddingZoneAbb = c("ALBE", "ALDE", "AT", "BE", "CZ", "DE", "HR", "HU", "FR", "NL", "PL", "RO", "SI", "SK",
                     "DK1", "ES", "IT", "BG", "AL", "BA", "CH", "GR", "ME", "MK", "PT", "RS", "TR", "UA", "KS",
                     "UK", "MA"),
  BiddingZone = c("ALEGrO Belgium", "ALEGrO Germany", "Austria", "Belgium", "Czech Republic", "Germany/Luxembourg",
                  "Croatia", "Hungary", "France", "Netherlands", "Poland", "Romania", "Slovenia", "Slovakia",
                  "Denmark 1", "Spain", "Italy", "Bulgaria", "Albania", "Bosnia and Herzegovina", "Switzerland",
                  "Greece", "Montenegro", "North Macedonia", "Portugal", "Serbia", "Turkey", "Ukraine", "Kosovo",
                  "United Kingdom", "Morocco")
)

read_core_sgm <- function() {
  path <- system.file("extdata", "core_sgm_6th_release.csv", package = "JAOPuTo")

  if (path == "") {
    stop("core_sgm_6th_release.csv has not been found in extdata of the JAOPuTo package.")
  }

  readr::read_csv(path, show_col_types = FALSE)
}
