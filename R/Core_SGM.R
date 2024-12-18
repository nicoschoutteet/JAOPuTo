#' Core - SGM
#'
#' @return a dataframe containing the Core Static Grid Model, including lines and transformers (source: SGM 6th release)
#' @import readr
#' @export
#'
#' @examples JAOPuTo_Core_SGM()
JAOPuTo_Core_SGM <- function() {

  SGM_path <- system.file("extdata", "Core SGM 6th release.csv", package = "JAOPuTo")

  if (SGM_path == "") {
    stop("Static Grid Model not found. Please ensure hte CSV is included in the package.")
  }

  df_SGM <- read_csv(SGM_path)

  return(df_SGM)

}
