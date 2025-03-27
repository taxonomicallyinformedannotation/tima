#' @title Get Spectra ids
#'
#' @description This function extracts spectra IDs as they are yet not consistently named
#'
#' @param spectra Spectra.
#'
#' @return NULL
#'
#' @examples NULL
get_spectra_ids <- function(spectra) {
  ## ISSUE see #148 find a way to have consistency in spectrum IDs
  possible_ids <- c("SLAW_ID", "FEATURE_ID", "acquisitionNum", "spectrum_id")

  valid_field <- purrr::detect(
    possible_ids,
    ~ !is.null(spectra@backend@spectraData[[.x]])
  )

  if (!is.null(valid_field)) {
    return(spectra@backend@spectraData[[valid_field]])
  } else {
    return(NULL)
  }
}
