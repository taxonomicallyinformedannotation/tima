#' @title Replace GNPS job ID
#'
#' @noRd
#'
#' @param x TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
replace_gnps_job_id <- function(x) {
  x <- x |>
    gsub(
      pattern = yamls_default$prepare_params$gnps,
      replacement = params$gnps
    )
  return(x)
}
