#' Title
#'
#' @noRd
#'
#' @param x TODO
#'
#' @return TODO
#' @export
#'
#' @examples
replace_gnps_job_id <- function(x) {
  x <- x |>
    gsub(
      pattern = yamls_default$prepare_params$gnps,
      replacement = params$gnps
    )
  return(x)
}
