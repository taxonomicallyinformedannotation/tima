#' @title Replace GNPS job ID
#'
#' @param x a character string containing a GNPS job ID
#' @param default a default values for a GNPS job ID
#' @param params a user-specified values for a GNPS job ID
#'
#' @return a character string with the GNPS job ID modified according to the rules specified in the function
#'
#' @export
#'
#' @examples replaced_job_id <- replace_gnps_job_id("MyGNPSJobID", yamls_default, params)
replace_gnps_job_id <- function(x, default = yamls_default$prepare_params$gnps, params = params$gnps) {
  # Replace the default GNPS job ID with the user-specified GNPS job ID
  replaced_job_id <- gsub(
    pattern = default,
    replacement = params,
    x = x
  )

  return(replaced_job_id)
}
