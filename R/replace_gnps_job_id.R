#' @title Replace GNPS job ID
#'
#' @description This function replaces the default GNPS job ID given in the example by a user-specified one
#'
#' @param job a character string containing a GNPS job ID
#' @param default a default values for a GNPS job ID
#' @param repl a user-specified values for a GNPS job ID
#'
#' @return Character string with the GNPS job ID modified according to the rules specified in the function
#'
#' @export
#'
#' @examples replaced_job_id <- replace_gnps_job_id(job = "MyGNPSJobIDXXX", default = "XXX", repl = "Foo")
replace_gnps_job_id <-
  function(job,
           default = yamls_default$`config/prepare_config`$gnps$id,
           repl = params$gnps$id) {
    # Replace the default GNPS job ID with the user-specified GNPS job ID
    replaced_job_id <- gsub(
      pattern = default,
      replacement = repl,
      x = job
    )

    return(replaced_job_id)
  }
