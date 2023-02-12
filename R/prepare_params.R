#' @title Prepare parameters
#'
#' @description This function prepares main parameters
#'
#' @param filename Name of the file
#' @param gnps_job_id GNPS job ID
#' @param ms_mode MS ionization mode. 'pos' or 'neg'
#' @param taxon Name of a taxon you want to enforce
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom stringr str_length
#' @importFrom yaml read_yaml write_yaml
#'
#' @examples NULL
prepare_params <- function(filename = params$files$pattern,
                           gnps_job_id = params$gnps$id,
                           ms_mode = params$ms$polarity,
                           taxon = params$organisms$taxon) {
  ## TODO Filename actually not taken into account

  stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
  stopifnot("Your ms_mode parameter must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))

  log_debug(x = "Loading default params")
  yaml_files <- list.files(
    path = file.path(paths$config$default),
    pattern = ".yaml",
    full.names = TRUE
  )
  yaml_names <- yaml_files |>
    gsub(pattern = "config/default/", replacement = "") |>
    gsub(pattern = ".yaml", replacement = "")

  yamls_default <<- lapply(X = yaml_files,
                           FUN = yaml::read_yaml)
  names(yamls_default) <<- yaml_names

  yamls_params <- yamls_default

  log_debug(x = "Changing params")
  yamls_params$prepare_edges$gnps$id <- gnps_job_id
  yamls_params$prepare_gnps$gnps$id <- gnps_job_id
  yamls_params$prepare_taxa$gnps$id <- gnps_job_id

  yamls_params$prepare_edges$files$networks$spectral$edges$processed <-
    yamls_params$prepare_edges$files$networks$spectral$edges$processed |>
    replace_gnps_job_id()
  yamls_params$prepare_features_classification$files$annotations$filled <-
    yamls_params$prepare_features_classification$files$annotations$filled |>
    replace_gnps_job_id()
  yamls_params$prepare_features_classification$files$annotations$treated <-
    yamls_params$prepare_features_classification$files$annotations$treated |>
    replace_gnps_job_id()
  yamls_params$prepare_features_components$files$annotations$pretreated <-
    yamls_params$prepare_features_components$files$annotations$pretreated |>
    replace_gnps_job_id()
  yamls_params$prepare_features_components$files$annotations$filled <-
    yamls_params$prepare_features_components$files$annotations$filled |>
    replace_gnps_job_id()
  yamls_params$prepare_features_components$gnps$id <- gnps_job_id
  yamls_params$prepare_gnps$files$annotations$pretreated <-
    yamls_params$prepare_gnps$files$annotations$pretreated |>
    replace_gnps_job_id()
  yamls_params$prepare_spectral_matches$files$annotations$raw$spectral <-
    yamls_params$prepare_spectral_matches$files$annotations$raw$spectral |>
    gsub(
      pattern = yamls_params$prepare_spectral_matches$files$annotations$raw$spectral,
      replacement = file.path(
        paths$data$interim$annotations$path,
        paste(params$files$pattern, "isdb.tsv.gz", sep = "_")
      )
    )
  yamls_params$prepare_spectral_matches$files$annotations$pretreated <-
    yamls_params$prepare_spectral_matches$files$annotations$pretreated |>
    replace_gnps_job_id()
  yamls_params$prepare_sirius$files$annotations$raw$sirius <-
    yamls_params$prepare_sirius$files$annotations$raw$sirius |>
    gsub(
      pattern = yamls_params$prepare_sirius$files$annotations$raw$sirius,
      replacement = file.path(
        paths$data$interim$annotations$path,
        paste(params$files$pattern, "sirius/", sep = "_")
      )
    )
  yamls_params$prepare_sirius$files$annotations$pretreated <-
    yamls_params$prepare_sirius$files$annotations$pretreated |>
    replace_gnps_job_id()
  yamls_params$prepare_taxa$files$taxa$processed <-
    yamls_params$prepare_taxa$files$taxa$processed |>
    replace_gnps_job_id()
  yamls_params$process_annotations$files$networks$spectral$edges <-
    yamls_params$process_annotations$files$networks$spectral$edges |>
    replace_gnps_job_id()
  yamls_params$process_annotations$files$taxa$processed <-
    yamls_params$process_annotations$files$taxa$processed |>
    replace_gnps_job_id()

  yamls_params$fake_features_components$ms$polarity <- ms_mode
  yamls_params$prepare_features_components$ms$polarity <- ms_mode
  yamls_params$process_annotations$ms$polarity <- ms_mode

  yamls_params$prepare_taxa$organisms$taxon <- taxon

  yaml_export <- yaml_files |>
    gsub(pattern = "default", replacement = "params")

  log_debug(x = "Exporting params ...")
  log_debug(x = "... checking directory")
  create_dir(export = yaml_export[[1]])

  log_debug(x = "Exporting")
  lapply(
    X = seq_along(yamls_params),
    FUN = function(x) {
      yaml::write_yaml(x = yamls_params[[x]],
                       file = yaml_export[x])
    }
  )
}
