#' Title
#'
#' @param filename TODO
#' @param gnps_job_id TODO
#' @param candidates_initial TODO
#' @param candidates_final TODO
#' @param weight_biological TODO
#' @param weight_chemical TODO
#' @param weight_spectral TODO
#' @param ms_mode TODO
#' @param annotate TODO
#' @param tolerance_ppm TODO
#' @param tolerance_rt TODO
#' @param force TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom stringr str_length
#' @importFrom yaml read_yaml write_yaml
#'
#' @examples
prepare_params <- function(filename = params$filename,
                           gnps_job_id = params$gnps,
                           candidates_initial = params$top_k$initial,
                           candidates_final = params$top_k$final,
                           weight_biological = params$weights$biological,
                           weight_chemical = params$weights$chemical,
                           weight_spectral = params$weights$spectral,
                           ms_mode = params$ms$mode,
                           annotate = params$ms$annotate,
                           tolerance_ppm = params$ms$tolerance$ppm,
                           tolerance_rt = params$ms$tolerance$rt,
                           taxon = params$taxon,
                           force = params$force) {
  stopifnot("Your GNPS job ID is invalid" = stringr::str_length(gnps_job_id) == 32)
  stopifnot("Your ms_mode parameter must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))
  stopifnot("Your ms_annotate parameter must be 'true' or 'false'" = annotate %in% c(TRUE, FALSE))
  if (force == FALSE) {
    stopifnot("Your ppm tolerance must be lower or equal to 20" = tolerance_ppm <=
      20)
    stopifnot("Your rt tolerance must be lower or equal to 0.1" = tolerance_rt <=
      0.1)
  }

  log_debug(x = "Loading default params")
  yaml_files <- list.files(
    path = file.path(paths$config$default),
    pattern = ".yaml",
    full.names = TRUE
  )
  yaml_names <- yaml_files |>
    gsub(pattern = "config/default/", replacement = "") |>
    gsub(pattern = ".yaml", replacement = "")

  yamls_default <- lapply(
    X = yaml_files,
    FUN = yaml::read_yaml
  )
  names(yamls_default) <- yaml_names

  yamls_params <- yamls_default

  log_debug(x = "Changing params")
  yamls_params$prepare_edges$gnps <- params$gnps
  yamls_params$prepare_edges$output <-
    yamls_params$prepare_edges$output |>
    replace_gnps_job_id()
  yamls_params$prepare_features_classification$input <-
    yamls_params$prepare_features_classification$input |>
    replace_gnps_job_id()
  yamls_params$prepare_features_classification$output <-
    yamls_params$prepare_features_classification$output |>
    replace_gnps_job_id()
  yamls_params$prepare_features_components$input <-
    yamls_params$prepare_features_components$input |>
    replace_gnps_job_id()
  yamls_params$prepare_features_components$output <-
    yamls_params$prepare_features_components$output |>
    replace_gnps_job_id()
  yamls_params$prepare_features_components$gnps <- params$gnps
  yamls_params$prepare_features_components$mode <- params$ms$mode
  yamls_params$prepare_gnps$output <-
    yamls_params$prepare_gnps$output |>
    replace_gnps_job_id()
  yamls_params$prepare_gnps$gnps <- params$gnps
  yamls_params$prepare_isdb$input <-
    yamls_params$prepare_isdb$input |>
    gsub(
      pattern = yamls_default$prepare_isdb$input,
      replacement = file.path(
        paths$data$interim$annotations$path,
        paste(params$filename, "isdb.tsv.gz", sep = "_")
      )
    )
  yamls_params$prepare_isdb$output <-
    yamls_params$prepare_isdb$output |>
    replace_gnps_job_id()
  yamls_params$prepare_sirius$directory <-
    yamls_params$prepare_sirius$directory |>
    gsub(
      pattern = yamls_default$prepare_sirius$directory,
      replacement = file.path(
        paths$data$interim$annotations$path,
        paste(params$filename, "sirius/", sep = "_")
      )
    )
  yamls_params$prepare_sirius$output <-
    yamls_params$prepare_sirius$output |>
    replace_gnps_job_id()
  yamls_params$prepare_taxa$output <-
    yamls_params$prepare_taxa$output |>
    replace_gnps_job_id()
  yamls_params$prepare_taxa$gnps <- params$gnps

  yamls_params$process_annotations$annotation$gnps <-
    yamls_params$process_annotations$annotation$gnps |>
    replace_gnps_job_id()
  yamls_params$process_annotations$annotation$isdb <-
    yamls_params$process_annotations$annotation$isdb |>
    replace_gnps_job_id()
  yamls_params$process_annotations$annotation$sirius <-
    yamls_params$process_annotations$annotation$sirius |>
    replace_gnps_job_id()
  yamls_params$process_annotations$taxa <-
    yamls_params$process_annotations$taxa |>
    replace_gnps_job_id()
  yamls_params$process_annotations$edges <-
    yamls_params$process_annotations$taxa |>
    replace_gnps_job_id()
  yamls_params$process_annotations$top_k$initial <-
    params$top_k$initial
  yamls_params$process_annotations$top_k$final <- params$top_k$final
  yamls_params$process_annotations$weights$biological <-
    params$weights$biological
  yamls_params$process_annotations$weights$chemical <-
    params$weights$chemical
  yamls_params$process_annotations$weights$spectral <-
    params$weights$spectral
  yamls_params$process_annotations$ms$mode <- params$ms$mode
  yamls_params$process_annotations$ms$annotate <- params$ms$annotate
  yamls_params$process_annotations$ms$tolerance$ppm <-
    params$ms$tolerance$ppm
  yamls_params$process_annotations$ms$tolerance$rt <-
    params$ms$tolerance$rt
  yamls_params$process_annotations$force <- params$force

  yaml_export <- yaml_files |>
    gsub(pattern = "default", replacement = "params")

  log_debug(x = "Exporting params ...")
  log_debug(x = "... checking directory")
  ifelse(
    test = !dir.exists(dirname(yaml_export[[1]])),
    yes = dir.create(dirname(yaml_export[[1]])),
    no = paste(dirname(yaml_export[[1]]), "exists")
  )
  log_debug(x = "Exporting")
  lapply(
    X = seq_along(yamls_params),
    FUN = function(x) {
      yaml::write_yaml(
        x = yamls_params[[x]],
        file = yaml_export[x]
      )
    }
  )
}
