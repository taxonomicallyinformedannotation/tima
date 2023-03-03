#' @title Prepare params
#'
#' @description This function prepares main parameters
#'
#' @param filename Name of the file
#' @param gnps_job_id GNPS job ID
#' @param ms_mode MS ionization mode. 'pos' or 'neg'
#' @param taxon Name of a taxon you want to enforce
#' @param parameters Params
#' @param step Step
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_params <- function(filename = params$files$pattern,
                           gnps_job_id = params$gnps$id,
                           ms_mode = params$ms$polarity,
                           taxon = params$organisms$taxon,
                           parameters = params,
                           step = NA) {
  ## TODO 'step' actually not taken into account

  stopifnot(
    "Either 'filename' or 'GNPS' have to be filled" = !is.null(filename) |
      !is.null(gnps_job_id)
  )
  stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
  stopifnot("Your ms_mode parameter must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))

  params <<- parameters
  gnps_job_id <<- gnps_job_id
  filename <<- filename
  paths <- parse_yaml_paths()
  log_debug(x = "Loading default params")
  yaml_files <- c(
    list.files(
      path = file.path(paths$params$default),
      pattern = ".yaml",
      full.names = TRUE
    ),
    paths$params$prepare_params
  )

  yaml_names <- yaml_files |>
    gsub(pattern = "params/default/", replacement = "") |>
    gsub(pattern = ".yaml", replacement = "")

  yamls_default <- lapply(
    X = yaml_files,
    FUN = yaml::read_yaml
  )
  names(yamls_default) <- yaml_names

  yamls_params <- yamls_default

  log_debug(x = "Changing params")

  yamls_params$annotate_masses$ms$polarity <- ms_mode

  yamls_params$prepare_taxa$organisms$taxon <- taxon

  log_debug(x = "Changing filenames")

  yamls_params$annotate_masses$files$annotations$pretreated <-
    yamls_params$annotate_masses$files$annotations$pretreated |>
    replace_id()
  yamls_params$annotate_masses$files$features$prepared <-
    yamls_params$annotate_masses$files$features$prepared |>
    replace_id()
  yamls_params$annotate_masses$files$networks$spectral$edges$raw <-
    yamls_params$annotate_masses$files$networks$spectral$edges$raw |>
    replace_id()

  yamls_params$annotate_spectra$files$annotations$raw$spectral <-
    yamls_params$annotate_spectra$files$annotations$raw$spectral |>
    replace_id()
  yamls_params$annotate_spectra$files$spectral$raw <-
    yamls_params$annotate_spectra$files$spectral$raw |>
    replace_id()

  yamls_params$create_edges_spectra$files$networks$spectral$edges$raw <-
    yamls_params$create_edges_spectra$files$networks$spectral$edges$raw |>
    replace_id()
  yamls_params$create_edges_spectra$files$spectral$raw <-
    yamls_params$create_edges_spectra$files$spectral$raw |>
    replace_id()

  yamls_params$prepare_features_tables$files$features$raw <-
    yamls_params$prepare_features_tables$files$features$raw |>
    replace_id()
  yamls_params$prepare_features_tables$files$features$prepared <-
    yamls_params$prepare_features_tables$files$features$prepared |>
    replace_id()

  yamls_params$prepare_features_components$files$annotations$pretreated <-
    yamls_params$prepare_features_components$files$annotations$pretreated |>
    replace_id()
  yamls_params$prepare_features_components$files$annotations$filled <-
    yamls_params$prepare_features_components$files$annotations$filled |>
    replace_id()
  yamls_params$prepare_features_components$files$networks$spectral$components$raw <-
    yamls_params$prepare_features_components$files$networks$spectral$components$raw |>
    replace_id()

  yamls_params$prepare_features_edges$files$networks$spectral$edges$raw <-
    yamls_params$prepare_features_edges$files$networks$spectral$edges$raw |>
    replace_id()
  yamls_params$prepare_features_edges$files$networks$spectral$edges$processed <-
    yamls_params$prepare_features_edges$files$networks$spectral$edges$processed |>
    replace_id()

  yamls_params$prepare_annotations_gnps$files$annotations$pretreated <-
    yamls_params$prepare_annotations_gnps$files$annotations$pretreated |>
    replace_id()

  yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius <-
    yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius |>
    replace_id()
  yamls_params$prepare_annotations_sirius$files$annotations$pretreated <-
    yamls_params$prepare_annotations_sirius$files$annotations$pretreated |>
    replace_id()

  yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral <-
    yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral |>
    replace_id()
  yamls_params$prepare_annotations_spectra$files$annotations$pretreated <-
    yamls_params$prepare_annotations_spectra$files$annotations$pretreated |>
    replace_id()

  yamls_params$prepare_taxa$files$taxa$processed <-
    yamls_params$prepare_taxa$files$taxa$processed |>
    replace_id()

  yamls_params$weight_annotations$files$annotations$filled <-
    yamls_params$weight_annotations$files$annotations$filled |>
    replace_id()
  yamls_params$weight_annotations$files$annotations$processed <-
    yamls_params$weight_annotations$files$annotations$processed |>
    replace_id()
  yamls_params$weight_annotations$files$networks$spectral$edges$processed <-
    yamls_params$weight_annotations$files$networks$spectral$edges$processed |>
    replace_id()
  yamls_params$weight_annotations$files$taxa$processed <-
    yamls_params$weight_annotations$files$taxa$processed |>
    replace_id()

  yaml_export <- yaml_files |>
    gsub(pattern = "default", replacement = "user")
  names(yaml_export) <- yaml_names

  if (!is.na(step)) {
    ## The dollar is for steps having similar names separated by underscores
    yamls_params <-
      yamls_params[grepl(
        pattern = paste0(step, "$"),
        x = names(yamls_params)
      )]
    ## The dot is for steps having similar names separated by underscores
    yaml_export <-
      yaml_export[grepl(pattern = paste0(step, "\\."), x = yaml_export)]
  }

  log_debug(x = "Exporting params ...")
  log_debug(x = "... checking directory")
  create_dir(export = yaml_export[[1]])

  log_debug(x = "Exporting")
  export_params(step = "prepare_params")
  lapply(
    X = seq_along(yamls_params),
    FUN = function(x) {
      yaml::write_yaml(
        x = yamls_params[[x]],
        file = yaml_export[x]
      )
    }
  )
  return(yaml_export)
}
