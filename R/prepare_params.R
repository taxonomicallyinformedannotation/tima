#' @title Prepare params
#'
#' @description This function prepares main parameters
#'
#' @include create_dir.R
#' @include export_params.R
#' @include load_yaml_files.R
#'
#' @param filename Name of the file
#' @param features File containing the features table
#' @param spectra File containing the spectra
#' @param gnps_job_id GNPS job ID
#' @param gnps_example_id GNPS example job ID
#' @param ms_mode MS ionization mode. 'pos' or 'neg'
#' @param taxon Name of a taxon you want to enforce
#' @param summarise Summarise results to one row per feature. BOOLEAN
#' @param step Step
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_params <- function(filename = get_params(step = "prepare_params")$files$pattern,
                           features = get_params(step = "prepare_params")$files$features$raw,
                           spectra = get_params(step = "prepare_params")$files$spectral$raw,
                           gnps_job_id = get_params(step = "prepare_params")$gnps$id,
                           gnps_example_id = parse_yaml_paths()$gnps$example,
                           ms_mode = get_params(step = "prepare_params")$ms$polarity,
                           taxon = get_params(step = "prepare_params")$organisms$taxon,
                           summarise = get_params(step = "prepare_params")$options$summarise,
                           step = NA) {
  ## TODO 'step' actually not taken into account
  params <- parameters
  filename <<- filename
  features <- features
  spectra <- spectra
  gnps_job_id <<- gnps_job_id
  gnps_example_id <<- gnps_example_id
  ms_mode <- ms_mode
  taxon <- taxon
  summarise <- summarise

  stopifnot(
    "Either 'filename' or 'GNPS' have to be filled" = !is.null(filename) |
      !is.null(gnps_job_id)
  )
  if (!is.null(gnps_job_id)) {
    if (gnps_job_id != "") {
      stopifnot(
        "Your GNPS job ID is invalid" =
          stringi::stri_length(
            str = gnps_job_id
          ) == 32
      )
    }
  }
  stopifnot(
    "Your ms_mode parameter must be 'pos' or 'neg'" =
      ms_mode %in% c("pos", "neg")
  )

  list <- load_yaml_files()
  yamls_params <- list[[1]]

  log_debug(x = "Changing params")

  yamls_params$annotate_masses$ms$polarity <- ms_mode
  yamls_params$annotate_spectra$ms$polarity <- ms_mode

  yamls_params$prepare_taxa$organisms$taxon <- taxon

  yamls_params$weight_annotations$options$summarise <- summarise
  yamls_params$weight_annotations$files$pattern <- filename

  log_debug(x = "Changing filenames")

  yamls_params$
    annotate_masses$
    files$
    annotations$
    prepared <-
    yamls_params$
    annotate_masses$
    files$
    annotations$
    prepared |>
    lapply(FUN = replace_id)
  yamls_params$
    annotate_masses$
    files$
    features$
    prepared <-
    yamls_params$
    annotate_masses$
    files$
    features$
    prepared |>
    lapply(FUN = replace_id)
  yamls_params$
    annotate_masses$
    files$
    networks$
    spectral$
    edges$
    raw <-
    yamls_params$
    annotate_masses$
    files$
    networks$
    spectral$
    edges$
    raw |>
    lapply(FUN = replace_id)

  yamls_params$
    annotate_spectra$
    files$
    annotations$
    raw$
    spectral <-
    yamls_params$
    annotate_spectra$
    files$
    annotations$
    raw$
    spectral |>
    lapply(FUN = replace_id)

  yamls_params$annotate_spectra$files$spectral$raw <- spectra
  # yamls_params$annotate_spectra$files$spectral$raw <-
  #   yamls_params$annotate_spectra$files$spectral$raw |>
  #   lapply(FUN = replace_id)

  yamls_params$
    create_edges_spectra$
    files$
    networks$
    spectral$
    edges$
    raw <-
    yamls_params$
    create_edges_spectra$
    files$
    networks$
    spectral$
    edges$
    raw |>
    lapply(FUN = replace_id)
  yamls_params$
    create_edges_spectra$
    files$
    spectral$
    raw <- spectra
  # yamls_params$create_edges_spectra$files$spectral$raw <-
  #   yamls_params$create_edges_spectra$files$spectral$raw |>
  #   lapply(FUN = replace_id)

  yamls_params$
    create_components$
    files$
    networks$
    spectral$
    edges$
    prepared <-
    yamls_params$
    create_components$
    files$
    networks$
    spectral$
    edges$
    prepared |>
    lapply(FUN = replace_id)
  yamls_params$
    create_components$
    files$
    networks$
    spectral$
    components$
    raw <-
    yamls_params$
    create_components$
    files$
    networks$
    spectral$
    components$
    raw |>
    lapply(FUN = replace_id)

  yamls_params$
    prepare_features_tables$
    files$
    features$
    raw <- features
  # yamls_params$prepare_features_tables$files$features$raw <-
  #   yamls_params$prepare_features_tables$files$features$raw |>
  #   lapply(FUN = replace_id)
  yamls_params$
    prepare_features_tables$
    files$
    features$
    prepared <-
    yamls_params$
    prepare_features_tables$
    files$
    features$
    prepared |>
    lapply(FUN = replace_id)

  yamls_params$
    prepare_features_components$
    files$
    networks$
    spectral$
    components$
    raw <-
    yamls_params$
    prepare_features_components$
    files$
    networks$
    spectral$
    components$
    raw |>
    lapply(FUN = replace_id)
  yamls_params$
    prepare_features_components$
    files$
    networks$
    spectral$
    components$
    prepared <-
    yamls_params$
    prepare_features_components$
    files$
    networks$
    spectral$
    components$
    prepared |>
    lapply(FUN = replace_id)

  yamls_params$
    prepare_features_edges$
    files$
    networks$
    spectral$
    edges$
    raw <-
    yamls_params$
    prepare_features_edges$
    files$
    networks$
    spectral$
    edges$
    raw |>
    lapply(FUN = replace_id)
  yamls_params$
    prepare_features_edges$
    files$
    networks$
    spectral$
    edges$
    prepared <-
    yamls_params$
    prepare_features_edges$
    files$
    networks$
    spectral$
    edges$
    prepared |>
    lapply(FUN = replace_id)

  yamls_params$
    prepare_annotations_gnps$
    files$
    annotations$
    raw$
    spectral <-
    yamls_params$
    prepare_annotations_gnps$
    files$
    annotations$
    raw$
    spectral |>
    lapply(FUN = replace_id)
  yamls_params$
    prepare_annotations_gnps$
    files$
    annotations$
    prepared <-
    yamls_params$
    prepare_annotations_gnps$
    files$
    annotations$
    prepared |>
    lapply(FUN = replace_id)

  yamls_params$
    prepare_annotations_sirius$
    files$
    annotations$
    raw$
    sirius <-
    yamls_params$
    prepare_annotations_sirius$
    files$
    annotations$
    raw$
    sirius |>
    lapply(FUN = replace_id)
  yamls_params$
    prepare_annotations_sirius$
    files$
    annotations$
    prepared <-
    yamls_params$
    prepare_annotations_sirius$
    files$
    annotations$
    prepared |>
    lapply(FUN = replace_id)

  yamls_params$
    prepare_annotations_spectra$
    files$
    annotations$
    raw$
    spectral <-
    yamls_params$
    prepare_annotations_spectra$
    files$
    annotations$
    raw$
    spectral |>
    lapply(FUN = replace_id)
  yamls_params$
    prepare_annotations_spectra$
    files$
    annotations$
    prepared <-
    yamls_params$
    prepare_annotations_spectra$
    files$
    annotations$
    prepared |>
    lapply(FUN = replace_id)

  yamls_params$prepare_taxa$files$features$raw <- features
  # yamls_params$prepare_taxa$files$features$raw <-
  #   yamls_params$prepare_taxa$files$features$raw |>
  #   lapply(FUN = replace_id)
  # yamls_params$prepare_taxa$files$taxa$raw <-
  #   yamls_params$prepare_taxa$files$taxa$raw |>
  #   lapply(FUN = replace_id)
  yamls_params$prepare_taxa$files$taxa$prepared <-
    yamls_params$prepare_taxa$files$taxa$prepared |>
    lapply(FUN = replace_id)

  yamls_params$
    weight_annotations$
    files$
    annotations$
    prepared <-
    yamls_params$
    weight_annotations$
    files$
    annotations$
    prepared |>
    lapply(FUN = replace_id)
  # yamls_params$weight_annotations$files$annotations$processed <-
  #   yamls_params$weight_annotations$files$annotations$processed |>
  #   lapply(FUN = replace_id)
  yamls_params$
    weight_annotations$
    files$
    features$
    prepared <-
    yamls_params$
    weight_annotations$
    files$
    features$
    prepared |>
    lapply(FUN = replace_id)
  yamls_params$
    weight_annotations$
    files$
    networks$
    spectral$
    components$
    prepared <-
    yamls_params$
    weight_annotations$
    files$
    networks$
    spectral$
    components$
    prepared |>
    lapply(FUN = replace_id)
  yamls_params$
    weight_annotations$
    files$
    networks$
    spectral$
    edges$
    prepared <-
    yamls_params$
    weight_annotations$
    files$
    networks$
    spectral$
    edges$
    prepared |>
    lapply(FUN = replace_id)
  yamls_params$
    weight_annotations$
    files$
    taxa$
    prepared <-
    yamls_params$
    weight_annotations$
    files$
    taxa$
    prepared |>
    lapply(FUN = replace_id)

  yaml_export <- list[[2]] |>
    gsub(pattern = "default", replacement = "user")
  names(yaml_export) <- list[[3]]

  if (!is.na(step)) {
    ## The dollar is for steps having similar names separated by underscores
    yamls_params <-
      yamls_params[grepl(
        pattern = paste0(step[[1]], "$"),
        x = names(yamls_params)
      )]
    ## The dot is for steps having similar names separated by underscores
    yaml_export <-
      yaml_export[grepl(pattern = paste0(step[[1]], "\\."), x = yaml_export)]
  }

  log_debug(x = "Exporting params ...")
  create_dir(export = yaml_export[[1]])
  # export_params(step = "prepare_params")
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
