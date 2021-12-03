#' Title
#'
#' @param input TODO
#' @param output TODO
#' @param tool TODO
#' @param components TODO
#' @param gnps_job_id TODO
#' @param mode TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_features_components <- function(input = params$input,
                                        output = params$output,
                                        tool = params$tool,
                                        components = params$components,
                                        gnps_job_id = params$gnps,
                                        mode = params$mode) {
  log_debug(x = "Loading files ...")
  log_debug(x = "... features table")
  table <- readr::read_delim(file = input)

  log_debug(x = "... cluster table")
  log_debug(x = "THIS STEP CAN BE IMPROVED BY CALCULATING THE CLUSTERS WITHIN SPEC2VEC")
  ## TODO
  components <-
    read_clusters(id = gnps_job_id) |>
    dplyr::select(
      feature_id = `cluster index`,
      component_id = componentindex,
      rt = RTMean,
      mz = `precursor mass`
    ) |>
    dplyr::distinct()

  if (tool == "manual") {
    manual_components <-
      readr::read_delim(file = components) |>
      dplyr::distinct(
        feature_id = CLUSTERID1,
        component_id = ComponentIndex
      )

    components <- components |>
      dplyr::select(-component_id) |>
      left_join(manual_components) |>
      dplyr::mutate(component_id = ifelse(
        test = is.na(component_id),
        yes = -1,
        no = component_id
      ))
  }

  log_debug(x = "Adding components to features")
  table_filled <-
    dplyr::left_join(components, table) |>
    dplyr::distinct() |>
    dplyr::arrange(desc(score_input)) |>
    dplyr::arrange(as.numeric(feature_id)) |>
    dplyr::select(
      feature_id,
      component_id,
      rt,
      mz,
      inchikey_2D,
      smiles_2D,
      molecular_formula,
      structure_exact_mass,
      score_input,
      library,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class
    )

  log_debug(x = "Calculating mz error")
  ## TODO can be improved
  if (mode == "pos") {
    table_filled <- table_filled |>
      dplyr::mutate(mz_error = mz - 1.007276 - structure_exact_mass)
  } else {
    table_filled <- table_filled |>
      dplyr::mutate(mz_error = mz + 1.007276 - structure_exact_mass)
  }

  log_debug(x = "Exporting ...")
  ifelse(
    test = !dir.exists(paths$data$interim$path),
    yes = dir.create(paths$data$interim$path),
    no = paste(paths$data$interim$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$annotations$path),
    yes = dir.create(paths$data$interim$annotations$path),
    no = paste(paths$data$interim$annotations$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$config$path),
    yes = dir.create(paths$data$interim$config$path),
    no = paste(paths$data$interim$config$path, "exists")
  )
  ifelse(
    test = !dir.exists(dirname(output)),
    yes = dir.create(dirname(output)),
    no = paste(dirname(output), "exists")
  )

  log_debug(
    x = "... path to export is",
    output
  )
  readr::write_delim(
    x = table_filled,
    file = output,
    delim = "\t"
  )

  export_params(
    parameters = params,
    directory = paths$data$interim$config$path,
    step = step
  )
}
