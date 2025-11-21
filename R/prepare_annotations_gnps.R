#' @title Prepare annotations GNPS
#'
#' @description This function prepares GNPS spectral library matching results
#'     by standardizing column names, integrating structure metadata, and
#'     formatting for downstream TIMA annotation workflows.
#'
#' @include get_params.R
#' @include select_annotations_columns.R
#'
#' @param input Character string or vector of paths to GNPS annotation files
#' @param output Character string path for prepared GNPS annotations output
#' @param str_stereo Character string path to structures stereochemistry file
#' @param str_met Character string path to structures metadata file
#' @param str_nam Character string path to structures names file
#' @param str_tax_cla Character string path to ClassyFire taxonomy file
#' @param str_tax_npc Character string path to NPClassifier taxonomy file
#'
#' @return Character string path to prepared GNPS annotations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_annotations_gnps()
#' unlink("data", recursive = TRUE)
#' }
prepare_annotations_gnps <- function(
  input = get_params(
    step = "prepare_annotations_gnps"
  )$files$annotations$raw$spectral$gnps,
  output = get_params(
    step = "prepare_annotations_gnps"
  )$files$annotations$prepared$structural$gnps,
  str_stereo = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$stereo,
  str_met = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$metadata,
  str_nam = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$names,
  str_tax_cla = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$taxonomies$npc
) {
  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  # Validate structure file paths
  str_files <- list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )

  for (param_name in names(str_files)) {
    param_value <- str_files[[param_name]]
    if (!is.character(param_value) || length(param_value) != 1L) {
      stop(param_name, " must be a single character string")
    }
    if (!file.exists(param_value)) {
      stop(param_name, " file not found: ", param_value)
    }
  }

  logger::log_info("Preparing GNPS annotations")

  if (length(input) == 0) {
    input <- "w1llN3v3r3v3r3x1st"
  }
  if (
    all(
      purrr::map(.x = input, .f = file.exists) |>
        unlist()
    )
  ) {
    # logger::log_trace("Loading and formatting GNPS results")
    ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
    table <- purrr::map(
      .x = input,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows() |>
      tidytable::mutate(
        candidate_structure_error_mz = as.numeric(MZErrorPPM) *
          1E-6 *
          as.numeric(Precursor_MZ)
      ) |>
      tidytable::select(
        tidyselect::any_of(
          x = c(
            "feature_id" = "#Scan#",
            "candidate_adduct" = "Adduct",
            "candidate_structure_error_mz" = "MassDiff",
            "candidate_library" = "LibraryName",
            "candidate_structure_name" = "Compound_Name",
            "candidate_score_similarity" = "MQScore",
            "candidate_count_similarity_peaks_matched" = "SharedPeaks",
            "candidate_structure_inchi" = "INCHI",
            "candidate_structure_inchikey" = "InChIKey",
            "candidate_structure_inchikey_connectivity_layer" = "InChIKey-Planar",
            "candidate_structure_tax_npc_01pat" = "npclassifier_pathway",
            "candidate_structure_tax_npc_02sup" = "npclassifier_superclass",
            "candidate_structure_tax_npc_03cla" = "npclassifier_class",
            "candidate_structure_exact_mass" = "ExactMass",
            ## Only partially present
            "candidate_structure_tax_cla_02sup" = "superclass",
            "candidate_structure_tax_cla_03cla" = "class",
            "candidate_structure_tax_cla_04dirpar" = "subclass"
          )
        )
      ) |>
      tidytable::mutate(
        candidate_structure_smiles_no_stereo = NA,
        candidate_structure_molecular_formula = candidate_structure_inchi |>
          ## really dirty
          gsub(
            pattern = ".*\\/C",
            replacement = "C",
            perl = TRUE
          ) |>
          gsub(
            pattern = "\\/.*",
            replacement = "",
            perl = TRUE
          ),
        candidate_structure_xlogp = NA,
        ## Only partially present
        candidate_structure_tax_cla_chemontid = NA,
        candidate_structure_tax_cla_01kin = NA
      ) |>
      select_annotations_columns()
  } else {
    logger::log_warn(
      "No GNPS annotations found, returning an empty file instead"
    )
    table <- fake_annotations_columns()
  }

  export_params(
    parameters = get_params(step = "prepare_annotations_gnps"),
    step = "prepare_annotations_gnps"
  )
  export_output(x = table, file = output[[1]])
  rm(table)
  return(output[[1]])
}
