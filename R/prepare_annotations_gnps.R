#' @title Prepare annotations GNPS
#'
#' @description This function prepares GNPS spectral library matching results
#'     by standardizing column names, integrating structure metadata, and
#'     formatting for downstream TIMA annotation workflows.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include select_annotations_columns.R
#'
#' @param input [character] Character string or vector of paths to GNPS annotation files
#' @param output [character] Character string path for prepared GNPS annotations output
#' @param str_stereo [character] Character string path to structures stereochemistry file
#' @param str_met [character] Character string path to structures metadata file
#' @param str_tax_cla [character] Character string path to ClassyFire taxonomy file
#' @param str_tax_npc [character] Character string path to NPClassifier taxonomy file
#'
#' @return Character string path to prepared GNPS annotations
#'
#' @family preparation
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
  str_tax_cla = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "prepare_annotations_gnps"
  )$files$libraries$sop$merged$structures$taxonomies$npc
) {
  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate all structure file paths
  validate_file_existence(
    file_list = list(
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    ),
    allow_null = FALSE
  )

  ctx <- log_operation("prepare_annotations_gnps", n_files = length(input))

  if (length(input) == 0) {
    input <- "w1llN3v3r3v3r3x1st"
  }
  if (
    all(
      purrr::map(.x = input, .f = file.exists) |>
        unlist()
    )
  ) {
    # log_trace("Loading and formatting GNPS results")
    ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
    table <- purrr::map2(
      .x = input,
      .y = seq_along(input),
      .f = ~ safe_fread(
        file = .x,
        file_type = paste0("GNPS results file ", .y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
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
            ## All structural identifiers (InChIKey, formula, mass,
            ## xlogp) are strictly recomputed from SMILES via
            ## process_smiles() downstream.  Only the SMILES is kept
            ## as single source of truth for structure identity.
            "candidate_structure_smiles_no_stereo" = "Smiles",
            "candidate_structure_tax_npc_01pat" = "npclassifier_pathway",
            "candidate_structure_tax_npc_02sup" = "npclassifier_superclass",
            "candidate_structure_tax_npc_03cla" = "npclassifier_class",
            ## Only partially present
            "candidate_structure_tax_cla_02sup" = "superclass",
            "candidate_structure_tax_cla_03cla" = "class",
            "candidate_structure_tax_cla_04dirpar" = "subclass"
          )
        )
      ) |>
      tidytable::mutate(
        ## Only partially present
        candidate_structure_tax_cla_chemontid = NA,
        candidate_structure_tax_cla_01kin = NA
      ) |>
      select_annotations_columns(
        str_stereo = str_stereo,
        str_met = str_met,
        str_tax_cla = str_tax_cla,
        str_tax_npc = str_tax_npc
      )
  } else {
    log_warn(
      "No GNPS annotations found, returning an empty file instead"
    )
    table <- fake_annotations_columns()
  }

  log_complete(ctx, n_annotations = nrow(table))

  export_params(
    parameters = get_params(step = "prepare_annotations_gnps"),
    step = "prepare_annotations_gnps"
  )
  export_output(x = table, file = output[[1L]])
  rm(table)
  return(output[[1L]])
}
