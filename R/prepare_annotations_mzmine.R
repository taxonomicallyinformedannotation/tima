#' @title Prepare annotations mzmine
#'
#' @description This function prepares mzmine spectral library matching results
#'     by standardizing column names, integrating structure metadata, and
#'     formatting for downstream TIMA annotation workflows.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include select_annotations_columns.R
#'
#' @param input [character] Character string or vector of paths to mzmine annotation files
#' @param output [character] Character string path for prepared mzmine annotations output
#' @param str_stereo [character] Character string path to structures stereochemistry file
#' @param str_met [character] Character string path to structures metadata file
#' @param str_tax_cla [character] Character string path to ClassyFire taxonomy file
#' @param str_tax_npc [character] Character string path to NPClassifier taxonomy file
#'
#' @return Character string path to prepared mzmine annotations
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_annotations_mzmine()
#' unlink("data", recursive = TRUE)
#' }
prepare_annotations_mzmine <- function(
  input = get_params(
    step = "prepare_annotations_mzmine"
  )$files$annotations$raw$spectral$mzmine,
  output = get_params(
    step = "prepare_annotations_mzmine"
  )$files$annotations$prepared$structural$mzmine,
  str_stereo = get_params(
    step = "prepare_annotations_mzmine"
  )$files$libraries$sop$merged$structures$stereo,
  str_met = get_params(
    step = "prepare_annotations_mzmine"
  )$files$libraries$sop$merged$structures$metadata,
  str_tax_cla = get_params(
    step = "prepare_annotations_mzmine"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "prepare_annotations_mzmine"
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

  ctx <- log_operation("prepare_annotations_mzmine", n_files = length(input))

  if (length(input) == 0) {
    input <- "w1llN3v3r3v3r3x1st"
  }
  if (
    all(
      purrr::map(.x = input, .f = file.exists) |>
        unlist()
    )
  ) {
    table <- purrr::map2(
      .x = input,
      .y = seq_along(input),
      .f = ~ safe_fread(
        file = .x,
        file_type = paste0("mzmine results file ", .y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    ) |>
      tidytable::bind_rows() |>
      tidytable::select(
        tidyselect::any_of(
          x = c(
            "feature_id" = "id",
            "candidate_structure_name" = "compound_name",
            ## All structural identifiers (InChIKey, formula, mass,
            ## xlogp) are strictly recomputed from SMILES via
            ## process_smiles() downstream.
            "candidate_structure_smiles_no_stereo" = "smiles",
            "candidate_adduct" = "adduct",
            "candidate_score_similarity" = "score"
          )
        )
      ) |>
      # TODO find solution for these
      tidytable::mutate(
        # TODO temporary
        "candidate_library" = "mzmine",
        "candidate_structure_error_mz" = NA_real_,
        "candidate_structure_error_rt" = NA_real_
      ) |>
      tidytable::mutate(
        ## exact_mass, xlogp, formula are recomputed from SMILES
        ## via process_smiles() in select_annotations_columns().
        "candidate_count_similarity_peaks_matched" = NA_integer_,
        "candidate_structure_tax_npc_01pat" = NA_character_,
        "candidate_structure_tax_npc_02sup" = NA_character_,
        "candidate_structure_tax_npc_03cla" = NA_character_,
        "candidate_structure_tax_cla_chemontid" = NA_character_,
        "candidate_structure_tax_cla_01kin" = NA_character_,
        "candidate_structure_tax_cla_02sup" = NA_character_,
        "candidate_structure_tax_cla_03cla" = NA_character_,
        "candidate_structure_tax_cla_04dirpar" = NA_character_
      ) |>
      select_annotations_columns(
        str_stereo = str_stereo,
        str_met = str_met,
        str_tax_cla = str_tax_cla,
        str_tax_npc = str_tax_npc
      )
  } else {
    log_warn(
      "No mzmine annotations found, returning an empty file instead"
    )
    table <- fake_annotations_columns()
  }

  log_complete(ctx, n_annotations = nrow(table))

  export_params(
    parameters = get_params(step = "prepare_annotations_mzmine"),
    step = "prepare_annotations_mzmine"
  )
  export_output(x = table, file = output[[1L]])
  rm(table)
  return(output[[1L]])
}
