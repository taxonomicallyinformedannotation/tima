#' @title Prepare annotations MS2
#'
#' @description This function prepares MS2 spectral library matching results
#'     by standardizing column names, integrating structure metadata, and
#'     formatting for downstream TIMA annotation workflows. Handles various
#'     spectral matching result formats.
#'
#' @include get_params.R
#' @include predicates_utils.R
#' @include select_annotations_columns.R
#'
#' @param input [character] Character string path to spectral matching results file
#' @param output [character] Character string path for prepared spectral annotations output
#' @param str_stereo [character] Character string path to structures stereochemistry file
#' @param str_met [character] Character string path to structures metadata file
#' @param str_tax_cla [character] Character string path to ClassyFire taxonomy file
#' @param str_tax_npc [character] Character string path to NPClassifier taxonomy file
#'
#' @return Character string path to prepared spectral annotations
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' data_interim <- "data/interim/"
#' dir <- paste0(github, repo)
#' input <- get_params(step = "prepare_annotations_spectra")$files$annotations$raw$spectral$spectral |>
#'   gsub(pattern = ".tsv.gz", replacement = "_pos.tsv", fixed = TRUE)
#' get_file(url = paste0(dir, input), export = input)
#' dir <- paste0(dir, data_interim)
#' prepare_annotations_spectra(
#'   input = input,
#'   str_stereo = paste0(dir, "libraries/sop/merged/structures/stereo.tsv"),
#'   str_met = paste0(dir, "libraries/sop/merged/structures/metadata.tsv"),
#'   str_tax_cla = paste0(dir, "libraries/sop/merged/structures/taxonomies/classyfire.tsv"),
#'   str_tax_npc = paste0(dir, "libraries/sop/merged/structures/taxonomies/npc.tsv")
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_annotations_spectra <- function(
  input = get_params(
    step = "prepare_annotations_spectra"
  )$files$annotations$raw$spectral$spectral,
  output = get_params(
    step = "prepare_annotations_spectra"
  )$files$annotations$prepared$structural$spectral,
  str_stereo = get_params(
    step = "prepare_annotations_spectra"
  )$files$libraries$sop$merged$structures$stereo,
  str_met = get_params(
    step = "prepare_annotations_spectra"
  )$files$libraries$sop$merged$structures$metadata,
  str_tax_cla = get_params(
    step = "prepare_annotations_spectra"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "prepare_annotations_spectra"
  )$files$libraries$sop$merged$structures$taxonomies$npc
) {
  # Input Validation ----

  # Validate input files
  if (!is.character(input) || length(input) == 0L) {
    cli::cli_abort(
      "input must be a non-empty character vector",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Vectorized file existence check
  missing_files <- input[!file.exists(input)]
  if (length(missing_files) > 0L) {
    cli::cli_abort(
      c(
        "input file(s) not found",
        "x" = paste(missing_files, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate output
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate structure file paths
  str_files <- list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )

  # Check all are single strings
  validate_all_single_strings(str_files, "Structure file parameter(s)")

  # Check all files exist
  str_files_vec <- unlist(str_files)
  missing_str_files <- str_files_vec[!file.exists(str_files_vec)]
  if (length(missing_str_files) > 0L) {
    cli::cli_abort(
      c(
        "structure file(s) not found",
        "x" = paste(missing_str_files, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Load and Process Spectral Annotations ----

  log_info(
    "Preparing spectral matching annotations from %d file(s)",
    length(input)
  )

  annotation_cols <- c(
    "feature_id",
    "candidate_adduct",
    "candidate_library",
    "candidate_spectrum_id",
    "candidate_spectrum_entropy",
    "candidate_structure_error_mz",
    "candidate_structure_name",
    ## SMILES is the single source of truth for structure identity.
    ## All structural identifiers (InChIKey, formula, mass, xlogp)
    ## are strictly recomputed from SMILES via process_smiles()
    ## downstream.
    "candidate_structure_smiles_no_stereo",
    "candidate_score_similarity",
    "candidate_count_similarity_peaks_matched"
  )

  table <- purrr::map(
    .x = input,
    .f = function(file) {
      prepare_spectral_annotation_chunk(
        file = file,
        annotation_cols = annotation_cols
      )
    }
  ) |>
    tidytable::bind_rows() |>
    tidytable::distinct(
      tidyselect::all_of(annotation_cols)
    ) |>
    ## Add new columns
    tidytable::mutate(
      candidate_structure_tax_npc_01pat = NA_character_,
      candidate_structure_tax_npc_02sup = NA_character_,
      candidate_structure_tax_npc_03cla = NA_character_,
      candidate_structure_tax_cla_chemontid = NA_character_,
      candidate_structure_tax_cla_01kin = NA_character_,
      candidate_structure_tax_cla_02sup = NA_character_,
      candidate_structure_tax_cla_03cla = NA_character_,
      candidate_structure_tax_cla_04dirpar = NA_character_
    ) |>
    select_annotations_columns(
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )

  export_params(
    parameters = get_params(step = "prepare_annotations_spectra"),
    step = "prepare_annotations_spectra"
  )
  export_output(x = table, file = output[[1L]])
  rm(table)

  return(output[[1L]])
}

prepare_spectral_annotation_chunk <- function(file, annotation_cols) {
  selected_cols <- names(tidytable::fread(
    file = file,
    nrows = 0L,
    showProgress = FALSE
  ))
  selected_cols <- intersect(annotation_cols, selected_cols)

  safe_fread(
    file = file,
    file_type = "spectral annotations",
    na.strings = c("", "NA"),
    colClasses = "character",
    select = selected_cols
  ) |>
    .ensure_expected_annotation_cols(annotation_cols) |>
    tidytable::filter(!is.na(feature_id)) |>
    tidytable::distinct(tidyselect::all_of(annotation_cols))
}

.ensure_expected_annotation_cols <- function(df, annotation_cols) {
  missing_cols <- setdiff(annotation_cols, names(df))
  if (length(missing_cols) == 0L) {
    return(df)
  }

  df |>
    tidytable::mutate(
      !!!stats::setNames(
        rep(list(NA_character_), length(missing_cols)),
        missing_cols
      )
    ) |>
    tidytable::select(tidyselect::all_of(annotation_cols))
}
