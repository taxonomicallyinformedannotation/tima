#' @title Prepare annotations MS2
#'
#' @description This function prepares MS2 spectral library matching results
#'     by standardizing column names, integrating structure metadata, and
#'     formatting for downstream TIMA annotation workflows. Handles various
#'     spectral matching result formats.
#'
#' @include get_params.R
#' @include select_annotations_columns.R
#'
#' @param input Character string path to spectral matching results file
#' @param output Character string path for prepared spectral annotations output
#' @param str_stereo Character string path to structures stereochemistry file
#' @param str_met Character string path to structures metadata file
#' @param str_nam Character string path to structures names file
#' @param str_tax_cla Character string path to ClassyFire taxonomy file
#' @param str_tax_npc Character string path to NPClassifier taxonomy file
#'
#' @return Character string path to prepared spectral annotations
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
#'   str_nam = paste0(dir, "libraries/sop/merged/structures/names.tsv"),
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
  str_nam = get_params(
    step = "prepare_annotations_spectra"
  )$files$libraries$sop$merged$structures$names,
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
    stop("input must be a non-empty character vector")
  }

  # Vectorized file existence check
  missing_files <- input[!file.exists(input)]
  if (length(missing_files) > 0L) {
    stop("Input file(s) not found: ", paste(missing_files, collapse = ", "))
  }

  # Validate output
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

  # Check all are single strings
  is_valid_string <- sapply(str_files, function(x) {
    is.character(x) && length(x) == 1L
  })

  if (!all(is_valid_string)) {
    invalid_params <- names(str_files)[!is_valid_string]
    stop(
      "Parameter(s) must be single character strings: ",
      paste(invalid_params, collapse = ", ")
    )
  }

  # Check all files exist
  str_files_vec <- unlist(str_files)
  missing_str_files <- str_files_vec[!file.exists(str_files_vec)]
  if (length(missing_str_files) > 0L) {
    stop(
      "Structure file(s) not found: ",
      paste(missing_str_files, collapse = ", ")
    )
  }

  # Load and Process Spectral Annotations ----

  log_info(
    "Preparing spectral matching annotations from {length(input)} file(s)"
  )
  # log_trace("Loading and formatting spectral matches")

  table <-
    purrr::map(
      .x = input,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
    tidytable::bind_rows() |>
    tidytable::filter(!is.na(feature_id)) |>
    tidytable::distinct(
      feature_id,
      candidate_adduct,
      candidate_library,
      candidate_spectrum_id,
      candidate_spectrum_entropy,
      candidate_structure_error_mz,
      candidate_structure_name,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo,
      candidate_structure_molecular_formula,
      candidate_structure_exact_mass,
      candidate_structure_xlogp,
      candidate_score_similarity,
      candidate_count_similarity_peaks_matched
    ) |>
    ## Add new columns
    tidytable::mutate(
      candidate_structure_exact_mass = as.numeric(
        candidate_structure_exact_mass
      ),
      candidate_structure_tax_npc_01pat = NA_character_,
      candidate_structure_tax_npc_02sup = NA_character_,
      candidate_structure_tax_npc_03cla = NA_character_,
      candidate_structure_tax_cla_chemontid = NA_character_,
      candidate_structure_tax_cla_01kin = NA_character_,
      candidate_structure_tax_cla_02sup = NA_character_,
      candidate_structure_tax_cla_03cla = NA_character_,
      candidate_structure_tax_cla_04dirpar = NA_character_,
    ) |>
    select_annotations_columns(
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )

  export_params(
    parameters = get_params(step = "prepare_annotations_spectra"),
    step = "prepare_annotations_spectra"
  )
  export_output(x = table, file = output[[1]])
  rm(table)

  return(output[[1]])
}
