#' @title Get standardized column naming schema
#'
#' @description Defines the standardized column naming schema used throughout
#'     the TIMA package. Organizes columns into logical groups for features,
#'     candidates, components, and scoring.
#'
#' @return Named list containing character vectors of column names by category:
#'   \item{features_columns}{Basic feature identifiers (ID, m/z, RT)}
#'   \item{features_calculated_columns}{Calculated feature properties}
#'   \item{candidates_calculated_columns}{Calculated candidate properties}
#'   \item{candidates_sirius_for_columns}{SIRIUS formula annotations}
#'   \item{candidates_sirius_str_columns}{SIRIUS structure scores}
#'   \item{candidates_spectra_columns}{Spectral library matching results}
#'   \item{candidates_structures_columns}{Chemical structure metadata}
#'   \item{components_columns}{Molecular network component IDs}
#'   \item{rank_columns}{Candidate ranking columns}
#'   \item{score_columns}{Candidate scoring columns}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Get column schema
#' model <- columns_model()
#'
#' # Access specific column groups
#' feature_cols <- model$features_columns
#' score_cols <- model$score_columns
#'
#' # Use in data frame selection
#' selected <- df |> select(any_of(model$candidates_structures_columns))
#' }
columns_model <- function() {
  # Basic feature identifiers ----
  features_columns <- c("feature_id", "feature_mz", "feature_rt")

  # Calculated feature-level properties ----
  features_calculated_columns <- c(
    "feature_spectrum_entropy",
    "feature_spectrum_peaks",
    "feature_pred_tax_cla_01kin_val",
    "feature_pred_tax_cla_01kin_score",
    "feature_pred_tax_cla_02sup_val",
    "feature_pred_tax_cla_02sup_score",
    "feature_pred_tax_cla_03cla_val",
    "feature_pred_tax_cla_03cla_score",
    "feature_pred_tax_cla_04dirpar_val",
    "feature_pred_tax_cla_04dirpar_score",
    "feature_pred_tax_npc_01pat_val",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_npc_02sup_val",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_npc_03cla_val",
    "feature_pred_tax_npc_03cla_score"
  )

  # Calculated candidate-level properties ----
  candidates_calculated_columns <- c("candidate_spectrum_entropy")

  # SIRIUS formula-level columns ----
  candidates_sirius_for_columns <- c(
    "candidate_structure_molecular_formula",
    "candidate_count_sirius_peaks_explained",
    "candidate_score_sirius_intensity",
    "candidate_score_sirius_isotope",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_tree",
    "candidate_score_sirius_zodiac"
  )

  # SIRIUS structure-level scores ----
  candidates_sirius_str_columns <- c(
    "candidate_score_sirius_confidence",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_msnovelist"
  )

  # Spectral library matching columns ----
  candidates_spectra_columns <- c(
    "candidate_library",
    "candidate_spectrum_id",
    "candidate_adduct",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity"
  )

  # Chemical structure metadata and taxonomy ----
  candidates_structures_columns <- c(
    "candidate_structure_name",
    "candidate_structure_exact_mass",
    "candidate_structure_molecular_formula",
    "candidate_structure_xlogp",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    # "candidate_structure_tax_cla",
    # "candidate_structure_tax_npc",
    "candidate_structure_organism_occurrence_closest",
    "candidate_structure_organism_occurrence_reference",
    "candidate_structure_error_mz",
    "candidate_structure_error_rt"
  )

  # Component identification ----
  components_columns <- c("component_id")

  # Ranking columns ----
  rank_columns <- c("rank_initial", "rank_final")

  # Scoring columns ----
  score_columns <- c(
    "score_initial",
    "score_biological",
    # "score_interim",
    "score_chemical",
    "score_final"
  )

  return(list(
    features_columns = features_columns,
    features_calculated_columns = features_calculated_columns,
    candidates_calculated_columns = candidates_calculated_columns,
    candidates_sirius_for_columns = candidates_sirius_for_columns,
    candidates_sirius_str_columns = candidates_sirius_str_columns,
    candidates_spectra_columns = candidates_spectra_columns,
    candidates_structures_columns = candidates_structures_columns,
    components_columns = components_columns,
    rank_columns = rank_columns,
    score_columns = score_columns
  ))
}

#' @title Collapse and clean grouped data
#'
#' @description Collapses grouped dataframe by combining unique values per group.
#'     Removes NA values, trims whitespace, and converts empty strings to NA.
#'     Useful for aggregating annotations or metadata.
#'     Internal helper for cleaning and aggregation functions.
#'
#' @include validations_utils.R
#'
#' @param grouped_df Grouped data frame to collapse
#' @param cols Character vector of column names to collapse.
#'     If NULL (default), applies to all columns.
#' @param separator Character string separator for collapsed values
#'     (default: " $ ")
#'
#' @return Data frame with unique values collapsed per group
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only
#' library(tidytable)
#' df <- data.frame(
#'   group = c("A", "A", "B", "B"),
#'   value = c("x", "y", "x", "x")
#' )
#' grouped <- df |> group_by(group)
#' clean_collapse(grouped, cols = "value")
#' }
clean_collapse <- function(grouped_df, cols = NULL, separator = " $ ") {
  # Input Validation ----
  validate_dataframe(grouped_df, param_name = "grouped_df")
  validate_character(
    separator,
    param_name = "separator",
    allow_empty = FALSE
  )

  # Determine Columns to Process ----
  cols <- determine_columns_to_process(grouped_df, cols)

  # Collapse and Clean ----
  collapse_fn <- create_collapse_function(separator)

  grouped_df |>
    tidytable::reframe(tidytable::across(
      .cols = tidyselect::all_of(x = cols),
      .fns = collapse_fn
    )) |>
    tidytable::ungroup() |>
    convert_lists_to_characters() |>
    clean_character_columns()
}

# Helper Functions ----

#' Determine which columns to process
#' @keywords internal
determine_columns_to_process <- function(df, cols) {
  if (is.null(cols)) {
    return(names(df))
  }

  cols <- as.character(cols)

  # Validate columns exist
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "Column(s) not found: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  cols
}

#' Create collapse function with separator
#' @keywords internal
create_collapse_function <- function(separator) {
  function(x) {
    list(paste(unique(x[!is.na(x)]), collapse = separator))
  }
}

#' Convert list columns to character
#' @keywords internal
convert_lists_to_characters <- function(df) {
  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.list),
      .fns = as.character
    ))
}

#' Clean character columns (trim and convert empty to NA)
#' @keywords internal
clean_character_columns <- function(df) {
  df |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = \(x) tidytable::na_if(x = trimws(x), y = "")
    ))
}

#' @title Create template annotation columns
#'
#' @description Creates a template data frame with all expected annotation
#'     columns initialized to NA. Used as a fallback when no annotations are
#'     available or to ensure consistent column structure.
#'
#' @return Data frame with one row and columns for all annotation fields,
#'     with all values set to NA. Columns include feature IDs, structure
#'     information, scores, and taxonomic classifications.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Create empty annotation template
#' template <- fake_annotations_columns()
#'
#' # Use as fallback when no annotations found
#' if (nrow(annotations) == 0) {
#'   annotations <- fake_annotations_columns()
#' }
#' }
fake_annotations_columns <- function() {
  data.frame(
    feature_id = NA_character_,
    candidate_structure_error_mz = NA_real_,
    candidate_structure_error_rt = NA_real_,
    candidate_structure_name = NA_character_,
    candidate_structure_inchikey_connectivity_layer = NA_character_,
    candidate_structure_smiles_no_stereo = NA_character_,
    candidate_structure_molecular_formula = NA_character_,
    candidate_structure_exact_mass = NA_real_,
    candidate_structure_xlogp = NA_real_,
    candidate_adduct = NA_character_,
    candidate_library = NA_character_,
    candidate_score_similarity = NA_real_,
    candidate_count_similarity_peaks_matched = NA_integer_,
    candidate_structure_tax_npc_01pat = NA_character_,
    candidate_structure_tax_npc_02sup = NA_character_,
    candidate_structure_tax_npc_03cla = NA_character_,
    candidate_structure_tax_cla_chemontid = NA_character_,
    candidate_structure_tax_cla_01kin = NA_character_,
    candidate_structure_tax_cla_02sup = NA_character_,
    candidate_structure_tax_cla_03cla = NA_character_,
    candidate_structure_tax_cla_04dirpar = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' @title Create template SOP columns
#'
#' @description Creates an empty structure-organism pair (SOP) dataframe
#'     template with all standard column names and NA values. Used as a
#'     placeholder when actual SOP data is unavailable.
#'
#' @return Single-row data frame with standard SOP columns filled with
#'     NA values
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Create empty SOP template
#' template <- fake_sop_columns()
#'
#' # Use as fallback when no SOP data available
#' if (nrow(sop_data) == 0) {
#'   sop_data <- fake_sop_columns()
#' }
#' }
fake_sop_columns <- function() {
  # Create template with all standard SOP columns as character type
  tidytable::tidytable(
    structure_name = NA_character_,
    structure_inchikey = NA_character_,
    structure_smiles = NA_character_,
    structure_inchikey_connectivity_layer = NA_character_,
    structure_smiles_no_stereo = NA_character_,
    structure_molecular_formula = NA_character_,
    structure_exact_mass = NA_real_,
    structure_xlogp = NA_real_,
    structure_tax_npc_01pat = NA_character_,
    structure_tax_npc_02sup = NA_character_,
    structure_tax_npc_03cla = NA_character_,
    structure_tax_cla_chemontid = NA_character_,
    structure_tax_cla_01kin = NA_character_,
    structure_tax_cla_02sup = NA_character_,
    structure_tax_cla_03cla = NA_character_,
    structure_tax_cla_04dirpar = NA_character_,
    organism_name = NA_character_,
    organism_taxonomy_ottid = NA_character_,
    organism_taxonomy_01domain = NA_character_,
    organism_taxonomy_02kingdom = NA_character_,
    organism_taxonomy_03phylum = NA_character_,
    organism_taxonomy_04class = NA_character_,
    organism_taxonomy_05order = NA_character_,
    organism_taxonomy_06family = NA_character_,
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = NA_character_,
    organism_taxonomy_09species = NA_character_,
    organism_taxonomy_10varietas = NA_character_,
    reference_doi = NA_character_
  )
}
