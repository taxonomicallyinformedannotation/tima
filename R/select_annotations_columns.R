#' @title Select and standardize annotation columns
#'
#' @description Selects and standardizes annotation columns by filtering to
#'     relevant fields, cleaning NULL/NA values, rounding numeric values,
#'     recomputing all structure properties from SMILES via RDKit, and
#'     complementing with structure metadata.
#'
#' @include columns_utils.R
#' @include complement_metadata_structures.R
#' @include process_smiles.R
#' @include round_reals.R
#' @include validations_utils.R
#'
#' @param df [data.frame] Data frame containing annotation results with structure and
#'     candidate information
#' @param str_stereo [character] Path to structure stereochemistry file
#' @param str_met [character] Path to structure metadata file
#' @param str_tax_cla [character] Path to ClassyFire taxonomy file
#' @param str_tax_npc [character] Path to NPClassifier taxonomy file
#'
#' @return Data frame with standardized annotation columns, cleaned values,
#'     and complemented metadata
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' result <- select_annotations_columns(
#'   df = annotations,
#'   str_stereo = "data/str_stereo.tsv",
#'   str_met = "data/str_metadata.tsv",
#'   str_tax_cla = "data/str_tax_classyfire.tsv",
#'   str_tax_npc = "data/str_tax_npclassifier.tsv"
#' )
#' }
select_annotations_columns <- function(
  df,
  str_stereo,
  str_met,
  str_tax_cla,
  str_tax_npc,
  cache = NULL
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")

  # Early exit for empty input
  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided")
    return(df)
  }

  # Validate file paths (will be passed to complement_metadata_structures)
  validate_file_existence(list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  # Derive SMILES cache path from str_stereo if not explicitly provided
  if (is.null(cache)) {
    cache <- file.path(dirname(str_stereo), "processed.csv.gz")
    if (!file.exists(cache)) {
      log_debug("SMILES cache not found at: %s", cache)
      cache <- NULL
    }
  }

  log_debug("Input: %d rows, %d columns", nrow(df), ncol(df))

  # Get column model
  model <- columns_model()

  # Select relevant columns
  df <- df |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          model$features_calculated_columns,
          model$candidates_calculated_columns,
          model$candidates_sirius_for_columns,
          model$candidates_sirius_str_columns,
          model$candidates_spectra_columns,
          model$candidates_structures_columns
        )
      )
    )

  # Clean NULL/NA sentinels in one pass while preserving current output semantics.
  character_cols <- names(df)[vapply(df, is.character, logical(1L))]
  if (length(character_cols) > 0L) {
    df <- df |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::all_of(x = character_cols),
        .fns = .normalize_annotation_text
      ))
  }

  # Recompute all structure properties from SMILES ----
  # When a SMILES is available from annotation sources, use process_smiles()
  # to recompute ALL derived fields (canonical SMILES, SMILES no stereo,
  # InChIKey, InChIKey connectivity layer, InChIKey no stereo, formula,
  # exact_mass, xlogp) in one shot.  Each unique SMILES is converted once
  # thanks to the caching inside process_smiles().
  df <- recompute_structure_fields_from_smiles(df, cache = cache)

  df <- df |>
    # Round numeric values
    round_reals() |>
    # Convert all numeric to character for consistency
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.numeric),
      .fns = as.character
    )) |>
    # Complement with structure metadata
    complement_metadata_structures(
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          model$features_calculated_columns,
          model$candidates_calculated_columns,
          model$candidates_sirius_for_columns,
          model$candidates_sirius_str_columns,
          model$candidates_spectra_columns,
          model$candidates_structures_columns
        )
      )
    )

  # log_trace("Output: ", nrow(df), " rows, ", ncol(df), " columns")

  return(df)
}

.normalize_annotation_text <- function(x) {
  vals <- trimws(x)
  vals[vals %in% c("N/A", "null", "")] <- NA_character_
  vals
}

#' @title Recompute structure fields from SMILES
#'
#' @description Given a data frame that may contain
#'     `candidate_structure_smiles_no_stereo`, run `process_smiles()` on the
#'     unique SMILES to recompute all derived fields (canonical SMILES,
#'     SMILES no stereo, InChIKey, InChIKey connectivity layer,
#'     InChIKey no stereo, molecular formula, exact mass, xlogp).
#'     Fields that are computable from SMILES are always overwritten so that
#'     they are consistent with the canonical RDKit output.
#'
#' @param df [data.frame] Annotation data frame.
#'
#' @return The same data frame with computable structure fields replaced by
#'     RDKit-derived values.
#'
#' @keywords internal
recompute_structure_fields_from_smiles <- function(df, cache = NULL) {
  smiles_col <- "candidate_structure_smiles_no_stereo"

  # Nothing to do if no SMILES column or all NA

  if (!smiles_col %in% names(df)) {
    return(df)
  }

  has_smiles <- !is.na(df[[smiles_col]]) & nzchar(trimws(df[[smiles_col]]))
  if (!any(has_smiles)) {
    return(df)
  }

  # Build a small table of unique SMILES to process
  unique_smiles <- unique(df[[smiles_col]][has_smiles])
  smiles_df <- tidytable::tidytable(
    structure_smiles_initial = unique_smiles
  )

  log_debug(
    "Recomputing structure fields from %d unique SMILES via RDKit",
    length(unique_smiles)
  )

  processed <- tryCatch(
    process_smiles(smiles_df, cache = cache),
    error = function(e) {
      log_warn(
        "SMILES recomputation failed: %s; keeping original values",
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(processed) || nrow(processed) == 0L) {
    return(df)
  }

  # Build a lookup keyed by the original (input) SMILES
  lookup <- processed |>
    tidytable::select(
      tidyselect::any_of(c(
        ".input_smiles" = "structure_smiles_initial",
        ".recomputed_smiles_no_stereo" = "structure_smiles_no_stereo",
        ".recomputed_inchikey" = "structure_inchikey",
        ".recomputed_inchikey_connectivity_layer" = "structure_inchikey_connectivity_layer",
        ".recomputed_inchikey_no_stereo" = "structure_inchikey_no_stereo",
        ".recomputed_molecular_formula" = "structure_molecular_formula",
        ".recomputed_exact_mass" = "structure_exact_mass",
        ".recomputed_xlogp" = "structure_xlogp"
      ))
    ) |>
    tidytable::distinct(.input_smiles, .keep_all = TRUE)

  # Join and overwrite computable fields
  # Ensure target columns exist before coalescing (they may not exist yet

  # if this is the first time they are being computed, e.g., from annotate_spectra output)
  ensure_cols <- c(
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp"
  )
  for (col in ensure_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }

  df <- df |>
    tidytable::left_join(
      lookup,
      by = stats::setNames(".input_smiles", smiles_col)
    ) |>
    tidytable::mutate(
      candidate_structure_smiles_no_stereo = tidytable::coalesce(
        .recomputed_smiles_no_stereo,
        candidate_structure_smiles_no_stereo
      ),
      candidate_structure_inchikey_connectivity_layer = tidytable::coalesce(
        .recomputed_inchikey_connectivity_layer,
        candidate_structure_inchikey_connectivity_layer
      ),
      candidate_structure_molecular_formula = .recomputed_molecular_formula,
      candidate_structure_exact_mass = .recomputed_exact_mass,
      candidate_structure_xlogp = .recomputed_xlogp
    )

  # Only overwrite InChIKey no-stereo if we have a recomputed value
  if (".recomputed_inchikey_no_stereo" %in% names(df)) {
    df <- df |>
      tidytable::mutate(
        candidate_structure_inchikey_no_stereo = tidytable::coalesce(
          .recomputed_inchikey_no_stereo,
          candidate_structure_inchikey_no_stereo
        )
      )
  }

  # Drop temporary columns
  df <- df |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".recomputed_smiles_no_stereo",
        ".recomputed_inchikey",
        ".recomputed_inchikey_connectivity_layer",
        ".recomputed_inchikey_no_stereo",
        ".recomputed_molecular_formula",
        ".recomputed_exact_mass",
        ".recomputed_xlogp"
      ))
    )

  return(df)
}
