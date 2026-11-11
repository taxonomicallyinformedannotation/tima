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
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param df [data.frame] Data frame containing annotation results with
#'     structure and
#'     candidate information
#' @param str_stereo [character] Path to structure stereochemistry file
#' @param str_met [character] Path to structure metadata file
#' @param str_tax_cla [character] Path to ClassyFire taxonomy file
#' @param str_tax_npc [character] Path to NPClassifier taxonomy file
#' @param cache (character or NULL) Optional path to a SMILES processing cache
#'     file (e.g. `processed.csv.gz`). When `NULL` (default), the cache path
#'     is auto-derived from `dirname(str_stereo)/processed.csv.gz`.
#' @param recompute_smiles [logical] If `TRUE` (default), recompute structure
#'   fields from SMILES via RDKit. Set to `FALSE` to skip RDKit and keep the
#'   provided structure fields as-is (faster for large batch runs).
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
  cache = NULL,
  recompute_smiles = TRUE
) {
  validate_dataframe(df, param_name = "df")

  # Early exit for empty input. Return a schema-complete (zero-row) frame
  # matching the projection columns the non-empty path produces, so that
  # downstream targets (summarize_results, clean_chemo_preprocessing, ...) do
  # not break on missing columns like candidate_structure_inchikey_*.
  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided")
    model <- columns_model()
    projection_columns <- .annotation_projection_columns(model)
    # Deduplicate the same way tidyselect::any_of does in the non-empty path
    # (projection_columns may contain duplicates across the model groups, e.g.
    # candidate_structure_molecular_formula appears in both sirius_for and
    # structures column sets).
    projection_columns <- unique(projection_columns)
    empty <- data.frame(
      matrix(
        nrow = 0L,
        ncol = length(projection_columns),
        dimnames = list(NULL, projection_columns)
      )
    )
    # Ensure character type for all columns (the non-empty path coerces numerics
    # to character and normalizes text; use character NA to stay consistent).
    for (col in projection_columns) {
      empty[[col]] <- character(0L)
    }
    return(empty)
  }

  # Validate file paths (will be passed to complement_metadata_structures)
  validate_file_existence(list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  cache <- .resolve_smiles_cache_path(str_stereo = str_stereo, cache = cache)

  log_debug("Input: %d rows, %d columns", nrow(df), ncol(df))

  model <- columns_model()

  projection_columns <- .annotation_projection_columns(model)

  # Select relevant columns
  df <- tidytable::as_tidytable(df) |>
    tidytable::select(
      tidyselect::any_of(x = projection_columns)
    )

  # Normalize adduct spacing (e.g., "[M + H]+" -> "[M+H]+")
  if ("candidate_adduct" %in% names(df)) {
    adducts_vec <- df[["candidate_adduct"]]
    u_add <- unique(adducts_vec)
    u_add_clean <- gsub("\\s+", "", u_add)
    data.table::set(
      df,
      j = "candidate_adduct",
      value = u_add_clean[fastmatch::fmatch(adducts_vec, u_add)]
    )
  }

  # Clean NULL/NA sentinels in-place while preserving output semantics.
  character_cols <- names(df)[vapply(df, is.character, logical(1L))]
  for (col in character_cols) {
    data.table::set(
      df,
      j = col,
      value = .normalize_annotation_text(df[[col]])
    )
  }

  if (isTRUE(recompute_smiles)) {
    # When a SMILES is available from annotation sources, use process_smiles()
    # to recompute ALL derived fields (canonical SMILES, SMILES no stereo,
    # InChIKey, InChIKey connectivity layer, InChIKey no stereo, formula,
    # exact_mass, xlogp) in one shot. Each unique SMILES is converted once
    # thanks to caching inside process_smiles().
    df <- recompute_structure_fields_from_smiles(df, cache = cache)
  } else {
    log_info("Skipping RDKit SMILES recomputation (recompute_smiles=FALSE)")
  }

  .num_to_char_fast <- function(x) {
    ux <- unique(x)
    as.character(ux)[fastmatch::fmatch(x, ux)]
  }

  # Round numeric values in-place
  df <- round_reals(df)

  # Convert all numeric columns to character in-place
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
  for (col in numeric_cols) {
    data.table::set(df, j = col, value = .num_to_char_fast(df[[col]]))
  }

  df_complement <- complement_metadata_structures(
    df = select_annotations_complement_working_columns(df),
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )
  for (col in names(df_complement)) {
    data.table::set(df, j = col, value = df_complement[[col]])
  }

  df <- df |>
    tidytable::select(
      tidyselect::any_of(x = projection_columns)
    )

  df
}

.fast_trim_and_na <- function(v) {
  ws <- grep("^[ \t\r\n]|[ \t\r\n]$", v)
  if (length(ws) > 0L) {
    v[ws] <- trimws(v[ws])
  }
  bad <- fastmatch::fmatch(v, c("N/A", "null", ""), nomatch = 0L) > 0L
  if (any(bad)) {
    v[bad] <- NA_character_
  }
  v
}

.normalize_annotation_text <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }
  ux <- unique(x)
  vals <- .fast_trim_and_na(ux)
  if (identical(vals, ux)) {
    return(x)
  }
  vals[fastmatch::fmatch(x, ux)]
}

.annotation_projection_columns <- function(model) {
  c(
    "feature_id",
    model$features_calculated_columns,
    model$candidates_calculated_columns,
    model$candidates_sirius_for_columns,
    model$candidates_sirius_str_columns,
    model$candidates_spectra_columns,
    model$candidates_structures_columns
  )
}

.resolve_smiles_cache_path <- function(str_stereo, cache) {
  if (!is.null(cache)) {
    return(cache)
  }

  derived <- file.path(dirname(str_stereo), "processed.csv.gz")
  if (!file.exists(derived)) {
    log_debug("SMILES cache not found at: %s", derived)
    return(NULL)
  }

  derived
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
#' @param cache (character or NULL) Optional path to a SMILES processing cache
#'     file. When provided, previously computed results are reused, avoiding
#'     redundant RDKit calls.
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

  smiles_vec <- as.character(df[[smiles_col]])
  unique_smiles <- unique(smiles_vec)
  unique_smiles <- trimws(unique_smiles[!is.na(unique_smiles)])
  unique_smiles <- unique_smiles[nzchar(unique_smiles)]
  if (length(unique_smiles) == 0L) {
    return(df)
  }

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

  # Build a lookup keyed by original (input) SMILES without full-table distinct() copy.
  if (anyDuplicated(processed$structure_smiles_initial) > 0L) {
    processed <- processed[!duplicated(processed$structure_smiles_initial), ]
  }
  lookup <- list(
    .input_smiles = processed$structure_smiles_initial,
    .recomputed_smiles_no_stereo = processed$structure_smiles_no_stereo,
    .recomputed_inchikey_connectivity_layer = processed$structure_inchikey_connectivity_layer,
    .recomputed_inchikey_no_stereo = processed$structure_inchikey_no_stereo,
    .recomputed_molecular_formula = processed$structure_molecular_formula,
    .recomputed_exact_mass = processed$structure_exact_mass,
    .recomputed_xlogp = processed$structure_xlogp
  )

  # Ensure target columns exist before overwrite/coalesce.
  df <- tidytable::as_tidytable(df)
  ensure_cols <- c(
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp"
  )
  n_rows <- nrow(df)
  for (col in ensure_cols) {
    if (!col %in% names(df)) {
      data.table::set(df, j = col, value = rep.int(NA_character_, n_rows))
    }
  }

  idx <- fastmatch::fmatch(smiles_vec, lookup$.input_smiles)

  .coalesce_recomputed <- function(recomputed, existing) {
    if (!anyNA(recomputed)) {
      return(recomputed)
    }
    if (all(is.na(existing))) {
      return(recomputed)
    }
    na_rec <- is.na(recomputed)
    if (any(na_rec)) {
      recomputed[na_rec] <- existing[na_rec]
    }
    recomputed
  }

  data.table::set(
    df,
    j = "candidate_structure_smiles_no_stereo",
    value = .coalesce_recomputed(
      lookup$.recomputed_smiles_no_stereo[idx],
      df$candidate_structure_smiles_no_stereo
    )
  )
  data.table::set(
    df,
    j = "candidate_structure_inchikey_connectivity_layer",
    value = .coalesce_recomputed(
      lookup$.recomputed_inchikey_connectivity_layer[idx],
      df$candidate_structure_inchikey_connectivity_layer
    )
  )
  data.table::set(
    df,
    j = "candidate_structure_inchikey_no_stereo",
    value = .coalesce_recomputed(
      lookup$.recomputed_inchikey_no_stereo[idx],
      df$candidate_structure_inchikey_no_stereo
    )
  )
  data.table::set(
    df,
    j = "candidate_structure_molecular_formula",
    value = lookup$.recomputed_molecular_formula[idx]
  )
  data.table::set(
    df,
    j = "candidate_structure_exact_mass",
    value = lookup$.recomputed_exact_mass[idx]
  )
  data.table::set(
    df,
    j = "candidate_structure_xlogp",
    value = lookup$.recomputed_xlogp[idx]
  )

  rm(
    processed,
    unique_smiles,
    smiles_df,
    lookup,
    idx,
    ensure_cols,
    smiles_col
  )

  df
}
