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
  candidates_calculated_columns <- c(
    "candidate_spectrum_entropy",
    "candidate_adduct_origin",
    "candidate_annotation_level",
    "candidate_evidence_tier",
    "adduct_support"
  )

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
    "candidate_query_adduct",
    "candidate_adduct_match_mode",
    "annotation_note",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse"
  )

  # Chemical structure metadata and taxonomy ----
  candidates_structures_columns <- c(
    "candidate_structure_name",
    "candidate_structure_exact_mass",
    "candidate_structure_molecular_formula",
    "candidate_structure_xlogp",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
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
    "candidate_structure_tag",
    "candidate_structure_error_mz",
    "candidate_structure_error_rt"
  )

  # Component identification ----
  components_columns <- "component_id"

  # Ranking columns ----
  rank_columns <- c("rank_initial", "rank_final")

  # Scoring columns ----
  score_columns <- c(
    "score_initial",
    "score_biological",
    "score_weighted_bio",
    # "score_interim",
    "score_chemical",
    "score_weighted_chemo",
    "score_final"
  )

  list(
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
  )
}

#' Select structure-organism pair columns
#' @keywords internal
select_structure_organism_pair_columns <- function(df) {
  nms <- names(df)
  wanted <- unique(c(
    nms[startsWith(nms, "candidate_structure_")],
    nms[startsWith(nms, "structure_")],
    nms[startsWith(nms, "organism_")],
    "reference_doi",
    "stereo_name"
  ))
  keep <- intersect(wanted, nms)
  if (identical(keep, nms) && inherits(df, "tidytable")) {
    return(df)
  }
  tidytable::as_tidytable(as.list(df)[keep])
}

#' Select loading columns for structure-organism pairs
#' @keywords internal
select_structure_organism_pair_loading_columns <- function(df) {
  nms <- names(df)
  wanted <- unique(c(
    "inchikey",
    "structure_inchikey",
    "candidate_structure_inchikey",
    "candidate_structure_inchikey_connectivity_layer",
    nms[startsWith(nms, "candidate_structure_")],
    nms[startsWith(nms, "structure_")],
    nms[startsWith(nms, "organism_")],
    "reference_doi",
    "stereo_name"
  ))
  keep <- intersect(wanted, nms)
  if (identical(keep, nms) && inherits(df, "tidytable")) {
    return(df)
  }
  tidytable::as_tidytable(as.list(df)[keep])
}

#' Select working columns for cluster consensus in clean_chemo
#' @keywords internal
select_clean_chemo_consensus_working_columns <- function(df) {
  nms <- names(df)
  wanted <- c(
    ".row_id",
    "feature_id",
    "rank_final",
    "candidate_adduct",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_exact_mass",
    "mz",
    "score_weighted_chemo",
    "candidate_score_similarity",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_confidence",
    "candidate_score_pseudo_initial",
    "annotation_note",
    "cluster_consensus_applied",
    "cluster_consensus_promoted_from_anchor",
    "cluster_consensus_group_id",
    "cluster_consensus_anchor_feature_id",
    "cluster_consensus_anchor_inchikey"
  )
  keep <- intersect(wanted, nms)
  if (identical(keep, nms) && inherits(df, "tidytable")) {
    return(df)
  }
  tidytable::as_tidytable(as.list(df)[keep])
}

#' Select working columns for annotation weighting
#' @keywords internal
select_weight_annotations_working_columns <- function(df) {
  model <- columns_model()
  nms <- names(df)
  wanted <- unique(c(
    "feature_id",
    "rt",
    "mz",
    nms[startsWith(nms, "sample_organism_")],
    nms[startsWith(nms, "candidate_organism_")],
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_name",
    "candidate_structure_exact_mass",
    "candidate_structure_molecular_formula",
    "candidate_structure_xlogp",
    "candidate_structure_tag",
    "candidate_structure_organism_occurrence_closest",
    "candidate_structure_organism_occurrence_reference",
    "candidate_structure_error_mz",
    "candidate_structure_error_rt",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "candidate_score_sirius_confidence",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_msnovelist",
    "candidate_score_sirius_intensity",
    "candidate_score_sirius_isotope",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_tree",
    "candidate_score_sirius_zodiac",
    "candidate_count_similarity_peaks_matched",
    "candidate_spectrum_entropy",
    "candidate_library",
    "candidate_spectrum_id",
    "candidate_adduct",
    "candidate_query_adduct",
    "candidate_adduct_match_mode",
    "annotation_note",
    "score_biological",
    "score_chemical",
    "score_weighted_bio",
    "score_weighted_chemo",
    "rank_initial",
    "rank_final",
    model$features_columns,
    model$features_calculated_columns,
    model$components_columns,
    model$candidates_calculated_columns,
    model$candidates_sirius_for_columns,
    model$candidates_sirius_str_columns,
    model$candidates_spectra_columns,
    model$candidates_structures_columns,
    nms[startsWith(nms, "candidate_structure_id_")],
    nms[startsWith(nms, "cluster_consensus_")],
    nms[startsWith(nms, "feature_pred_")]
  ))
  keep <- intersect(wanted, nms)
  if (identical(keep, nms) && inherits(df, "tidytable")) {
    return(df)
  }
  tidytable::as_tidytable(as.list(df)[keep])
}

#' Select working columns for annotation complementation
#' @keywords internal
select_annotations_complement_working_columns <- function(df) {
  model <- columns_model()
  nms <- names(df)
  wanted <- unique(c(
    "feature_id",
    "candidate_structure_smiles_no_stereo",
    model$candidates_structures_columns
  ))
  keep <- intersect(wanted, nms)
  if (identical(keep, nms) && inherits(df, "tidytable")) {
    return(df)
  }
  tidytable::as_tidytable(as.list(df)[keep])
}

#' @title Collapse and clean grouped data
#'
#' @description Collapses grouped dataframe by combining unique values per
#'     group.
#'     Removes NA values, trims whitespace, and converts empty strings to NA.
#'     Useful for aggregating annotations or metadata.
#'     Internal helper for cleaning and aggregation functions.
#'
#' @include validations_utils.R
#' @include errors_utils.R
#' @include validations_params.R
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
  if (!is.data.frame(grouped_df)) {
    tima_abort(
      problem = "grouped_df must be a data frame or grouped tidytable",
      class = c("tima_validation_error", "tima_error")
    )
  }
  validate_character(
    separator,
    param_name = "separator",
    allow_empty = FALSE
  )

  # Determine Columns to Process ----
  cols <- determine_columns_to_process(grouped_df, cols)
  grp_vars <- tidytable::group_vars(grouped_df)

  if (length(grp_vars) != 1L || nrow(grouped_df) == 0L) {
    collapse_fn <- create_collapse_function(separator)
    return(
      grouped_df |>
        tidytable::reframe(tidytable::across(
          .cols = tidyselect::all_of(x = cols),
          .fns = collapse_fn
        )) |>
        tidytable::ungroup() |>
        convert_lists_to_characters() |>
        clean_character_columns()
    )
  }

  g_name <- grp_vars[[1L]]
  g_col <- grouped_df[[g_name]]
  u_g <- unique(g_col)
  g_id <- if (anyNA(u_g)) {
    match(g_col, u_g)
  } else {
    fastmatch::fmatch(g_col, u_g)
  }
  attr(u_g, ".match.hash") <- NULL
  n_g <- length(u_g)

  if (g_name %in% cols) {
    u_g_chr <- as.character(u_g)
    u_g_chr <- trimws(u_g_chr)
    u_g_chr[!nzchar(u_g_chr) | is.na(u_g)] <- NA_character_
    out <- tidytable::tidytable(u_g_chr)
  } else if (is.character(u_g)) {
    u_g_clean <- trimws(u_g)
    u_g_clean[!nzchar(u_g_clean)] <- NA_character_
    out <- tidytable::tidytable(u_g_clean)
  } else {
    out <- tidytable::tidytable(u_g)
  }
  data.table::setnames(out, g_name)

  cols_val <- setdiff(cols, g_name)
  if (length(cols_val) == 0L) {
    return(out)
  }

  counts_g <- tabulate(g_id, nbins = n_g)
  all_single <- max(counts_g) <= 1L

  collapse_one_col <- function(vals) {
    res <- rep.int(NA_character_, n_g)
    ok <- which(!is.na(vals))
    if (length(ok) == 0L) {
      return(res)
    }
    v_ok <- if (is.character(vals)) vals[ok] else as.character(vals[ok])
    if (all_single) {
      u_v <- unique(v_ok)
      u_trim <- trimws(u_v)
      u_trim[!nzchar(u_trim)] <- NA_character_
      m_v <- fastmatch::fmatch(v_ok, u_v)
      res[g_id[ok]] <- u_trim[m_v]
      return(res)
    }

    g_ok <- g_id[ok]
    u_v <- unique(v_ok)
    v_id <- fastmatch::fmatch(v_ok, u_v)
    n_v <- length(u_v)
    use_int <- as.double(n_g) * n_v < .Machine$integer.max
    gv_key <- if (use_int) {
      (g_ok - 1L) * n_v + v_id
    } else {
      as.double(g_ok - 1L) * n_v + v_id
    }
    keep <- if (use_int) {
      !duplicated(gv_key)
    } else {
      fastmatch::fmatch(gv_key, gv_key) == seq_along(gv_key)
    }
    g_ok <- g_ok[keep]
    v_ok <- v_ok[keep]

    if (is.unsorted(g_ok)) {
      ord <- order(g_ok, method = "radix")
      g_ok <- g_ok[ord]
      v_ok <- v_ok[ord]
    }
    n_m <- length(g_ok)
    new_g <- if (n_m > 1L) c(TRUE, g_ok[-1L] != g_ok[-n_m]) else TRUE
    r_m <- seq_len(n_m) - cummax(ifelse(new_g, seq_len(n_m), 0L)) + 1L
    max_r <- max(r_m)
    idx1 <- which(r_m == 1L)
    u_g_pres <- g_ok[idx1]
    acc <- v_ok[idx1]
    if (max_r > 1L) {
      for (r in 2L:max_r) {
        idx_r <- which(r_m == r)
        pos <- fastmatch::fmatch(g_ok[idx_r], u_g_pres)
        acc[pos] <- paste(acc[pos], v_ok[idx_r], sep = separator)
      }
    }
    u_acc <- unique(acc)
    u_trim <- trimws(u_acc)
    u_trim[!nzchar(u_trim)] <- NA_character_
    res[u_g_pres] <- u_trim[fastmatch::fmatch(acc, u_acc)]
    res
  }

  for (col in cols_val) {
    data.table::set(out, j = col, value = collapse_one_col(grouped_df[[col]]))
  }
  out
}


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
    cli::cli_abort(
      c(
        "column(s) not found",
        "x" = paste(missing_cols, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
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
    candidate_structure_inchikey_no_stereo = NA_character_,
    candidate_structure_smiles_no_stereo = NA_character_,
    candidate_structure_molecular_formula = NA_character_,
    candidate_structure_exact_mass = NA_real_,
    candidate_structure_xlogp = NA_real_,
    candidate_adduct = NA_character_,
    candidate_query_adduct = NA_character_,
    candidate_adduct_match_mode = NA_character_,
    annotation_note = NA_character_,
    candidate_adduct_origin = NA_character_,
    candidate_annotation_level = NA_character_,
    candidate_evidence_tier = NA_character_,
    adduct_support = NA_integer_,
    candidate_library = NA_character_,
    candidate_score_similarity = NA_real_,
    candidate_score_similarity_forward = NA_real_,
    candidate_score_similarity_reverse = NA_real_,
    candidate_count_similarity_peaks_matched = NA_integer_,
    candidate_structure_tag = NA_character_,
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
    structure_inchikey_no_stereo = NA_character_,
    structure_smiles_no_stereo = NA_character_,
    structure_molecular_formula = NA_character_,
    structure_exact_mass = NA_real_,
    structure_xlogp = NA_real_,
    structure_tag = NA_character_,
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

#' Normalize ClassyFire chemontid values
#'
#' @description Normalizes chemontid values to the canonical format
#'   `CHEMONTID:NNNNNNN` (prefix + 7-digit zero-padded number). Handles
#'   inputs with or without the `CHEMONTID:` prefix and with inconsistent
#'   zero-padding (e.g., `2011` vs `0002011` vs `CHEMONTID:0002011`).
#'
#' @param x Character vector of chemontid values
#'
#' @return Character vector with normalized chemontid values in the format
#'   `CHEMONTID:NNNNNNN`. `NA` values are preserved.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' normalize_chemontid(c("CHEMONTID:0002011", "2011", "0002011", NA))
#' # => c("CHEMONTID:0002011", "CHEMONTID:0002011", "CHEMONTID:0002011", NA)
#' }
normalize_chemontid <- function(x) {
  x_chr <- if (is.character(x)) x else as.character(x)
  if (length(x_chr) == 0L) {
    return(character(0L))
  }
  u <- unique(x_chr)
  if (length(u) == 1L && is.na(u[[1L]])) {
    return(x_chr)
  }
  u_orig <- u
  # Strip "CHEMONTID:" or "CHEMONT:" prefix if present (case-insensitive)
  stripped <- sub("^CHEMONT(ID)?:", "", u, ignore.case = TRUE)
  # Zero-pad the numeric part to 7 digits and add the canonical prefix
  is_num <- !is.na(stripped) & grepl("^[0-9]+$", stripped)
  if (!any(is_num)) {
    return(x_chr)
  }
  u[is_num] <- paste0(
    "CHEMONTID:",
    sprintf("%07d", as.integer(stripped[is_num]))
  )
  if (identical(u, u_orig)) {
    return(x_chr)
  }
  idx <- if (anyNA(u_orig)) {
    match(x_chr, u_orig)
  } else {
    fastmatch::fmatch(x_chr, u_orig)
  }
  u[idx]
}
