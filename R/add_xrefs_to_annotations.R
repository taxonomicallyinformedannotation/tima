#' Add external reference identifiers (xrefs) to final annotation tables
#'
#' Enriches annotation results with external database identifiers (xrefs)
#' from Wikidata/QLever query by joining on InChIKey, adding one column
#' per reference database.
#'
#' @include safe_fread.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param results_list List with elements full, filtered, mini (data frames)
#' @param xrefs_file Character path to xrefs file from [get_compounds_xrefs()]
#'
#' @return Same list structure with xref columns added to each data frame
#'   (only if not all rows are empty for each xref ID)
#'
#' @keywords internal
add_xrefs_to_annotations <- function(results_list, xrefs_file) {
  if (
    !is.list(results_list) ||
      !all(c("full", "filtered", "mini") %in% names(results_list))
  ) {
    cli::cli_abort(
      "results_list must be a list with elements: full, filtered, mini",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  validate_dataframe(results_list$full, param_name = "results_list$full")
  validate_dataframe(
    results_list$filtered,
    param_name = "results_list$filtered"
  )
  validate_dataframe(results_list$mini, param_name = "results_list$mini")

  if (!file.exists(xrefs_file)) {
    log_warn("xrefs_file not found: %s. Skipping xref enrichment", xrefs_file)
    return(results_list)
  }

  xrefs <- safe_fread(xrefs_file)

  if (nrow(xrefs) == 0L) {
    log_info("No xrefs available. Skipping enrichment")
    return(results_list)
  }

  required_cols <- c("inchikey", "prefix", "id")
  if (!all(required_cols %in% names(xrefs))) {
    cli::cli_abort(
      c(
        "xrefs file must contain required columns",
        "x" = paste(required_cols, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Enriching %d result tables with xrefs", length(results_list))

  results_list <- lapply(
    X = results_list,
    FUN = function(df) {
      .add_xrefs_to_df(df, xrefs)
    }
  )

  results_list
}

#' Helper: Add xrefs to a single data frame
#'
#' @keywords internal
#' @noRd
.add_xrefs_to_df <- function(df, xrefs) {
  # Skip if no data
  if (nrow(df) == 0L) {
    return(df)
  }

  # Find the inchikey column
  inchikey_col <- "candidate_structure_inchikey_connectivity_layer"

  if (!inchikey_col %in% names(df)) {
    log_warn(
      "Column %s not found in results. Skipping xref enrichment",
      inchikey_col
    )
    return(df)
  }

  # Restrict xrefs to only InChIKeys present in this result tier.
  # Normalize to connectivity layer (first 14 chars) on unique values first.
  raw_ik <- as.character(df[[inchikey_col]])
  u_raw <- unique(raw_ik)
  u_key <- substr(u_raw, 1L, 14L)
  valid_u <- !is.na(u_key) & nzchar(u_key)
  inchikey_values <- unique(u_key[valid_u])

  if (length(inchikey_values) == 0L) {
    return(df)
  }

  xref_raw_ik <- as.character(xrefs$inchikey)
  u_xref_raw <- unique(xref_raw_ik)
  u_xref_key <- substr(u_xref_raw, 1L, 14L)
  u_xref_match <- fastmatch::fmatch(u_xref_key, inchikey_values)

  if (all(is.na(u_xref_match))) {
    return(df)
  }

  xref_row_u <- fastmatch::fmatch(xref_raw_ik, u_xref_raw)
  keep_xref <- !is.na(u_xref_match[xref_row_u])
  f_ik <- u_xref_key[xref_row_u[keep_xref]]
  f_prefix <- as.character(xrefs$prefix[keep_xref])
  f_id <- as.character(xrefs$id[keep_xref])

  u_prefixes <- unique(f_prefix)
  u_prefixes <- u_prefixes[!is.na(u_prefixes)]
  u_f_ik <- unique(f_ik)
  in_xrefs_u <- !is.na(fastmatch::fmatch(u_key, u_f_ik))

  df_u_idx <- if (anyNA(u_raw)) {
    match(raw_ik, u_raw)
  } else {
    fastmatch::fmatch(raw_ik, u_raw)
  }

  idx_by_prefix <- split(seq_along(f_prefix), f_prefix)
  added_cols <- list()
  non_empty_cols <- character(0L)

  for (p in u_prefixes) {
    p_idx <- idx_by_prefix[[p]]
    p_ik <- f_ik[p_idx]
    p_id <- f_id[p_idx]
    if (anyDuplicated(p_ik) == 0L) {
      u_p_ik <- p_ik
      u_p_val <- p_id
    } else {
      u_p_ik <- unique(p_ik)
      p_ik_id <- fastmatch::fmatch(p_ik, u_p_ik)
      counts <- tabulate(p_ik_id, nbins = length(u_p_ik))
      u_p_val <- character(length(u_p_ik))
      single_mask <- counts == 1L
      if (any(single_mask)) {
        single_rows <- !duplicated(p_ik_id) & single_mask[p_ik_id]
        u_p_val[p_ik_id[single_rows]] <- p_id[single_rows]
      }
      multi_ids <- which(counts > 1L)
      if (length(multi_ids) > 0L) {
        multi_rows <- which(counts[p_ik_id] > 1L)
        sp <- split(p_id[multi_rows], p_ik_id[multi_rows])
        u_p_val[as.integer(names(sp))] <- vapply(
          sp,
          function(x) paste(unique(x), collapse = " $ "),
          character(1L),
          USE.NAMES = FALSE
        )
      }
    }
    if (!all(is.na(u_p_val))) {
      non_empty_cols <- c(non_empty_cols, p)
      m_u <- fastmatch::fmatch(u_key, u_p_ik)
      u_col <- u_p_val[m_u]
      u_col[is.na(m_u) & in_xrefs_u] <- ""
      added_cols[[paste0("candidate_structure_id_", p)]] <- u_col[df_u_idx]
    }
  }

  if (length(non_empty_cols) == 0L) {
    log_debug(
      "No identifier columns with non-empty data. Skipping enrichment for this tier"
    )
    return(df)
  }

  log_debug(
    "Adding %d identifier columns to result tier: %s",
    length(non_empty_cols),
    paste(non_empty_cols, collapse = ", ")
  )

  res_list <- c(as.list(df), added_cols)
  tidytable::as_tidytable(res_list)
}
