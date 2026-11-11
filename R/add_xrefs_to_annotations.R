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
  n_up <- length(u_prefixes)
  n_uf <- length(u_f_ik)

  df_u_idx <- if (anyNA(u_raw)) {
    match(raw_ik, u_raw)
  } else {
    fastmatch::fmatch(raw_ik, u_raw)
  }

  p_id_vec <- fastmatch::fmatch(f_prefix, u_prefixes)
  ok_p <- which(!is.na(p_id_vec))
  p_id_sub <- p_id_vec[ok_p]
  ik_id_sub <- fastmatch::fmatch(f_ik[ok_p], u_f_ik)
  f_id_sub <- f_id[ok_p]

  keep_p <- which(tabulate(p_id_sub[!is.na(f_id_sub)], nbins = n_up) > 0L)
  if (length(keep_p) == 0L) {
    log_debug(
      "No identifier columns with non-empty data. Skipping enrichment for this tier"
    )
    return(df)
  }
  non_empty_cols <- u_prefixes[keep_p]

  cell_id <- (p_id_sub - 1L) * n_uf + ik_id_sub
  keep_d <- !duplicated(paste(cell_id, f_id_sub, sep = "\r"))
  c_d <- cell_id[keep_d]
  v_d <- f_id_sub[keep_d]

  xref_mat <- matrix("", nrow = n_uf, ncol = n_up)
  if (anyDuplicated(c_d) == 0L) {
    xref_mat[c_d] <- v_d
  } else {
    u_c <- unique(c_d)
    c_pos <- fastmatch::fmatch(c_d, u_c)
    cnts <- tabulate(c_pos, nbins = length(u_c))
    u_v <- character(length(u_c))
    s_mask <- cnts == 1L
    if (any(s_mask)) {
      s_rows <- s_mask[c_pos]
      u_v[c_pos[s_rows]] <- v_d[s_rows]
    }
    m_rows <- which(cnts[c_pos] > 1L)
    sp <- split(v_d[m_rows], c_pos[m_rows])
    u_v[as.integer(names(sp))] <- vapply(
      sp,
      paste,
      character(1L),
      collapse = " $ ",
      USE.NAMES = FALSE
    )
    xref_mat[u_c] <- u_v
  }

  m_row <- fastmatch::fmatch(u_key, u_f_ik)[df_u_idx]
  added_cols <- lapply(keep_p, function(j) xref_mat[m_row, j])
  names(added_cols) <- paste0("candidate_structure_id_", non_empty_cols)

  log_debug(
    "Adding %d identifier columns to result tier: %s",
    length(non_empty_cols),
    paste(non_empty_cols, collapse = ", ")
  )

  res_list <- c(as.list(df), added_cols)
  tidytable::as_tidytable(res_list)
}
