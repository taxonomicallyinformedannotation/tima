#' Add external reference identifiers (xrefs) to final annotation tables
#'
#' Enriches annotation results with external database identifiers (xrefs)
#' from Wikidata/QLever query by joining on InChIKey, adding one column
#' per reference database.
#'
#' @include safe_fread.R
#' @include validations_utils.R
#' @include logs_utils.R
#'
#' @param results_list List with elements full, filtered, mini (data frames)
#' @param xrefs_file Character path to xrefs file from [get_compounds_xrefs()]
#'
#' @return Same list structure with xref columns added to each data frame
#'   (only if not all rows are empty for each xref ID)
#'
#' @keywords internal
add_xrefs_to_annotations <- function(results_list, xrefs_file) {
  # Validation ----
  if (
    !is.list(results_list) ||
      !all(c("full", "filtered", "mini") %in% names(results_list))
  ) {
    stop(
      "results_list must be a list with elements: full, filtered, mini",
      call. = FALSE
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

  # Load xrefs ----
  xrefs <- safe_fread(xrefs_file)

  if (nrow(xrefs) == 0L) {
    log_info("No xrefs available. Skipping enrichment")
    return(results_list)
  }

  # Validate xrefs structure ----
  required_cols <- c("inchikey", "prefix", "id")
  if (!all(required_cols %in% names(xrefs))) {
    stop(
      "xrefs file must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  log_info("Enriching %d result tables with xrefs", length(results_list))

  # Process each result tier ----
  results_list <- lapply(
    X = results_list,
    FUN = function(df) {
      .add_xrefs_to_df(df, xrefs)
    }
  )

  return(results_list)
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
  # Normalize to connectivity layer (first 14 chars) before matching.
  df_key <- substr(df[[inchikey_col]], 1L, 14L)
  inchikey_values <- unique(df_key)
  inchikey_values <- inchikey_values[
    !is.na(inchikey_values) & nzchar(inchikey_values)
  ]

  if (length(inchikey_values) == 0L) {
    return(df)
  }

  xrefs_filtered <- xrefs |>
    tidytable::mutate(inchikey = substr(inchikey, 1L, 14L)) |>
    tidytable::filter(inchikey %in% inchikey_values)

  if (nrow(xrefs_filtered) == 0L) {
    return(df)
  }

  # Pivot xrefs: one column per prefix (database)
  xrefs_wide <- xrefs_filtered |>
    tidytable::distinct(inchikey, prefix, id) |>
    tidytable::pivot_wider(
      id_cols = inchikey,
      names_from = prefix,
      values_from = id,
      values_fn = list(id = function(x) {
        # If multiple IDs per inchikey × prefix, collapse with " $ "
        paste(unique(x), collapse = " $ ")
      })
    )

  # Identify which prefix columns to add
  # Filter: only add if not all values are NA (i.e., has some coverage)
  columns_to_add <- setdiff(names(xrefs_wide), "inchikey")
  non_empty_cols <- columns_to_add[
    vapply(
      columns_to_add,
      function(col) {
        !all(is.na(xrefs_wide[[col]]))
      },
      logical(1L)
    )
  ]

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

  # Prepare xref subset with standardized column names
  xrefs_subset <- xrefs_wide |>
    tidytable::select(inchikey, tidyselect::all_of(non_empty_cols)) |>
    tidytable::rename_with(
      ~ paste0("candidate_structure_id_", .x),
      .cols = tidyselect::all_of(non_empty_cols)
    )

  # Join: left_join to preserve all annotation rows
  # Join on connectivity-layer keys (first 14 chars)
  xrefs_for_join <- xrefs_subset |>
    tidytable::rename(inchikey_key = inchikey)

  df_for_join <- df
  df_for_join$inchikey_key <- substr(df_for_join[[inchikey_col]], 1L, 14L)

  result_df <- df_for_join |>
    tidytable::left_join(xrefs_for_join, by = "inchikey_key") |>
    tidytable::select(-inchikey_key)

  return(result_df)
}
