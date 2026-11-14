#' Validate Inputs for prepare_libraries_sop_merged
#'
#' @description Internal helper to validate all input parameters.
#'
#' @keywords internal
validate_sop_merged_inputs <- function(
  files,
  filter,
  level,
  value,
  output_key,
  output_org_tax_ott,
  output_str_can,
  output_str_stereo,
  output_str_met,
  output_str_tax_cla,
  output_str_tax_npc
) {
  if (
    !is.character(files) ||
      length(files) < 1L ||
      anyNA(files)
  ) {
    cli::cli_abort(
      "files must be a non-empty character vector",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.logical(filter) || length(filter) != 1L) {
    cli::cli_abort(
      "filter must be a single logical value (TRUE/FALSE)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  output_paths <- list(
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_can = output_str_can,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  validate_all_single_strings(output_paths, "Output parameter(s)")

  if (isTRUE(filter)) {
    valid_levels <- names(.OTT_RANK_COL_MAP)

    if (!level %in% valid_levels) {
      cli::cli_abort(
        c(
          "level must be one of the supported taxonomic ranks",
          "x" = paste(valid_levels, collapse = ", "),
          "i" = paste0("got: ", level)
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    if (!is.character(value) || length(value) != 1L) {
      cli::cli_abort(
        "value must be a single character string",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  invisible(NULL)
}

#' Load and Merge Library Files
#'
#' @description Internal helper to load and combine library files.
#'
#' @keywords internal
load_and_merge_libraries <- function(files, cache) {
  libraries <- lapply(
    X = files,
    FUN = function(file) {
      safe_fread(
        file = file,
        file_type = "library",
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }
  )

  libraries |>
    tidytable::bind_rows() |>
    split_tables_sop(cache = cache)
}

#' Complete Organism Taxonomy
#'
#' @description Internal helper to fill missing organism taxonomy.
#'
#' @keywords internal
complete_organism_taxonomy <- function(table_keys, table_org_tax_ott) {
  u_orgs <- unique(table_keys$organism_name)
  u_orgs <- u_orgs[!is.na(u_orgs)]
  if (length(u_orgs) == 0L) {
    return(table_org_tax_ott)
  }
  missing_orgs <- u_orgs[
    fastmatch::fmatch(u_orgs, table_org_tax_ott$organism_name, nomatch = 0L) ==
      0L
  ]
  attr(table_org_tax_ott$organism_name, ".match.hash") <- NULL
  if (length(missing_orgs) == 0L) {
    return(table_org_tax_ott)
  }

  table_org_tax_ott_2 <- data.frame(
    organism = missing_orgs,
    stringsAsFactors = FALSE
  )

  table_org_tax_ott_full <- table_org_tax_ott_2 |>
    get_organism_taxonomy_ott(retry = FALSE)

  table_org_tax_ott |>
    tidytable::bind_rows(
      table_org_tax_ott_full |>
        tidytable::as_tidytable() |>
        tidytable::mutate(
          tidytable::across(
            .cols = tidyselect::where(fn = is.numeric),
            .fns = as.character
          ),
          tidytable::across(
            .cols = tidyselect::where(fn = is.list),
            .fns = as.character
          ),
          tidytable::across(
            .cols = tidyselect::where(fn = is.logical),
            .fns = as.character
          )
        )
    )
}

#' Apply Taxonomic Filter
#'
#' @description Internal helper to filter library by taxonomic criteria.
#'
#' @keywords internal
apply_taxonomic_filter <- function(
  table_keys,
  table_org_tax_ott,
  level,
  value
) {
  # Prefer canonical 15-rank OTT column name, falling back to anchored legacy suffix
  canonical_col <- unname(.OTT_RANK_COL_MAP[level])
  level_col <- if (
    length(canonical_col) == 1L &&
      !is.na(canonical_col) &&
      canonical_col %in% colnames(table_org_tax_ott)
  ) {
    canonical_col
  } else {
    colnames(table_org_tax_ott)[grepl(
      pattern = paste0("(?:_|\\d)", level, "$"),
      x = colnames(table_org_tax_ott),
      perl = TRUE
    )]
  }

  if (length(level_col) == 0) {
    cli::cli_abort(
      c(
        "no column found matching requested taxonomic level",
        "x" = level
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  tax_vals <- table_org_tax_ott[[level_col[[1L]]]]
  u_tax <- unique(tax_vals)
  u_tax <- u_tax[!is.na(u_tax)]
  u_match <- u_tax[grepl(pattern = value, x = u_tax, perl = TRUE)]
  if (length(u_match) == 0L) {
    matching_orgs <- character(0L)
  } else {
    match_org_mask <- fastmatch::fmatch(tax_vals, u_match, nomatch = 0L) > 0L
    matching_orgs <- unique(table_org_tax_ott$organism_name[match_org_mask])
    matching_orgs <- matching_orgs[!is.na(matching_orgs)]
  }

  if (length(matching_orgs) == 0L) {
    cli::cli_abort(
      c(
        "filter led to no entries",
        "x" = paste0("level: ", level),
        "i" = paste0("value: ", value),
        "i" = "try different filter criteria"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  keep_rows <- fastmatch::fmatch(
    table_keys$organism_name,
    matching_orgs,
    nomatch = 0L
  ) >
    0L
  if (!any(keep_rows)) {
    cli::cli_abort(
      c(
        "filter led to no entries",
        "x" = paste0("level: ", level),
        "i" = paste0("value: ", value),
        "i" = "try different filter criteria"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  idx <- which(keep_rows)
  tidytable::distinct(
    tidytable::tidytable(
      structure_inchikey = table_keys$structure_inchikey[idx],
      structure_smiles_no_stereo = table_keys$structure_smiles_no_stereo[idx],
      organism_name = table_keys$organism_name[idx],
      reference_doi = table_keys$reference_doi[idx]
    )
  )
}

#' Export All Library Tables
#'
#' @description Internal helper to export all library tables.
#'
#' @keywords internal
export_library_tables <- function(
  table_keys,
  table_org_tax_ott,
  table_structures_canonical,
  table_structures_stereo,
  table_structures_metadata,
  table_structures_taxonomy_cla,
  table_structures_taxonomy_npc,
  output_key,
  output_org_tax_ott,
  output_str_can,
  output_str_stereo,
  output_str_met,
  output_str_tax_cla,
  output_str_tax_npc
) {
  export_output(x = table_keys, file = output_key)
  export_output(x = table_org_tax_ott, file = output_org_tax_ott)
  export_output(x = table_structures_canonical, file = output_str_can)
  export_output(x = table_structures_stereo, file = output_str_stereo)
  export_output(x = table_structures_metadata, file = output_str_met)
  export_output(x = table_structures_taxonomy_cla, file = output_str_tax_cla)
  export_output(x = table_structures_taxonomy_npc, file = output_str_tax_npc)
  rm(
    table_keys,
    table_org_tax_ott,
    table_structures_canonical,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc
  )

  c(
    "key" = output_key,
    "org_tax_ott" = output_org_tax_ott,
    "str_can" = output_str_can,
    "str_stereo" = output_str_stereo,
    "str_met" = output_str_met,
    "str_tax_cla" = output_str_tax_cla,
    "str_tax_npc" = output_str_tax_npc
  )
}

#' Apply column mapping to a data frame
#' @description Renames columns based on a mapping list. For each target name,
#'   tries possible source names in order and renames the first match found.
#' @include logs_utils.R
#' @include safe_fread.R
#' @include columns_utils.R
#' @include predicates_utils.R
#' @include create_dir.R
#' @include export_output.R
#' @include get_organism_taxonomy_ott.R
#' @include split_tables_sop.R
#' @param df Data frame to rename columns in
#' @param col_mapping Named list: names are target col names, values are
#'   character vectors of possible source names (tried in order)
#' @return Data frame with renamed columns
#' @keywords internal
.apply_col_mapping <- function(df, col_mapping) {
  df_names <- names(df)
  targets_needed <- setdiff(names(col_mapping), df_names)
  first_srcs <- vapply(
    targets_needed,
    function(target) {
      src <- intersect(col_mapping[[target]], df_names)
      if (length(src) > 0L) src[[1L]] else NA_character_
    },
    character(1L)
  )
  valid <- !is.na(first_srcs)
  if (any(valid)) {
    m <- match(first_srcs[valid], df_names)
    df_names[m] <- targets_needed[valid]
    names(df) <- df_names
  }
  df
}

# Helper function: Normalize chemontid values to prevent duplicates
.normalize_chemontid_in_tables <- function(
  cache_data,
  taxonomy_table,
  chemontid_col = "structure_tax_cla_chemontid"
) {
  if (chemontid_col %in% names(cache_data)) {
    cache_data[[chemontid_col]] <- normalize_chemontid(
      cache_data[[chemontid_col]]
    )
  }
  if (
    !is.null(taxonomy_table) &&
      nrow(taxonomy_table) > 0 &&
      chemontid_col %in% names(taxonomy_table)
  ) {
    taxonomy_table[[chemontid_col]] <- normalize_chemontid(
      taxonomy_table[[chemontid_col]]
    )
  }
  list(cache_data = cache_data, taxonomy_table = taxonomy_table)
}

.clean_not_classified_col <- function(x) {
  x <- as.character(x)
  nc_mask <- !is.na(x) & x == "notClassified"
  if (any(nc_mask)) {
    x[nc_mask] <- NA_character_
  }
  x
}

# Helper function: Normalize and write cache file
.normalize_and_write_cache <- function(
  cache_data,
  cache_path,
  expected_cols,
  taxonomy_name
) {
  tryCatch(
    {
      keep_cols <- intersect(expected_cols, names(cache_data))
      cache_clean <- tidytable::as_tidytable(
        lapply(as.list(cache_data)[keep_cols], .clean_not_classified_col)
      )
      cache_clean <- tidytable::distinct(cache_clean)

      compress_method <- if (grepl("\\.gz$", cache_path, ignore.case = TRUE)) {
        "gzip"
      } else {
        "none"
      }
      tidytable::fwrite(
        cache_clean,
        file = cache_path,
        sep = "\t",
        na = "",
        compress = compress_method,
        showProgress = FALSE
      )
      log_debug(
        "Normalized %s cache file (%d entries): %s",
        taxonomy_name,
        nrow(cache_clean),
        cache_path
      )
      TRUE
    },
    error = function(e) {
      log_warn(
        "Failed to normalize %s cache file: %s",
        taxonomy_name,
        conditionMessage(e)
      )
      FALSE
    }
  )
}

# Helper function: Get existing keys from taxonomy table
.get_existing_keys <- function(taxonomy_table, key_col) {
  if (!is.null(taxonomy_table) && nrow(taxonomy_table) > 0) {
    taxonomy_table[[key_col]]
  } else {
    character(0)
  }
}

.subset_with_expected_cols <- function(df, expected_cols) {
  df_list <- as.list(df)
  missing_cols <- setdiff(expected_cols, names(df_list))
  if (length(missing_cols) > 0L) {
    df_list[missing_cols] <- list(rep.int(NA_character_, nrow(df)))
  }
  tidytable::as_tidytable(df_list[expected_cols])
}

# Helper function: Prepare cache supplement
.prepare_cache_supplement <- function(
  cache_data,
  key_col,
  all_keys,
  existing_keys,
  expected_cols,
  taxonomy_name
) {
  # Keep only cache entries for keys present in library but missing taxonomy
  u_all <- unique(all_keys)
  u_all <- u_all[!is.na(u_all)]
  missing_keys <- if (length(existing_keys) == 0L) {
    u_all
  } else {
    u_all[fastmatch::fmatch(u_all, existing_keys, nomatch = 0L) == 0L]
  }

  if (length(missing_keys) == 0) {
    log_info(
      "All %s keys already have %s taxonomy",
      length(all_keys),
      taxonomy_name
    )
    return(NULL)
  }

  idx <- fastmatch::fmatch(cache_data[[key_col]], missing_keys, nomatch = 0L) >
    0L
  cache_supplement <- cache_data[idx, ]

  if (nrow(cache_supplement) == 0) {
    log_info(
      "Additional %s cache has no entries for %d missing keys",
      taxonomy_name,
      length(missing_keys)
    )
    return(NULL)
  }

  # Select only matching columns, filling missing with NA
  .subset_with_expected_cols(cache_supplement, expected_cols) |>
    tidytable::distinct()
}

# Helper function: Read taxonomy cache file with automatic delimiter and
# headerless-TSV detection (e.g., ClassyFire cache files starting directly
# with InChIKey + CHEMONTID rows).
.read_taxonomy_cache_file <- function(
  cache_path,
  taxonomy_name,
  key_col,
  all_keys,
  expected_cols,
  col_mapping
) {
  open_con <- if (grepl("\\.bz2$", cache_path, ignore.case = TRUE)) {
    bzfile
  } else if (grepl("\\.(gz|bgz)$", cache_path, ignore.case = TRUE)) {
    gzfile
  } else {
    file
  }
  con <- open_con(cache_path, open = "rt")
  first_line <- tryCatch(
    readLines(con, n = 1L, warn = FALSE),
    finally = close(con)
  )
  if (length(first_line) == 0L || !nzchar(trimws(first_line))) {
    return(tidytable::tidytable())
  }

  sep <- if (grepl("\t", first_line, fixed = TRUE)) {
    "\t"
  } else if (grepl(",", first_line, fixed = TRUE)) {
    ","
  } else {
    "\t"
  }

  first_tokens <- strsplit(first_line, sep, fixed = TRUE)[[1L]]
  first_tokens <- gsub('^"|"$', "", trimws(first_tokens))

  known_cols <- tolower(unique(c(
    key_col,
    expected_cols,
    if (!is.null(col_mapping)) {
      c(names(col_mapping), unlist(col_mapping, use.names = FALSE))
    }
  )))

  looks_headerless <- !any(tolower(first_tokens) %in% known_cols) &&
    length(first_tokens) >= 2L &&
    (grepl("^[A-Z]{14}-[A-Z]{10}-[A-Z]$", first_tokens[[1L]]) ||
      grepl("^CHEMONTID:[0-9]+$", first_tokens[[2L]], ignore.case = TRUE) ||
      (length(all_keys) > 0L &&
        !is.na(fastmatch::fmatch(first_tokens[[1L]], all_keys))))

  quote_arg <- if (identical(sep, "\t") && looks_headerless) "" else "\""

  cache_data <- safe_fread(
    file = cache_path,
    file_type = paste(taxonomy_name, "additional cache"),
    sep = sep,
    header = !looks_headerless,
    quote = quote_arg,
    fill = TRUE,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  if (looks_headerless && ncol(cache_data) > 0L) {
    target_cols <- if (length(expected_cols) > 0L) {
      expected_cols
    } else if (!is.null(col_mapping) && length(col_mapping) > 0L) {
      names(col_mapping)
    } else {
      character(0L)
    }
    n_assign <- min(ncol(cache_data), length(target_cols))
    if (n_assign > 0L) {
      names(cache_data)[seq_len(n_assign)] <- target_cols[seq_len(n_assign)]
    }
  }

  attr(cache_data, ".needs_disk_normalization") <- isTRUE(looks_headerless) ||
    !identical(sep, "\t")
  cache_data
}

.cache_needs_disk_normalization <- function(
  cache_data,
  expected_cols,
  had_col_rename = FALSE,
  had_chemontid_norm = FALSE
) {
  if (
    isTRUE(attr(cache_data, ".needs_disk_normalization")) ||
      isTRUE(had_col_rename) ||
      isTRUE(had_chemontid_norm)
  ) {
    return(TRUE)
  }
  if (!identical(names(cache_data), expected_cols)) {
    return(TRUE)
  }
  any(vapply(
    expected_cols,
    function(col) {
      x <- cache_data[[col]]
      is.character(x) && any(!is.na(x) & x == "notClassified")
    },
    logical(1L)
  ))
}

# Helper function: Write back enriched taxonomy to cache
.write_enriched_taxonomy_to_cache <- function(
  enriched,
  cache_path,
  expected_cols,
  col_mapping,
  taxonomy_name,
  existing_cache = NULL,
  taxonomy_table = NULL,
  key_col = NULL,
  needs_disk_norm = TRUE
) {
  tryCatch(
    {
      # When existing_cache is already normalized on disk and taxonomy_table
      # contributes no keys absent from existing_cache, skip rewriting disk
      if (
        !is.null(existing_cache) &&
          !isTRUE(needs_disk_norm) &&
          !is.null(key_col) &&
          key_col %in% names(existing_cache)
      ) {
        if (
          is.null(taxonomy_table) ||
            nrow(taxonomy_table) == 0L ||
            !key_col %in% names(taxonomy_table)
        ) {
          return(invisible(FALSE))
        }
        tax_keys <- taxonomy_table[[key_col]]
        tax_keys <- tax_keys[!is.na(tax_keys)]
        if (
          length(tax_keys) == 0L ||
            !anyNA(fastmatch::fmatch(tax_keys, existing_cache[[key_col]]))
        ) {
          return(invisible(FALSE))
        }
      }

      # Reuse in-memory cache when provided; otherwise read from disk
      if (is.null(existing_cache)) {
        existing_cache <- tryCatch(
          safe_fread(
            file = cache_path,
            file_type = paste(taxonomy_name, "cache for write-back"),
            na.strings = c("", "NA"),
            colClasses = "character"
          ),
          error = function(e) tidytable::tidytable()
        )
      }

      # Apply same column mapping to existing cache before merging
      if (!is.null(col_mapping) && nrow(existing_cache) > 0) {
        existing_cache <- .apply_col_mapping(existing_cache, col_mapping)
      }

      # Keep only expected columns from existing cache
      if (nrow(existing_cache) > 0) {
        existing_cache <- .subset_with_expected_cols(
          existing_cache,
          expected_cols
        )
      }

      # Only rows from taxonomy_table can be new relative to existing_cache
      # (cache_supplement was extracted from existing_cache itself)
      source_for_new <- taxonomy_table %||% enriched
      cache_to_write <- if (
        !is.null(source_for_new) && nrow(source_for_new) > 0L
      ) {
        .subset_with_expected_cols(source_for_new, expected_cols)
      } else {
        tidytable::tidytable()
      }

      combined_cache <- if (nrow(cache_to_write) > 0L) {
        tidytable::bind_rows(existing_cache, cache_to_write)
      } else {
        existing_cache
      }
      combined_cache <- tidytable::as_tidytable(
        lapply(as.list(combined_cache), .clean_not_classified_col)
      )
      combined_cache <- if (
        !is.null(key_col) && key_col %in% names(combined_cache)
      ) {
        .fast_distinct_with_key_guard(combined_cache, key_col)
      } else {
        tidytable::distinct(combined_cache)
      }

      create_dir(export = cache_path)
      compress_method <- if (grepl("\\.gz$", cache_path, ignore.case = TRUE)) {
        "gzip"
      } else {
        "none"
      }
      tidytable::fwrite(
        combined_cache,
        file = cache_path,
        sep = "\t",
        na = "",
        compress = compress_method,
        showProgress = FALSE
      )
      log_info(
        "Updated additional %s cache (%d total entries): %s",
        taxonomy_name,
        nrow(combined_cache),
        cache_path
      )
      invisible(TRUE)
    },
    error = function(e) {
      log_warn(
        "Failed to write back to additional %s cache: %s",
        taxonomy_name,
        conditionMessage(e)
      )
      invisible(FALSE)
    }
  )
}

#' Enrich Taxonomy from Additional Cache
#'
#' @description Internal helper to supplement taxonomy tables with entries
#'   from additional external cache files. For structures present in the
#'   stereo/canonical tables but absent from the taxonomy table, matching
#'   rows from the cache are appended.
#'
#' @param taxonomy_table Existing taxonomy table (NPC or ClassyFire)
#' @param cache_path Path to additional cache file (TSV/TSV.gz)
#' @param key_col Column name used as key for matching (e.g., "structure_smiles"
#'   for NPC, "structure_inchikey" for ClassyFire)
#' @param all_keys Character vector of all keys present in the library
#'   (e.g., all unique SMILES or InChIKeys)
#' @param col_mapping Named list mapping internal column names to a character
#'   vector of possible cache column names. The first matching name found in
#'   the cache is used. Example:
#' `list("structure_tax_npc_01pat" = c("pathway",
#'     "structure_taxonomy_npclassifier_01pathway"))`
#' @param taxonomy_name Name for logging (e.g., "NPClassifier", "ClassyFire")
#'
#' @return Enriched taxonomy table
#' @keywords internal

enrich_taxonomy_from_cache <- function(
  taxonomy_table,
  cache_path,
  key_col,
  all_keys,
  col_mapping = NULL,
  taxonomy_name = "taxonomy"
) {
  # Early return if cache is not available
  if (is.null(cache_path) || !nzchar(cache_path) || !file.exists(cache_path)) {
    return(taxonomy_table)
  }

  log_info(
    "Enriching %s taxonomy from additional cache: %s",
    taxonomy_name,
    cache_path
  )

  expected_cols_hint <- if (
    !is.null(taxonomy_table) && ncol(taxonomy_table) > 0
  ) {
    names(taxonomy_table)
  } else if (!is.null(col_mapping) && length(col_mapping) > 0L) {
    names(col_mapping)
  } else {
    character(0L)
  }

  # Read cache data
  cache_data <- tryCatch(
    .read_taxonomy_cache_file(
      cache_path = cache_path,
      taxonomy_name = taxonomy_name,
      key_col = key_col,
      all_keys = all_keys,
      expected_cols = expected_cols_hint,
      col_mapping = col_mapping
    ),
    error = function(e) {
      log_warn(
        "Failed to read additional %s cache: %s",
        taxonomy_name,
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(cache_data)) {
    return(taxonomy_table)
  }

  if (nrow(cache_data) == 0) {
    log_info("Additional %s cache is empty", taxonomy_name)
    return(taxonomy_table)
  }

  raw_cache_names <- names(cache_data)
  if (!is.null(col_mapping)) {
    cache_data <- .apply_col_mapping(cache_data, col_mapping)
  }
  had_col_rename <- !identical(names(cache_data), raw_cache_names)

  # Verify key column exists in cache BEFORE touching the cache file on disk
  if (!key_col %in% names(cache_data)) {
    log_warn(
      "Key column '%s' not found in additional %s cache. Available: %s",
      key_col,
      taxonomy_name,
      paste(names(cache_data), collapse = ", ")
    )
    return(taxonomy_table)
  }

  # Normalize chemontid values
  old_chemontid <- if ("structure_tax_cla_chemontid" %in% names(cache_data)) {
    cache_data[["structure_tax_cla_chemontid"]]
  } else {
    NULL
  }
  chemontid_normalized <- .normalize_chemontid_in_tables(
    cache_data,
    taxonomy_table
  )
  cache_data <- chemontid_normalized$cache_data
  taxonomy_table <- chemontid_normalized$taxonomy_table
  had_chemontid_norm <- !is.null(old_chemontid) &&
    !identical(old_chemontid, cache_data[["structure_tax_cla_chemontid"]])
  rm(old_chemontid)

  # Determine expected columns
  expected_cols <- if (!is.null(taxonomy_table) && ncol(taxonomy_table) > 0) {
    names(taxonomy_table)
  } else {
    names(cache_data)
  }

  needs_disk_norm <- .cache_needs_disk_normalization(
    cache_data = cache_data,
    expected_cols = expected_cols,
    had_col_rename = had_col_rename,
    had_chemontid_norm = had_chemontid_norm
  )

  existing_keys <- .get_existing_keys(taxonomy_table, key_col)

  cache_supplement <- .prepare_cache_supplement(
    cache_data,
    key_col,
    all_keys,
    existing_keys,
    expected_cols,
    taxonomy_name
  )

  if (is.null(cache_supplement)) {
    if (needs_disk_norm) {
      # Normalize cache file on disk when format/columns needed normalization
      .normalize_and_write_cache(
        cache_data,
        cache_path,
        expected_cols,
        taxonomy_name
      )
    } else if (
      !is.null(taxonomy_table) &&
        nrow(taxonomy_table) > 0L &&
        key_col %in% names(taxonomy_table)
    ) {
      .write_enriched_taxonomy_to_cache(
        enriched = taxonomy_table,
        cache_path = cache_path,
        expected_cols = expected_cols,
        col_mapping = col_mapping,
        taxonomy_name = taxonomy_name,
        existing_cache = cache_data,
        taxonomy_table = taxonomy_table,
        key_col = key_col,
        needs_disk_norm = FALSE
      )
    }
    return(taxonomy_table)
  }

  # Combine enriched taxonomy
  enriched <- tidytable::bind_rows(taxonomy_table, cache_supplement) |>
    .fast_distinct_with_key_guard(key_col)

  n_added <- nrow(enriched) - nrow(taxonomy_table)
  log_info(
    "Enriched %s taxonomy with %d entries from additional cache (%d missing keys matched)",
    taxonomy_name,
    n_added,
    nrow(cache_supplement)
  )

  # Write back the full enriched taxonomy to cache for persistence (reusing in-memory cache_data)
  .write_enriched_taxonomy_to_cache(
    enriched = enriched,
    cache_path = cache_path,
    expected_cols = expected_cols,
    col_mapping = col_mapping,
    taxonomy_name = taxonomy_name,
    existing_cache = cache_data,
    taxonomy_table = taxonomy_table,
    key_col = key_col,
    needs_disk_norm = needs_disk_norm
  )

  enriched
}
