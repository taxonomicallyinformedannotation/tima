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
  # Validate files parameter
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

  # Validate filter parameter
  if (!is.logical(filter) || length(filter) != 1L) {
    cli::cli_abort(
      "filter must be a single logical value (TRUE/FALSE)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate output paths
  output_paths <- list(
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_can = output_str_can,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  # Validate all output paths are single character strings
  validate_all_single_strings(output_paths, "Output parameter(s)")

  # Validate taxonomic filter parameters if filtering
  if (isTRUE(filter)) {
    valid_levels <- c(
      "domain",
      "kingdom",
      "phylum",
      "class",
      "order",
      "family",
      "tribe",
      "genus",
      "species",
      "varietas"
    )

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
  libraries <- files |>
    purrr::map(
      .f = function(file) {
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
  table_org_tax_ott_2 <- table_keys |>
    tidytable::anti_join(y = table_org_tax_ott, by = "organism_name") |>
    tidytable::distinct(organism = organism_name) |>
    # Filter out NA organism names before querying API
    tidytable::filter(!is.na(organism)) |>
    data.frame()

  if (nrow(table_org_tax_ott_2) == 0) {
    return(table_org_tax_ott)
  }

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
  table_keys_filtered <- table_keys |>
    tidytable::left_join(y = table_org_tax_ott, by = "organism_name")

  # Find column matching the taxonomic level
  level_col <- colnames(table_keys_filtered)[grepl(
    pattern = level,
    x = colnames(table_keys_filtered),
    perl = TRUE
  )]

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

  table_keys_filtered <- table_keys_filtered |>
    tidytable::filter(grepl(
      x = !!as.name(level_col),
      pattern = value,
      perl = TRUE
    )) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles_no_stereo,
      organism_name,
      reference_doi
    ) |>
    tidytable::distinct()

  if (nrow(table_keys_filtered) == 0) {
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

  table_keys_filtered
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
#' @param df Data frame to rename columns in
#' @param col_mapping Named list: names are target col names, values are
#'   character vectors of possible source names (tried in order)
#' @return Data frame with renamed columns
#' @keywords internal
.apply_col_mapping <- function(df, col_mapping) {
  df_names <- names(df)
  targets_needed <- setdiff(names(col_mapping), df_names)
  for (target in targets_needed) {
    src <- intersect(col_mapping[[target]], df_names)
    if (length(src) > 0L) {
      df_names[df_names == src[[1L]]] <- target
    }
  }
  names(df) <- df_names
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

# Helper function: Normalize and write cache file
.normalize_and_write_cache <- function(
  cache_data,
  cache_path,
  expected_cols,
  taxonomy_name
) {
  tryCatch(
    {
      cache_clean <- cache_data |>
        tidytable::select(tidyselect::any_of(expected_cols)) |>
        tidytable::mutate(
          tidytable::across(
            tidyselect::everything(),
            ~ {
              x <- as.character(.x)
              x[!is.na(x) & x == "notClassified"] <- NA_character_
              x
            }
          )
        ) |>
        tidytable::distinct()

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
  missing_keys <- setdiff(all_keys, existing_keys)
  missing_keys <- missing_keys[!is.na(missing_keys)]

  if (length(missing_keys) == 0) {
    log_info(
      "All %s keys already have %s taxonomy",
      length(all_keys),
      taxonomy_name
    )
    return(NULL)
  }

  cache_supplement <- cache_data |>
    tidytable::filter(.data[[key_col]] %in% missing_keys)

  if (nrow(cache_supplement) == 0) {
    log_info(
      "Additional %s cache has no entries for %d missing keys",
      taxonomy_name,
      length(missing_keys)
    )
    return(NULL)
  }

  # Select only matching columns, filling missing with NA
  missing_cols <- setdiff(expected_cols, names(cache_supplement))
  for (col in missing_cols) {
    cache_supplement[[col]] <- NA_character_
  }
  cache_supplement <- cache_supplement |>
    tidytable::select(tidyselect::all_of(expected_cols)) |>
    tidytable::distinct()

  cache_supplement
}

# Helper function: Write back enriched taxonomy to cache
.write_enriched_taxonomy_to_cache <- function(
  enriched,
  cache_path,
  expected_cols,
  col_mapping,
  taxonomy_name
) {
  tryCatch(
    {
      cache_to_write <- enriched |>
        tidytable::select(tidyselect::all_of(expected_cols)) |>
        tidytable::distinct()

      # Read existing cache again (may have entries for keys NOT in this library)
      existing_cache <- tryCatch(
        safe_fread(
          file = cache_path,
          file_type = paste(taxonomy_name, "cache for write-back"),
          na.strings = c("", "NA"),
          colClasses = "character"
        ),
        error = function(e) tidytable::tidytable()
      )

      # Apply same column mapping to existing cache before merging
      if (!is.null(col_mapping) && nrow(existing_cache) > 0) {
        for (internal_name in names(col_mapping)) {
          if (internal_name %in% names(existing_cache)) {
            next
          }
          possible_names <- col_mapping[[internal_name]]
          for (cache_name in possible_names) {
            if (cache_name %in% names(existing_cache)) {
              names(existing_cache)[
                names(existing_cache) == cache_name
              ] <- internal_name
              break
            }
          }
        }
      }

      # Keep only expected columns from existing cache
      if (nrow(existing_cache) > 0) {
        for (col in expected_cols) {
          if (!col %in% names(existing_cache)) {
            existing_cache[[col]] <- NA_character_
          }
        }
        existing_cache <- existing_cache |>
          tidytable::select(tidyselect::all_of(expected_cols))
      }

      combined_cache <- tidytable::bind_rows(existing_cache, cache_to_write) |>
        tidytable::mutate(
          tidytable::across(
            tidyselect::everything(),
            ~ {
              x <- as.character(.x)
              x[!is.na(x) & x == "notClassified"] <- NA_character_
              x
            }
          )
        ) |>
        tidytable::distinct()

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
    },
    error = function(e) {
      log_warn(
        "Failed to write back to additional %s cache: %s",
        taxonomy_name,
        conditionMessage(e)
      )
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

  # Read cache data
  cache_data <- tryCatch(
    safe_fread(
      file = cache_path,
      file_type = paste(taxonomy_name, "additional cache"),
      na.strings = c("", "NA"),
      colClasses = "character"
    ),
    error = function(e) {
      log_warn(
        "Failed to read additional %s cache: %s",
        taxonomy_name,
        conditionMessage(e)
      )
      taxonomy_table
    }
  )

  if (is.null(cache_data) || nrow(cache_data) == 0) {
    log_info("Additional %s cache is empty", taxonomy_name)
    return(taxonomy_table)
  }

  # Apply column mapping if provided
  if (!is.null(col_mapping)) {
    cache_data <- .apply_col_mapping(cache_data, col_mapping)
  }

  # Normalize chemontid values
  chemontid_normalized <- .normalize_chemontid_in_tables(
    cache_data,
    taxonomy_table
  )
  cache_data <- chemontid_normalized$cache_data
  taxonomy_table <- chemontid_normalized$taxonomy_table

  # Determine expected columns
  expected_cols <- if (!is.null(taxonomy_table) && ncol(taxonomy_table) > 0) {
    names(taxonomy_table)
  } else {
    intersect(names(cache_data), names(cache_data))
  }

  # Normalize cache file on disk
  .normalize_and_write_cache(
    cache_data,
    cache_path,
    expected_cols,
    taxonomy_name
  )

  # Verify key column exists in cache
  if (!key_col %in% names(cache_data)) {
    log_warn(
      "Key column '%s' not found in additional %s cache. Available: %s",
      key_col,
      taxonomy_name,
      paste(names(cache_data), collapse = ", ")
    )
    return(taxonomy_table)
  }

  # Get existing keys
  existing_keys <- .get_existing_keys(taxonomy_table, key_col)

  # Prepare cache supplement
  cache_supplement <- .prepare_cache_supplement(
    cache_data,
    key_col,
    all_keys,
    existing_keys,
    expected_cols,
    taxonomy_name
  )

  if (is.null(cache_supplement)) {
    return(taxonomy_table)
  }

  # Combine enriched taxonomy
  enriched <- tidytable::bind_rows(taxonomy_table, cache_supplement) |>
    tidytable::distinct()

  n_added <- nrow(enriched) - nrow(taxonomy_table)
  log_info(
    "Enriched %s taxonomy with %d entries from additional cache (%d missing keys matched)",
    taxonomy_name,
    n_added,
    nrow(cache_supplement)
  )

  # Write back the full enriched taxonomy to cache for persistence
  .write_enriched_taxonomy_to_cache(
    enriched,
    cache_path,
    expected_cols,
    col_mapping,
    taxonomy_name
  )

  enriched
}
