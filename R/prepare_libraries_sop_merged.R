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
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::where(fn = is.numeric),
          .fns = as.character
        )) |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::where(fn = is.list),
          .fns = as.character
        )) |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::where(fn = is.logical),
          .fns = as.character
        ))
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
#'   `list("structure_tax_npc_01pat" = c("pathway", "structure_taxonomy_npclassifier_01pathway"))`
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
  if (is.null(cache_path) || !nzchar(cache_path) || !file.exists(cache_path)) {
    return(taxonomy_table)
  }

  log_info(
    "Enriching %s taxonomy from additional cache: %s",
    taxonomy_name,
    cache_path
  )

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
      return(taxonomy_table)
    }
  )

  if (is.null(cache_data) || nrow(cache_data) == 0) {
    log_info("Additional %s cache is empty", taxonomy_name)
    return(taxonomy_table)
  }

  # Apply column mapping if provided
  if (!is.null(col_mapping)) {
    for (internal_name in names(col_mapping)) {
      # Skip if internal name already exists in cache
      if (internal_name %in% names(cache_data)) {
        next
      }

      # Try each possible cache name in order
      possible_names <- col_mapping[[internal_name]]
      for (cache_name in possible_names) {
        if (cache_name %in% names(cache_data)) {
          names(cache_data)[names(cache_data) == cache_name] <- internal_name
          break
        }
      }
    }
  }

  # Normalize chemontid values to prevent duplicates from inconsistent
  # formatting (e.g., "2011" vs "0002011" vs "CHEMONTID:0002011")
  chemontid_col <- "structure_tax_cla_chemontid"
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

  # Determine expected columns for the cache
  expected_cols <- if (!is.null(taxonomy_table) && ncol(taxonomy_table) > 0) {
    names(taxonomy_table)
  } else {
    intersect(names(cache_data), names(cache_data))
  }

  # Normalize cache file on disk: harmonize column names, drop extra columns,
  # strip "notClassified", and normalize chemontid. This runs on every read so
  # the cache file converges to a clean internal format even when no enrichment
  # is needed (i.e., before early returns).
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
    },
    error = function(e) {
      log_warn(
        "Failed to normalize %s cache file: %s",
        taxonomy_name,
        conditionMessage(e)
      )
    }
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

  # Identify keys already covered by existing taxonomy
  existing_keys <- if (!is.null(taxonomy_table) && nrow(taxonomy_table) > 0) {
    taxonomy_table[[key_col]]
  } else {
    character(0)
  }

  # Keep only cache entries for keys present in the library but missing taxonomy
  missing_keys <- setdiff(all_keys, existing_keys)
  missing_keys <- missing_keys[!is.na(missing_keys)]

  if (length(missing_keys) == 0) {
    log_info(
      "All %s keys already have %s taxonomy",
      length(all_keys),
      taxonomy_name
    )
    return(taxonomy_table)
  }

  cache_supplement <- cache_data |>
    tidytable::filter(.data[[key_col]] %in% missing_keys)

  if (nrow(cache_supplement) == 0) {
    log_info(
      "Additional %s cache has no entries for %d missing keys",
      taxonomy_name,
      length(missing_keys)
    )
    return(taxonomy_table)
  }

  # Select only matching columns, filling missing with NA
  for (col in expected_cols) {
    if (!col %in% names(cache_supplement)) {
      cache_supplement[[col]] <- NA_character_
    }
  }
  cache_supplement <- cache_supplement |>
    tidytable::select(tidyselect::all_of(expected_cols)) |>
    tidytable::mutate(
      tidytable::across(
        tidyselect::everything(),
        ~ {
          x <- as.character(.x)
          x[is.na(x)] <- "notClassified"
          x
        }
      )
    ) |>
    tidytable::distinct()

  enriched <- tidytable::bind_rows(taxonomy_table, cache_supplement) |>
    tidytable::distinct()

  n_added <- nrow(enriched) - nrow(taxonomy_table)
  log_info(
    "Enriched %s taxonomy with %d entries from additional cache (%d missing keys matched)",
    taxonomy_name,
    n_added,
    nrow(cache_supplement)
  )

  # Write back the full enriched taxonomy to the additional cache so it
  # grows over time. Entries already present in the library but absent from
  # the cache are appended, making subsequent runs faster and richer.
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

  enriched
}

#' @title Prepare merged structure organism pairs libraries
#'
#' @description This function merges all structure-organism pair libraries
#'     (LOTUS, HMDB, ECMDB, etc.) into a single comprehensive library. Can
#'     optionally filter by taxonomic level to create biologically-focused
#'     subsets. Also splits structures into separate metadata tables.
#'
#' @details Creates merged library by combining all available SOP sources,
#'     optionally filtering by taxonomic criteria (e.g., only Gentianaceae).
#'     Splits output into structures metadata, names, taxonomy, and organisms.
#'
#' @include get_organism_taxonomy_ott.R
#' @include get_params.R
#' @include split_tables_sop.R
#'
#' @param files [character] Character vector or list of paths to prepared library files
#' @param filter [logical] Logical whether to filter the merged library by taxonomy
#' @param level [character] Character string taxonomic rank for filtering (kingdom, phylum,
#'     family, genus, etc.)
#' @param value [character] Character string taxon name(s) to keep (can use | for multiple,
#'     e.g., 'Gentianaceae|Apocynaceae')
#' @param cache [character] Character string path to cache directory for processed SMILES
#' @param additional_npc_cache [character] Optional path to an additional NPClassifier
#'   taxonomy cache file (TSV/TSV.gz). Structures present in the merged library
#'   but missing NPClassifier taxonomy will be looked up in this cache. Expected
#'   columns: `structure_smiles`, `structure_tax_npc_01pat`,
#'   `structure_tax_npc_02sup`, `structure_tax_npc_03cla`. Alternative column
#'   names from external tools (e.g., `pathway`, `superclass`, `class`) are also
#'   supported.
#' @param additional_cla_cache [character] Optional path to an additional ClassyFire
#'   taxonomy cache file (TSV/TSV.gz). Structures present in the merged library
#'   but missing ClassyFire taxonomy will be looked up in this cache. Expected
#'   columns: `structure_inchikey`, `structure_tax_cla_chemontid`,
#'   `structure_tax_cla_01kin`, `structure_tax_cla_02sup`,
#'   `structure_tax_cla_03cla`, `structure_tax_cla_04dirpar`. Alternative column
#'   names (e.g., `inchikey`, `chemontid`, `kingdom`, `superclass`, `class`,
#'   `directparent`) are also supported.
#' @param output_key [character] Character string path for output keys file
#' @param output_org_tax_ott [character] Character string path for organisms taxonomy (OTT) file
#' @param output_str_can [character] Character string path for structures canonical SMILES file
#' @param output_str_stereo [character] Character string path for structures stereochemistry file
#' @param output_str_met [character] Character string path for structures metadata file
#' @param output_str_tax_cla [character] Character string path for ClassyFire taxonomy file
#' @param output_str_tax_npc [character] Character string path for NPClassifier taxonomy file
#'
#' @return Character string path to the prepared merged SOP library
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' files <- get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$prepared$lotus |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' get_file(url = paste0(dir, files), export = files)
#' prepare_libraries_sop_merged(files = files)
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_merged <- function(
  files = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$prepared,
  filter = get_params(
    step = "prepare_libraries_sop_merged"
  )$organisms$filter$mode,
  level = get_params(
    step = "prepare_libraries_sop_merged"
  )$organisms$filter$level,
  value = get_params(
    step = "prepare_libraries_sop_merged"
  )$organisms$filter$value,
  cache = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$processed,
  additional_npc_cache = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$additional_npc,
  additional_cla_cache = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$additional_cla,
  output_key = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$keys,
  ## document it above in case
  # output_org_nam = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$organisms$names,
  output_org_tax_ott = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$organisms$taxonomies$ott,
  output_str_can = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$canonical,
  output_str_stereo = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$stereo,
  output_str_met = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$metadata,
  output_str_tax_cla = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  output_str_tax_npc = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$npc
) {
  # Initialize logging context
  ctx <- log_operation(
    "prepare_libraries_sop_merged",
    n_libraries = length(files),
    filter_enabled = isTRUE(filter),
    filter_level = if (isTRUE(filter)) level else "none"
  )

  # Input Validation ----

  validate_sop_merged_inputs(
    files = files,
    filter = filter,
    level = level,
    value = value,
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_can = output_str_can,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  # Load and Process Libraries ----
  log_metadata(ctx, phase = "loading", n_files = length(files))

  tables <- load_and_merge_libraries(files, cache)

  table_keys <- tables$key |>
    data.frame()
  table_org_tax_ott <- tables$org_tax_ott

  # Complete organism taxonomy
  table_org_tax_ott <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  # Extract structure tables
  table_structures_canonical <- tables$str_can
  table_structures_stereo <- tables$str_stereo
  table_structures_metadata <- tables$str_met
  table_structures_taxonomy_cla <- tables$str_tax_cla
  table_structures_taxonomy_npc <- tables$str_tax_npc

  # Enrich taxonomy from additional caches (if provided) ----

  # NPClassifier: keyed by structure_smiles
  if (
    !is.null(additional_npc_cache) &&
      length(additional_npc_cache) == 1L &&
      nzchar(additional_npc_cache)
  ) {
    all_smiles <- table_structures_stereo |>
      tidytable::filter(!is.na(structure_smiles)) |>
      tidytable::pull(structure_smiles) |>
      unique()

    table_structures_taxonomy_npc <- enrich_taxonomy_from_cache(
      taxonomy_table = table_structures_taxonomy_npc,
      cache_path = additional_npc_cache,
      key_col = "structure_smiles",
      all_keys = all_smiles,
      col_mapping = list(
        "structure_smiles" = "smiles",
        "structure_tax_npc_01pat" = c(
          "pathway",
          "structure_taxonomy_npclassifier_01pathway"
        ),
        "structure_tax_npc_02sup" = c(
          "superclass",
          "structure_taxonomy_npclassifier_02superclass"
        ),
        "structure_tax_npc_03cla" = c(
          "class",
          "structure_taxonomy_npclassifier_03class"
        )
      ),
      taxonomy_name = "NPClassifier"
    )
  }

  # ClassyFire: keyed by structure_inchikey
  if (
    !is.null(additional_cla_cache) &&
      length(additional_cla_cache) == 1L &&
      nzchar(additional_cla_cache)
  ) {
    all_inchikeys <- table_keys |>
      tidytable::filter(!is.na(structure_inchikey)) |>
      tidytable::pull(structure_inchikey) |>
      unique()

    table_structures_taxonomy_cla <- enrich_taxonomy_from_cache(
      taxonomy_table = table_structures_taxonomy_cla,
      cache_path = additional_cla_cache,
      key_col = "structure_inchikey",
      all_keys = all_inchikeys,
      col_mapping = list(
        "structure_inchikey" = "inchikey",
        "structure_tax_cla_chemontid" = c(
          "chemontid",
          "structure_taxonomy_classyfire_chemontid"
        ),
        "structure_tax_cla_01kin" = c(
          "kingdom",
          "structure_taxonomy_classyfire_01kingdom"
        ),
        "structure_tax_cla_02sup" = c(
          "superclass",
          "structure_taxonomy_classyfire_02superclass"
        ),
        "structure_tax_cla_03cla" = c(
          "class",
          "structure_taxonomy_classyfire_03class"
        ),
        "structure_tax_cla_04dirpar" = c(
          "directparent",
          "structure_taxonomy_classyfire_04directparent"
        )
      ),
      taxonomy_name = "ClassyFire"
    )
  }

  # Apply Taxonomic Filter (if enabled) ----

  if (filter) {
    n_pre_filter <- nrow(table_keys)
    log_metadata(
      ctx,
      phase = "filtering",
      n_pre_filter = n_pre_filter,
      filter_level = level,
      filter_value = value
    )

    table_keys <- apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level,
      value
    )

    log_metadata(
      ctx,
      n_post_filter = nrow(table_keys),
      n_removed = n_pre_filter - nrow(table_keys)
    )
  }

  # Export Results ----
  log_metadata(
    ctx,
    phase = "exporting",
    n_structures = nrow(table_structures_stereo),
    n_organisms = nrow(table_org_tax_ott)
  )

  export_params(
    parameters = get_params(step = "prepare_libraries_sop_merged"),
    step = "prepare_libraries_sop_merged"
  )

  result <- export_library_tables(
    table_keys = table_keys,
    table_org_tax_ott = table_org_tax_ott,
    table_structures_canonical = table_structures_canonical,
    table_structures_stereo = table_structures_stereo,
    table_structures_metadata = table_structures_metadata,
    table_structures_taxonomy_cla = table_structures_taxonomy_cla,
    table_structures_taxonomy_npc = table_structures_taxonomy_npc,
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_can = output_str_can,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  log_complete(
    ctx,
    n_pairs = nrow(table_keys),
    n_structures = nrow(table_structures_stereo),
    n_organisms = nrow(table_org_tax_ott),
    files_exported = length(result)
  )

  # Clean up
  rm(
    table_keys,
    table_org_tax_ott,
    table_structures_canonical,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc,
    tables
  )

  return(result)
}
