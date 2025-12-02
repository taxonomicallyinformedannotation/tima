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
  output_str_stereo,
  output_str_met,
  output_str_nam,
  output_str_tax_cla,
  output_str_tax_npc
) {
  # Validate filter parameter
  if (!is.logical(filter) || length(filter) != 1L) {
    stop("filter must be a single logical value (TRUE/FALSE)", call. = FALSE)
  }

  # Validate output paths
  output_paths <- list(
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_nam = output_str_nam,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  # Validate all output paths are single character strings
  are_valid <- vapply(
    output_paths,
    function(p) {
      is.character(p) && length(p) == 1L
    },
    logical(1L)
  )

  if (!all(are_valid)) {
    invalid_params <- names(output_paths)[!are_valid]
    stop(
      "The following output parameters must be single character strings: ",
      paste(invalid_params, collapse = ", "),
      call. = FALSE
    )
  }

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
      stop(
        "level must be one of: ",
        paste(valid_levels, collapse = ", "),
        ". Got: ",
        level,
        call. = FALSE
      )
    }

    if (!is.character(value) || length(value) != 1L) {
      stop("value must be a single character string", call. = FALSE)
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
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
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
    stop("No column found matching level: ", level, call. = FALSE)
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
    stop(
      "Filter led to no entries. Level: ",
      level,
      ", Value: ",
      value,
      ". Try different criteria.",
      call. = FALSE
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
  table_structures_stereo,
  table_structures_metadata,
  table_structures_names,
  table_structures_taxonomy_cla,
  table_structures_taxonomy_npc,
  output_key,
  output_org_tax_ott,
  output_str_stereo,
  output_str_met,
  output_str_nam,
  output_str_tax_cla,
  output_str_tax_npc
) {
  export_output(x = table_keys, file = output_key)
  export_output(x = table_org_tax_ott, file = output_org_tax_ott)
  export_output(x = table_structures_stereo, file = output_str_stereo)
  export_output(x = table_structures_metadata, file = output_str_met)
  export_output(x = table_structures_names, file = output_str_nam)
  export_output(x = table_structures_taxonomy_cla, file = output_str_tax_cla)
  export_output(x = table_structures_taxonomy_npc, file = output_str_tax_npc)

  c(
    "key" = output_key,
    "org_tax_ott" = output_org_tax_ott,
    "str_stereo" = output_str_stereo,
    "str_met" = output_str_met,
    "str_name" = output_str_nam,
    "str_tax_cla" = output_str_tax_cla,
    "str_tax_npc" = output_str_tax_npc
  )
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
#' @param files Character vector or list of paths to prepared library files
#' @param filter Logical whether to filter the merged library by taxonomy
#' @param level Character string taxonomic rank for filtering (kingdom, phylum,
#'     family, genus, etc.)
#' @param value Character string taxon name(s) to keep (can use | for multiple,
#'     e.g., 'Gentianaceae|Apocynaceae')
#' @param cache Character string path to cache directory for processed SMILES
#' @param output_key Character string path for output keys file
#' @param output_org_tax_ott Character string path for organisms taxonomy (OTT) file
#' @param output_str_stereo Character string path for structures stereochemistry file
#' @param output_str_met Character string path for structures metadata file
#' @param output_str_nam Character string path for structures names file
#' @param output_str_tax_cla Character string path for ClassyFire taxonomy file
#' @param output_str_tax_npc Character string path for NPClassifier taxonomy file
#'
#' @return Character string path to the prepared merged SOP library
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
  output_key = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$keys,
  ## document it above in case
  # output_org_nam = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$organisms$names,
  output_org_tax_ott = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$organisms$taxonomies$ott,
  output_str_stereo = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$stereo,
  output_str_met = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$metadata,
  output_str_nam = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$names,
  output_str_tax_cla = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  output_str_tax_npc = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$npc
) {
  # Input Validation ----

  validate_sop_merged_inputs(
    files = files,
    filter = filter,
    level = level,
    value = value,
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_nam = output_str_nam,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  log_info("Preparing merged structure-organism pairs library")
  log_debug("Filter mode: %s", filter)
  if (filter) {
    log_info("Filtering by %s: %s", level, value)
  }

  # Load and Process Libraries ----

  tables <- load_and_merge_libraries(files, cache)

  table_keys <- tables$key |> data.frame()
  table_org_tax_ott <- tables$org_tax_ott

  # Complete organism taxonomy
  table_org_tax_ott <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  # Extract structure tables
  table_structures_stereo <- tables$str_stereo
  table_structures_metadata <- tables$str_met
  table_structures_names <- tables$str_nam
  table_structures_taxonomy_cla <- tables$str_tax_cla
  table_structures_taxonomy_npc <- tables$str_tax_npc

  # Apply Taxonomic Filter (if enabled) ----

  if (filter) {
    table_keys <- apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level,
      value
    )
  }

  # Export Results ----

  export_params(
    parameters = get_params(step = "prepare_libraries_sop_merged"),
    step = "prepare_libraries_sop_merged"
  )

  result <- export_library_tables(
    table_keys = table_keys,
    table_org_tax_ott = table_org_tax_ott,
    table_structures_stereo = table_structures_stereo,
    table_structures_metadata = table_structures_metadata,
    table_structures_names = table_structures_names,
    table_structures_taxonomy_cla = table_structures_taxonomy_cla,
    table_structures_taxonomy_npc = table_structures_taxonomy_npc,
    output_key = output_key,
    output_org_tax_ott = output_org_tax_ott,
    output_str_stereo = output_str_stereo,
    output_str_met = output_str_met,
    output_str_nam = output_str_nam,
    output_str_tax_cla = output_str_tax_cla,
    output_str_tax_npc = output_str_tax_npc
  )

  # Clean up
  rm(
    table_keys,
    table_org_tax_ott,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_names,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc,
    tables
  )

  return(result)
}
