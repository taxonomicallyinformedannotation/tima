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
#' @param files [character] Character vector or list of paths to prepared
#'     library files
#' @param filter [logical] Logical whether to filter the merged library by
#'     taxonomy
#' @param level [character] Character string taxonomic rank for filtering
#'     (kingdom, phylum,
#'     family, genus, etc.)
#' @param value [character] Character string taxon name(s) to keep (can use |
#'     for multiple,
#'     e.g., 'Gentianaceae|Apocynaceae')
#' @param cache [character] Character string path to cache directory for
#'     processed SMILES
#' @param npc_cache [character] Optional path to an additional NPClassifier
#'   taxonomy cache file (TSV/TSV.gz). Structures present in the merged library
#'   but missing NPClassifier taxonomy will be looked up in this cache. Expected
#'   columns: `structure_smiles`, `structure_tax_npc_01pat`,
#'   `structure_tax_npc_02sup`, `structure_tax_npc_03cla`. Alternative column
#'   names from external tools (e.g., `pathway`, `superclass`, `class`) are also
#'   supported.
#' @param cla_cache [character] Optional path to an additional ClassyFire
#'   taxonomy cache file (TSV/TSV.gz). Structures present in the merged library
#'   but missing ClassyFire taxonomy will be looked up in this cache. Expected
#'   columns: `structure_inchikey`, `structure_tax_cla_chemontid`,
#'   `structure_tax_cla_01kin`, `structure_tax_cla_02sup`,
#'   `structure_tax_cla_03cla`, `structure_tax_cla_04dirpar`. Alternative column
#'   names (e.g., `inchikey`, `chemontid`, `kingdom`, `superclass`, `class`,
#'   `directparent`) are also supported.
#' @param output_key [character] Character string path for output keys file
#' @param output_org_tax_ott [character] Character string path for organisms
#'     taxonomy (OTT) file
#' @param output_str_can [character] Character string path for structures
#'     canonical SMILES file
#' @param output_str_stereo [character] Character string path for structures
#'     stereochemistry file
#' @param output_str_met [character] Character string path for structures
#'     metadata file
#' @param output_str_tax_cla [character] Character string path for ClassyFire
#'     taxonomy file
#' @param output_str_tax_npc [character] Character string path for NPClassifier
#'     taxonomy file
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
#' files <- get_params(step =
#'     "prepare_libraries_sop_merged")$files$libraries$sop$prepared$lotus |>
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
  npc_cache = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$n,
  cla_cache = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$structures$taxonomies$c,
  output_key = get_params(
    step = "prepare_libraries_sop_merged"
  )$files$libraries$sop$merged$keys,
  ## document it above in case
  # output_org_nam = get_params(step =
  # "prepare_libraries_sop_merged")$files$libraries$sop$merged$organisms$names,
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
    !is.null(npc_cache) &&
      length(npc_cache) == 1L &&
      nzchar(npc_cache)
  ) {
    all_smiles <- table_structures_stereo |>
      tidytable::filter(!is.na(structure_smiles)) |>
      tidytable::pull(structure_smiles) |>
      unique()

    table_structures_taxonomy_npc <- enrich_taxonomy_from_cache(
      taxonomy_table = table_structures_taxonomy_npc,
      cache_path = npc_cache,
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
    !is.null(cla_cache) &&
      length(cla_cache) == 1L &&
      nzchar(cla_cache)
  ) {
    all_inchikeys <- table_keys |>
      tidytable::filter(!is.na(structure_inchikey)) |>
      tidytable::pull(structure_inchikey) |>
      unique()

    table_structures_taxonomy_cla <- enrich_taxonomy_from_cache(
      taxonomy_table = table_structures_taxonomy_cla,
      cache_path = cla_cache,
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

  result
}
