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
  # Validate filter parameter
  if (!is.logical(filter) || length(filter) != 1L) {
    stop("filter must be a single logical value (TRUE/FALSE)")
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

  for (param_name in names(output_paths)) {
    param_value <- output_paths[[param_name]]
    if (!is.character(param_value) || length(param_value) != 1L) {
      stop(param_name, " must be a single character string")
    }
  }

  logger::log_info("Preparing merged structure-organism pairs library")
  logger::log_debug("Filter mode: {filter}")

  if (isTRUE(filter)) {
    # Validate taxonomic filter parameters
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
        level
      )
    }

    if (!is.character(value) || length(value) != 1L) {
      stop("value must be a single character string")
    }
  }
  if (filter) {
    logger::log_info("Filtering by {level}: {value}")
  }

  # logger::log_trace("Loading and concatenating prepared libraries")
  libraries <- files |>
    purrr::map(
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

  tables <- libraries |>
    tidytable::bind_rows() |>
    split_tables_sop(cache = cache)

  # logger::log_trace("Keeping keys")
  table_keys <- tables$key |>
    data.frame()

  # logger::log_trace("Keeping organisms")
  table_org_tax_ott <- tables$org_tax_ott

  # logger::log_trace("Completing organisms taxonomy")
  table_org_tax_ott_2 <- table_keys |>
    tidytable::anti_join(table_org_tax_ott) |>
    tidytable::distinct(organism = organism_name) |>
    data.frame()

  if (nrow(table_org_tax_ott_2) != 0) {
    table_org_tax_ott_full <-
      table_org_tax_ott_2 |>
      get_organism_taxonomy_ott(retry = FALSE)

    table_org_tax_ott <-
      table_org_tax_ott |>
      tidytable::bind_rows(
        table_org_tax_ott_full |>
          tidytable::as_tidytable() |>
          tidytable::mutate(tidytable::across(
            .cols = tidyselect::where(is.numeric),
            .fns = as.character
          )) |>
          tidytable::mutate(tidytable::across(
            .cols = tidyselect::where(is.list),
            .fns = as.character
          )) |>
          tidytable::mutate(tidytable::across(
            .cols = tidyselect::where(is.logical),
            .fns = as.character
          ))
      )
  }

  # logger::log_trace("Keeping structures")
  table_structures_stereo <- tables$str_stereo
  table_structures_metadata <- tables$str_met
  table_structures_names <- tables$str_nam
  table_structures_taxonomy_cla <- tables$str_tax_cla
  table_structures_taxonomy_npc <- tables$str_tax_npc

  ## ISSUE see #19
  # logger::log_trace("Completing structures names")
  # logger::log_trace("Completing structures taxonomy (classyfire)")
  # logger::log_trace("Completing structures taxonomy (NPC)")

  ## If filter is TRUE,
  ## filter the library based on the specified level and value
  if (filter == TRUE) {
    # logger::log_trace("Filtering library")
    table_keys <- table_keys |>
      tidytable::left_join(table_org_tax_ott)

    table_keys <- table_keys |>
      tidytable::filter(grepl(
        x = !!as.name(colnames(table_keys)[grepl(
          pattern = level,
          x = colnames(table_keys),
          perl = TRUE
        )]),
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

    stopifnot(
      "Your filter led to no entries,
        try to change it." = nrow(table_keys) != 0
    )
  }

  export_params(
    parameters = get_params(step = "prepare_libraries_sop_merged"),
    step = "prepare_libraries_sop_merged"
  )
  export_output(x = table_keys, file = output_key)
  export_output(x = table_org_tax_ott, file = output_org_tax_ott)
  export_output(x = table_structures_stereo, file = output_str_stereo)
  export_output(x = table_structures_metadata, file = output_str_met)
  export_output(x = table_structures_names, file = output_str_nam)
  export_output(x = table_structures_taxonomy_cla, file = output_str_tax_cla)
  export_output(x = table_structures_taxonomy_npc, file = output_str_tax_npc)

  rm(
    table_keys,
    table_org_tax_ott,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_names,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc
  )
  return(
    c(
      "key" = output_key,
      "org_tax_ott" = output_org_tax_ott,
      "str_stereo" = output_str_stereo,
      "str_met" = output_str_met,
      "str_name" = output_str_nam,
      "str_tax_cla" = output_str_tax_cla,
      "str_tax_npc" = output_str_tax_npc
    )
  )
}
