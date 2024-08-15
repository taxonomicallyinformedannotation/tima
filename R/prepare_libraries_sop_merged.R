#' @title Prepare merged structure organism pairs libraries
#'
#' @description This function prepares the libraries made of
#'    all sub-libraries containing structure-organism pairs
#'
#' @details It can be restricted to specific taxa to have
#'    more biologically meaningful annotation.
#'
#' @include get_organism_taxonomy_ott.R
#' @include get_params.R
#' @include split_tables_sop.R
#'
#' @param files List of libraries to be merged
#' @param filter Boolean. TRUE or FALSE if you want to filter the library
#' @param level Biological rank to be filtered.
#'    Kingdom, phylum, family, genus, ...
#' @param value Name of the taxon or taxa to be kept,
#'    e.g. 'Gentianaceae|Apocynaceae'
#' @param output_key Output file for keys
#' @param output_org_tax_ott Output file for organisms taxonomy (OTT)
#' @param output_str_stereo Output file for structures stereo
#' @param output_str_met Output file for structures metadata
#' @param output_str_nam Output file for structures names
#' @param output_str_tax_cla Output file for structures taxonomy (Classyfire)
#' @param output_str_tax_npc Output file for structures taxonomy (NPC)
#'
#' @return The path to the prepared structure-organism pairs library MERGED
#'
#' @export
#'
#' @examples
#' tima:::copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' files <- get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$prepared$lotus |>
#'   gsub(
#'     pattern = ".gz",
#'     replacement = "",
#'     fixed = TRUE
#'   )
#' get_file(url = paste0(dir, files), export = files)
#' prepare_libraries_sop_merged(files = files)
#' unlink("data", recursive = TRUE)
prepare_libraries_sop_merged <-
  function(files = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$prepared,
           filter = get_params(step = "prepare_libraries_sop_merged")$organisms$filter$mode,
           level = get_params(step = "prepare_libraries_sop_merged")$organisms$filter$level,
           value = get_params(step = "prepare_libraries_sop_merged")$organisms$filter$value,
           output_key = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$keys,
           ## document it above in case
           # output_org_nam = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$organisms$names,
           output_org_tax_ott = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$organisms$taxonomies$ott,
           output_str_stereo = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$stereo,
           output_str_met = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$metadata,
           output_str_nam = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$names,
           output_str_tax_cla = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$taxonomies$cla,
           output_str_tax_npc = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$taxonomies$npc) {
    stopifnot(
      "Your filter parameter must be 'true' or 'false'" =
        filter %in% c(TRUE, FALSE)
    )

    if (isTRUE(filter)) {
      stopifnot(
        "Your level parameter must be one of:
      'domain',
      'kingdom',
      'phylum',
      'class',
      'order',
      'family',
      'tribe',
      'genus',
      'species',
      'varietas'
      " = level %in% c(
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
      )
    }

    log_debug(x = "Loading and concatenating prepared libraries")
    libraries <- files |>
      lapply(
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      )

    tables <- libraries |>
      tidytable::bind_rows() |>
      split_tables_sop()

    log_debug(x = "Keeping keys")
    table_keys <- tables$key |>
      data.frame()

    log_debug(x = "Keeping organisms")
    table_org_tax_ott <- tables$org_tax_ott

    log_debug(x = "Completing organisms taxonomy")
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
              .cols = tidyselect::where(is.numeric), .fns = as.character
            )) |>
            tidytable::mutate(tidytable::across(
              .cols = tidyselect::where(is.list), .fns = as.character
            )) |>
            tidytable::mutate(tidytable::across(
              .cols = tidyselect::where(is.logical), .fns = as.character
            ))
        )
    }

    log_debug(x = "Keeping structures")
    table_structures_stereo <- tables$str_stereo
    table_structures_metadata <- tables$str_met
    table_structures_names <- tables$str_nam
    table_structures_taxonomy_cla <- tables$str_tax_cla
    table_structures_taxonomy_npc <- tables$str_tax_npc

    ## ISSUE see #19
    # log_debug(x = "Completing structures metadata")
    # log_debug(x = "Completing structures names")
    # log_debug(x = "Completing structures taxonomy (classyfire)")
    # log_debug(x = "Completing structures taxonomy (NPC)")

    ## If filter is TRUE,
    ## filter the library based on the specified level and value
    if (filter == TRUE) {
      log_debug(x = "Filtering library")
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
          structure_smiles,
          organism_name,
          reference_doi
        ) |>
        tidytable::distinct()

      stopifnot("Your filter led to no entries,
        try to change it." = nrow(table_keys) != 0)
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
