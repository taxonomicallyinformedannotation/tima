utils::globalVariables(
  c(
    "organism_name",
    "params",
    "paths",
    "reference_doi",
    "structure_inchikey",
    "structure_smiles"
  )
)

#' @title Prepare merged structure organism pairs libraries
#'
#' @description This function prepares the libraries made of
#'    all sub-libraries containing structure-organism pairs
#'
#' @details It can be restricted to specific taxa to have
#'    more biologically meaningful annotation.
#'
#' @include export_output.R
#' @include export_params.R
#' @include get_organism_taxonomy_ott.R
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
#' @param output_str_2d_3d Output file for structures (2D + 3D)
#' @param output_str_met Output file for structures metadata
#' @param output_str_nam Output file for structures names
#' @param output_str_tax_cla Output file for structures taxonomy (Classyfire)
#' @param output_str_tax_npc Output file for structures taxonomy (NPC)
#' @param parameters Param
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_sop_merged <-
  function(files = params$files$libraries$sop$prepared,
           filter = params$organisms$filter$mode,
           level = params$organisms$filter$level,
           value = params$organisms$filter$value,
           output_key = params$files$libraries$sop$merged$keys,
           ## document it above in case
           # output_org_nam =
           # params$files$libraries$sop$merged$organisms$names,
           output_org_tax_ott =
             params$files$libraries$sop$merged$organisms$taxonomies$ott,
           output_str_2d_3d =
             params$files$libraries$sop$merged$structures$dd_ddd,
           output_str_met =
             params$files$libraries$sop$merged$structures$metadata,
           output_str_nam =
             params$files$libraries$sop$merged$structures$names,
           output_str_tax_cla =
             params$files$libraries$sop$merged$structures$taxonomies$cla,
           output_str_tax_npc =
             params$files$libraries$sop$merged$structures$taxonomies$npc,
           parameters = params) {
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
    params <<- parameters
    libraries <- list()
    for (i in seq_along(files)) {
      libraries[[i]] <- tidytable::fread(
        file = files[[i]],
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }

    tables <- tidytable::bind_rows(libraries) |>
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
            tidytable::mutate(
              tidytable::across(
                .cols = tidytable::where(is.numeric),
                .fns = as.character
              )
            ) |>
            tidytable::mutate(
              tidytable::across(
                .cols = tidytable::where(is.list),
                .fns = as.character
              )
            ) |>
            tidytable::mutate(
              tidytable::across(
                .cols = tidytable::where(is.logical),
                .fns = as.character
              )
            )
        )
    }

    log_debug(x = "Keeping structures")
    table_structures_2d_3d <- tables$str_2d_3d
    table_structures_metadata <- tables$str_met
    table_structures_names <- tables$str_nam
    table_structures_taxonomy_cla <- tables$str_tax_cla
    table_structures_taxonomy_npc <- tables$str_tax_npc

    log_debug(x = "Completing structures metadata")
    log_debug(x = "TODO")

    log_debug(x = "Completing structures names")
    log_debug(x = "TODO")

    log_debug(x = "Completing structures taxonomy (classyfire)")
    log_debug(x = "TODO")

    log_debug(x = "Completing structures taxonomy (NPC)")
    log_debug(x = "TODO")

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
            x = colnames(table_keys)
          )]),
          pattern = value
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

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_libraries_sop_merged")
    export_output(
      x = table_keys,
      file = output_key
    )
    export_output(
      x = table_org_tax_ott,
      file = output_org_tax_ott
    )
    export_output(
      x = table_structures_2d_3d,
      file = output_str_2d_3d
    )
    export_output(
      x = table_structures_metadata,
      file = output_str_met
    )
    export_output(
      x = table_structures_names,
      file = output_str_nam
    )
    export_output(
      x = table_structures_taxonomy_cla,
      file = output_str_tax_cla
    )
    export_output(
      x = table_structures_taxonomy_npc,
      file = output_str_tax_npc
    )

    return(
      c(
        "key" = output_key,
        "org_tax_ott" = output_org_tax_ott,
        "str_2d_3d" = output_str_2d_3d,
        "str_met" = output_str_met,
        "str_name" = output_str_nam,
        "str_tax_cla" = output_str_tax_cla,
        "str_tax_npc" = output_str_tax_npc
      )
    )
  }
