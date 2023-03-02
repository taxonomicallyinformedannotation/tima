#' @title Prepare merged structure organism pairs libraries
#'
#' @description This function prepares the libraries made of all sub-libraries containing structure-organism pairs
#'
#' @details It can be restricted to specific taxa to have more biologically meaningful annotation.
#'
#' @param files List of libraries to be merged
#' @param filter Boolean. TRUE or FALSE if you want to filter the library
#' @param level Biological rank to be filtered. Kingdom, phylum, family, genus, ...
#' @param value Name of the taxon or taxa to be kept, e.g. 'Gentianaceae|Apocynaceae'
#' @param output_key Output file for keys
#' @param output_org_nam Output file for organisms names
#' @param output_org_tax_ott Output file for organisms taxonomy (OTT)
#' @param output_str_2D_3D Output file for structures (2D + 3D)
#' @param output_str_met Output file for structures metadata
#' @param output_str_nam Output file for structures names
#' @param output_str_tax_cla Output file for structures taxonomy (Classyifre)
#' @param output_str_tax_npc Output file for structures taxonomy (NPC)
#' @param parameters Param
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr anti_join bind_rows distinct filter left_join mutate_all
#' @importFrom readr cols read_delim write_delim
#'
#' @examples NULL
prepare_libraries_sop_merged <-
  function(files = params$files$libraries$sop$processed,
           filter = params$organisms$filter$mode,
           level = params$organisms$filter$level,
           value = params$organisms$filter$value,
           output_key = paths$data$interim$libraries$merged$keys,
           output_org_nam = paths$data$interim$libraries$merged$organisms$names,
           output_org_tax_ott = paths$data$interim$libraries$merged$organisms$taxonomies$ott,
           output_str_2D_3D = paths$data$interim$libraries$merged$structures$dd_ddd,
           output_str_met = paths$data$interim$libraries$merged$structures$metadata,
           output_str_nam = paths$data$interim$libraries$merged$structures$names,
           output_str_tax_cla = paths$data$interim$libraries$merged$structures$taxonomies$classyfire,
           output_str_tax_npc = paths$data$interim$libraries$merged$structures$taxonomies$npc,
           parameters = params) {
    # Check if the filter parameter is valid
    stopifnot("Your filter parameter must be 'true' or 'false'" = filter %in% c(TRUE, FALSE))

    # If filter is TRUE, check if the level parameter is valid
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

    # Load and concatenate the prepared libraries
    log_debug(x = "Loading and concatenating prepared libraries")
    params <<- parameters
    libraries <- list()
    for (i in seq_along(files)) {
      libraries[[i]] <- readr::read_delim(
        file = files[[i]],
        col_types = readr::cols(.default = "c")
      )
    }

    tables <- dplyr::bind_rows(libraries) |>
      split_tables_sop()

    log_debug(x = "Keeping keys")
    table_keys <- tables$key

    log_debug(x = "Keeping organisms")
    table_organisms_names <- tables$org_nam
    table_organisms_taxonomy_ott <- tables$org_tax_ott

    log_debug(x = "Completing organisms taxonomy")
    table_organisms_taxonomy_ott_2 <- table_organisms_names |>
      dplyr::anti_join(table_organisms_taxonomy_ott) |>
      dplyr::distinct(organism = organism_name)

    if (nrow(table_organisms_taxonomy_ott_2) != 0) {
      table_organisms_taxonomy_ott_full <-
        table_organisms_taxonomy_ott_2 |>
        get_organism_taxonomy_ott()

      table_organisms_taxonomy_ott <-
        table_organisms_taxonomy_ott |>
        dplyr::bind_rows(table_organisms_taxonomy_ott_full |>
          dplyr::mutate_all(as.character))
    }

    log_debug(x = "Keeping structures")
    table_structures_2D_3D <- tables$str_2D_3D
    table_structures_metadata <- tables$str_met
    table_structures_names <- tables$str_nam
    table_structures_taxonomy_classyfire <- tables$str_tax_cla
    table_structures_taxonomy_npc <- tables$str_tax_npc

    log_debug(x = "Completing structures metadata")
    log_debug(x = "TODO")

    log_debug(x = "Completing structures names")
    log_debug(x = "TODO")

    log_debug(x = "Completing structures taxonomy (classyfire)")
    log_debug(x = "TODO")

    log_debug(x = "Completing structures taxonomy (NPC)")
    log_debug(x = "TODO")

    # If filter is TRUE, filter the library based on the specified level and value
    if (filter == TRUE) {
      log_debug(x = "Filtering library")
      table_keys <- table_keys |>
        dplyr::left_join(table_organisms_taxonomy_ott)

      table_keys <- table_keys |>
        dplyr::filter(grepl(
          x = !!as.name(colnames(table_keys)[grepl(
            pattern = level,
            x = colnames(table_keys)
          )]),
          pattern = value
        )) |>
        dplyr::select(
          structure_inchikey,
          structure_smiles,
          organism_name,
          reference_doi
        ) |>
        dplyr::distinct()

      stopifnot("Your filter led to no entries, try to change it." = nrow(table_keys) != 0)
    }

    # Export the library
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_libraries")
    export_output(
      x = table_keys,
      file = output_key
    )
    export_output(
      x = table_organisms_names,
      file = output_org_nam
    )
    export_output(
      x = table_organisms_taxonomy_ott,
      file = output_org_tax_ott
    )
    export_output(
      x = table_structures_2D_3D,
      file = output_str_2D_3D
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
      x = table_structures_taxonomy_classyfire,
      file = output_str_tax_cla
    )
    export_output(
      x = table_structures_taxonomy_npc,
      file = output_str_tax_npc
    )

    return(
      c(
        "key" = output_key,
        "org_nam" = output_org_nam,
        "org_tax_ott" = output_org_tax_ott,
        "str_2D_3D" = output_str_2D_3D,
        "str_met" = output_str_met,
        "str_name" = output_str_nam,
        "str_tax_cla" = output_str_tax_cla,
        "str_tax_npc" = output_str_tax_npc
      )
    )
  }
