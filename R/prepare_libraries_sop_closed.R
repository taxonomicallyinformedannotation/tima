#' @title Prepare libraries of structure organism pairs CLOSED
#'
#' @param input Input file
#' @param output Output file
#' @param parameters params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_sop_closed <-
  function(input = params$files$libraries$sop$raw$closed,
           output = params$files$libraries$sop$prepared,
           parameters = params) {
    params <<- parameters
    if (file.exists(input)) {
      log_debug(x = "Loading closed resources")
      closed <-
        readr::read_delim(file = input)

      log_debug(x = "Formatting closed resource")
      closed_prepared <- closed |>
        dplyr::mutate(structure_inchikey_2D = substring(
          text = structure_inchikey,
          first = 1,
          last = 14
        )) |>
        dplyr::select(
          structure_name = structure_nameTraditional,
          structure_inchikey,
          structure_smiles,
          structure_inchikey_2D,
          structure_smiles_2D,
          structure_molecular_formula,
          structure_exact_mass,
          structure_xlogp,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          structure_taxonomy_classyfire_chemontid,
          structure_taxonomy_classyfire_01kingdom,
          structure_taxonomy_classyfire_02superclass,
          structure_taxonomy_classyfire_03class,
          structure_taxonomy_classyfire_04directparent,
          organism_name,
          organism_taxonomy_ottid,
          organism_taxonomy_01domain,
          organism_taxonomy_02kingdom,
          organism_taxonomy_03phylum,
          organism_taxonomy_04class,
          organism_taxonomy_05order,
          organism_taxonomy_06family,
          organism_taxonomy_07tribe,
          organism_taxonomy_08genus,
          organism_taxonomy_09species,
          organism_taxonomy_10varietas
        ) |>
        round_reals() |>
        dplyr::distinct() |>
        dplyr::mutate(reference_doi = NA)
    } else {
      log_debug("Sorry, you do not have access to the closed resource, returning an empty file instead")
      closed_prepared <- data.frame(
        structure_name = NA,
        structure_inchikey = NA,
        structure_smiles = NA,
        structure_inchikey_2D = NA,
        structure_smiles_2D = NA,
        structure_molecular_formula = NA,
        structure_exact_mass = NA,
        structure_xlogp = NA,
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
        structure_taxonomy_classyfire_chemontid = NA,
        structure_taxonomy_classyfire_01kingdom = NA,
        structure_taxonomy_classyfire_02superclass = NA,
        structure_taxonomy_classyfire_03class = NA,
        structure_taxonomy_classyfire_04directparent = NA,
        organism_name = NA,
        organism_taxonomy_ottid = NA,
        organism_taxonomy_01domain = NA,
        organism_taxonomy_02kingdom = NA,
        organism_taxonomy_03phylum = NA,
        organism_taxonomy_04class = NA,
        organism_taxonomy_05order = NA,
        organism_taxonomy_06family = NA,
        organism_taxonomy_07tribe = NA,
        organism_taxonomy_08genus = NA,
        organism_taxonomy_09species = NA,
        organism_taxonomy_10varietas = NA,
        reference_doi = NA
      )
    }

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_libraries_sop_closed")
    export_output(x = closed_prepared, file = output)
    return(output)
  }
