#' @title Prepare LOTUS
#'
#' @description This function prepares the LOTUS structure-organism pairs
#'
#' @param input Input file
#' @param output Output file
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr distinct mutate select
#' @importFrom readr read_csv
#'
#' @examples NULL
prepare_lotus <-
  function(input = paths$data$source$libraries$lotus,
           output = paths$data$interim$libraries$lotus) {
    log_debug(x = "Loading and preparing LOTUS")
    # Read input file and select specific columns
    lotus_prepared <- input |>
      readr::read_csv(
        col_select = c(
          structure_nameTraditional,
          structure_inchikey,
          structure_smiles_2D,
          structure_molecular_formula,
          structure_exact_mass,
          structure_xlogp,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          organism_name,
          organism_taxonomy_01domain,
          organism_taxonomy_02kingdom,
          organism_taxonomy_03phylum,
          organism_taxonomy_04class,
          organism_taxonomy_05order,
          organism_taxonomy_06family,
          organism_taxonomy_07tribe,
          organism_taxonomy_08genus,
          organism_taxonomy_09species,
          organism_taxonomy_10varietas,
          reference_doi
        )
      ) |>
      # Create new column by extracting substring from structure_inchikey column
      dplyr::mutate(structure_inchikey_2D = substring(
        text = structure_inchikey,
        first = 1,
        last = 14
      )) |>
      # Select specific columns
      dplyr::select(
        structure_nameTraditional,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class,
        organism_name,
        organism_taxonomy_01domain,
        organism_taxonomy_02kingdom,
        organism_taxonomy_03phylum,
        organism_taxonomy_04class,
        organism_taxonomy_05order,
        organism_taxonomy_06family,
        organism_taxonomy_07tribe,
        organism_taxonomy_08genus,
        organism_taxonomy_09species,
        organism_taxonomy_10varietas,
        reference_doi
      ) |>
      # Keep only unique rows
      dplyr::distinct()

    log_debug(x = "Exporting ...")
    # Write modified data frame to output file
    export_output(x = lotus_prepared, file = output)
  }
