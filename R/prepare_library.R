#' @title Prepare LOTUS
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr distinct mutate select
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
prepare_lotus <-
  function(input = paths$data$source$libraries$lotus,
           output = paths$data$interim$libraries$lotus) {
    log_debug(x = "Loading and preparing LOTUS")
    lotus_prepared <- input |>
      readr::read_csv() |>
      dplyr::mutate(structure_inchikey_2D = substring(
        text = structure_inchikey,
        first = 1,
        last = 14
      )) |>
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
      dplyr::distinct()

    log_debug(x = "Exporting ...")
    export_output(x = lotus_prepared, file = output)
  }
