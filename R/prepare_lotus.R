#' Title
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_lotus <-
  function(input = paths$data$source$libraries$lotus,
           output = paths$data$interim$libraries$lotus) {
    log_debug(x = "Loading files")
    lotus <-
      readr::read_delim(file = input)

    lotus_prepared <- lotus |>
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
    ifelse(
      test = !dir.exists(dirname(dirname(dirname(output)))),
      yes = dir.create(dirname(dirname(dirname(output)))),
      no = paste(dirname(dirname(dirname(output)), "exists"))
    )
    ifelse(
      test = !dir.exists(dirname(dirname(output))),
      yes = dir.create(dirname(dirname(output))),
      no = paste(dirname(dirname(output)), "exists")
    )
    ifelse(
      test = !dir.exists(dirname(output)),
      yes = dir.create(dirname(output)),
      no = paste(dirname(output), "exists")
    )

    readr::write_delim(
      x = lotus_prepared,
      file = output,
      delim = "\t"
    )
  }
