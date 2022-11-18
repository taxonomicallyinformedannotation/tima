start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

#' @title Prepare closed
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
prepare_closed <- function(input = params$input,
                           output = params$output) {
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
        organism_taxonomy_10varietas
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(reference_doi = NA)

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_closed")
    export_output(x = closed_prepared, file = output)
  } else {
    log_debug("Sorry, you do not have access to the closed resource")
  }
}


log_debug(
  "This script",
  crayon::green("prepares closed referenced structure-organism pairs \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

step <- "prepare_closed"
paths <- parse_yaml_paths()
params <- get_params(step = step)

prepare_closed()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
