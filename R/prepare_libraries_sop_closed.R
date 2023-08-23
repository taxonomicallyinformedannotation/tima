utils::globalVariables(
  c(
    "organism_name",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "organism_taxonomy_04class",
    "organism_taxonomy_05order",
    "organism_taxonomy_06family",
    "organism_taxonomy_07tribe",
    "organism_taxonomy_08genus",
    "organism_taxonomy_09species",
    "organism_taxonomy_10varietas",
    "organism_taxonomy_ottid",
    "params",
    "structure_exact_mass",
    "structure_inchikey",
    "structure_inchikey_2D",
    "structure_molecular_formula",
    "structure_nameTraditional",
    "structure_smiles",
    "structure_smiles_2D",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_xlogp"
  )
)

#' @title Prepare libraries of structure organism pairs CLOSED
#'
#' @include export_output.R
#' @include export_params.R
#' @include round_reals.R
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
      closed <- input |>
        tidytable::fread(
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      log_debug(x = "Formatting closed resource")
      closed_prepared <- closed |>
        tidytable::mutate(
          structure_inchikey_2D = stringi::stri_sub(
            str = structure_inchikey,
            from = 1,
            to = 14
          )
        ) |>
        tidytable::rename(structure_name = structure_nameTraditional) |>
        tidytable::mutate(reference_doi = NA) |>
        select_sop_columns() |>
        round_reals() |>
        tidytable::distinct()
    } else {
      log_debug("Sorry, you do not have access to the closed resource,
                returning an empty file instead")
      closed_prepared <- fake_sop_columns()
    }

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_libraries_sop_closed")
    export_output(x = closed_prepared, file = output)
    return(output)
  }
