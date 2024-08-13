import::from(tidytable, fwrite, .into = environment())
import::from(tidytable, tidytable, .into = environment())

#' @title Fake LOTUS
#'
#' @description This function fakes LOTUS in case the download failed
#'
#' @importFrom tidytable fwrite
#' @importFrom tidytable tidytable
#'
#' @param export Path to save the file to
#'
#' @return NULL
#'
#' @examples NULL
fake_lotus <- function(export) {
  log_debug("External failure. Returning empty file instead.")
  tidytable(
    structure_wikidata = NA,
    structure_inchikey = NA,
    structure_inchi = NA,
    structure_smiles = NA,
    structure_molecular_formula = NA,
    structure_exact_mass = NA,
    structure_xlogp = NA,
    structure_smiles_2D = NA,
    structure_cid = NA,
    structure_nameIupac = NA,
    structure_nameTraditional = NA,
    structure_stereocenters_total = NA,
    structure_stereocenters_unspecified = NA,
    structure_taxonomy_npclassifier_01pathway = NA,
    structure_taxonomy_npclassifier_02superclass = NA,
    structure_taxonomy_npclassifier_03class = NA,
    structure_taxonomy_classyfire_chemontid = NA,
    structure_taxonomy_classyfire_01kingdom = NA,
    structure_taxonomy_classyfire_02superclass = NA,
    structure_taxonomy_classyfire_03class = NA,
    structure_taxonomy_classyfire_04directparent = NA,
    organism_wikidata = NA,
    organism_name = NA,
    organism_taxonomy_gbifid = NA,
    organism_taxonomy_ncbiid = NA,
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
    reference_wikidata = NA,
    reference_doi = NA,
    manual_validation = NA
  ) |>
    fwrite(export)
  return(export)
}
