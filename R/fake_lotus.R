#' @title Fake LOTUS
#'
#' @description This function creates a minimal fake LOTUS (natural products)
#'     database file when the real download fails. Creates an empty TSV with
#'     proper column structure to prevent pipeline failures during testing or
#'     when external resources are unavailable.
#'
#' @include create_dir.R
#'
#' @param export Character string path where the fake LOTUS TSV file should be saved
#'
#' @return Character string path to the created fake file
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fake_lotus(export = "data/source/lotus.tsv.gz")
#' }
fake_lotus <- function(export) {
  # Validate input
  if (missing(export) || !is.character(export) || length(export) != 1L) {
    stop("export path must be a single character string")
  }

  log_warn(
    "LOTUS download failed. Creating empty placeholder file with proper structure."
  )

  # Create empty data frame with all expected LOTUS columns
  fake_data <- tidytable::tidytable(
    structure_wikidata = character(0),
    structure_inchikey = character(0),
    structure_inchi = character(0),
    structure_smiles = character(0),
    structure_molecular_formula = character(0),
    structure_exact_mass = numeric(0),
    structure_xlogp = numeric(0),
    structure_smiles_2D = character(0),
    structure_cid = character(0),
    structure_nameIupac = character(0),
    structure_nameTraditional = character(0),
    structure_stereocenters_total = integer(0),
    structure_stereocenters_unspecified = integer(0),
    structure_taxonomy_npclassifier_01pathway = character(0),
    structure_taxonomy_npclassifier_02superclass = character(0),
    structure_taxonomy_npclassifier_03class = character(0),
    structure_taxonomy_classyfire_chemontid = character(0),
    structure_taxonomy_classyfire_01kingdom = character(0),
    structure_taxonomy_classyfire_02superclass = character(0),
    structure_taxonomy_classyfire_03class = character(0),
    structure_taxonomy_classyfire_04directparent = character(0),
    organism_wikidata = character(0),
    organism_name = character(0),
    organism_taxonomy_gbifid = character(0),
    organism_taxonomy_ncbiid = character(0),
    organism_taxonomy_ottid = character(0),
    organism_taxonomy_01domain = character(0),
    organism_taxonomy_02kingdom = character(0),
    organism_taxonomy_03phylum = character(0),
    organism_taxonomy_04class = character(0),
    organism_taxonomy_05order = character(0),
    organism_taxonomy_06family = character(0),
    organism_taxonomy_07tribe = character(0),
    organism_taxonomy_08genus = character(0),
    organism_taxonomy_09species = character(0),
    organism_taxonomy_10varietas = character(0),
    reference_wikidata = character(0),
    reference_doi = character(0),
    manual_validation = logical(0)
  )

  # Write to file
  create_dir(export = export)
  tidytable::fwrite(x = fake_data, file = export)

  log_debug("Created fake LOTUS file at: {export}")
  return(export)
}
