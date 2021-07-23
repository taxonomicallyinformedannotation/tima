start <- Sys.time()
language <- "r"

source(file = "R/functions/helpers.R")
log_debug(x = "sourcing files")
source(file = "paths.md")

log_debug(x = "loading libraries")
library(data.table)
library(dplyr)

log_debug(
    "This script prepares LOTUS referenced structure-organism pairs \n",
    "for further processing. \n"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug(x = "loading files")
tempValidatedSet <-
    data.table::fread(
        file = data_source_databases_lotus,
        sep = ","
    )

structureOrganismPairsTable <- tempValidatedSet |>
    dplyr::mutate(structure_inchikey_2D = substring(
        text = structure_inchikey,
        first = 1,
        last = 14
    )) |>
    dplyr::select(
        # structure_name,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        # structure_xlogp,
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
        # reference_title
    ) |>
    dplyr::distinct()

log_debug("ensuring directories exist ...")
ifelse(
    test = !dir.exists(data_interim),
    yes = dir.create(data_interim),
    no = paste(data_interim, "exists")
)
data.table::fwrite(
    x = structureOrganismPairsTable,
    file = data_interim_lotus_prepared,
    sep = "\t"
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
