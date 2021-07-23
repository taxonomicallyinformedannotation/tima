start <- Sys.time()
language <- "r"

source(file = "R/functions/helpers.R")
log_debug(x = "sourcing files")
source(file = "paths.md")

doc <- readChar(con = docopt_prepare_dnp,
                nchars = file.info(docopt_prepare_dnp)$size)

arguments <- docopt(doc, version = "beta")

log_debug(x = "loading libraries")
library(data.table)
library(dplyr)
library(purrr)
library(yaml)

log_debug(
    "This script prepares DNP referenced structure-organism pairs \n",
    "for further processing. \n"
)

log_debug("Authors: AR")
log_debug("Contributors: ...")

params <-
    yaml::read_yaml(file = config_default_dnp, handlers = list(
        seq = function(x) {
            purrr::flatten(x)
        }
    ))
params <-
    yaml::read_yaml(file = config_params_dnp, handlers = list(
        seq = function(x) {
            purrr::flatten(x)
        }
    ))

log_debug("checking command line arguments")
if (exists("arguments")) {
    if (!is.null(arguments$input)) {
        params$file$input <- arguments$input
    }
}
if (file.exists(params$file$input)) {
    log_debug(x = "loading files")
    tempValidatedSet <-
        data.table::fread(file = params$file$input)
    
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
            # reference_title
        ) |>
        dplyr::distinct() |>
        dplyr::mutate(reference_doi = NA)
    
    log_debug("ensuring directories exist ...")
    ifelse(
        test = !dir.exists(data_interim),
        yes = dir.create(data_interim),
        no = paste(data_interim, "exists")
    )
    data.table::fwrite(x = structureOrganismPairsTable,
                       file = data_interim_dnp_prepared,
                       sep = "\t")
} else{
    log_debug("Sorry, you do not have access to the DNP")
}

end <- Sys.time()

log_debug("Script finished in", format(end - start))
