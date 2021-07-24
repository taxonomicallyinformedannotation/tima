start <- Sys.time()
language <- "r"

source(file = "R/functions/helpers.R")
log_debug(x = "sourcing files")
source(file = "paths.md")

log_debug(x = "loading libraries")
library(crayon)
library(data.table)
library(dplyr)
library(docopt)
library(splitstackshape)
library(stringr)
library(tidyr)
library(yaml)

readChar(
  con = docopt_annotate_features,
  nchars = file.info(docopt_annotate_features)$size
) -> doc

arguments <- docopt(doc, version = "Taxo_scorer alpha 0.01")

log_debug(
  "This script performs",
  green("taxonomically informed scoring"),
  "preceded by",
  blue("MS1 annotation"),
  "and folowed by",
  yellow("chemical consistency informed scoring")
)
log_debug("Authors: \n", green("AR, P-MA"))
log_debug("Contributors: \n", "...")

params <-
  yaml::read_yaml(file = config_default_annotate, handlers = list(
    seq = function(x) {
      purrr::flatten(x)
    }
  ))
params <-
  yaml::read_yaml(file = config_params_annotate, handlers = list(
    seq = function(x) {
      purrr::flatten(x)
    }
  ))
log_debug("checking command line arguments", "\n")
if (exists("arguments")) {
  if (!is.null(arguments$library)) {
    params$file$library <- arguments$library
  }
  if (!is.null(arguments$annotation.file)) {
    params$file$annotation$interim <- arguments$annotation.file
  }
  if (!is.null(arguments$features.file)) {
    params$file$features$taxed <- arguments$features.file
  }
  if (!is.null(arguments$edges.file)) {
    params$file$edges$interim <- arguments$edges.file
  }
  if (!is.null(arguments$directory)) {
    params$file$annotation$directory <-
      file.path(params$file$annotation$directory, arguments$directory)
  } else {
    params$file$annotation$directory <-
      file.path(
        params$file$annotation$directory,
        format(Sys.time(), "%y%m%d_%H%M%OS")
      )
  }
  if (!is.null(arguments$output)) {
    params$file$annotation$weighted <- arguments$output
  }
  if (!is.null(arguments$mode.ms)) {
    params$ms$mode <- arguments$mode.ms
  }
  if (!is.null(arguments$complement)) {
    params$ms$annotate <- arguments$complement
  }
  if (!is.null(arguments$ppm.tol)) {
    params$ms$tolerance$ppm <- as.numeric(arguments$ppm.tol)
  }
  if (!is.null(arguments$rt.tol)) {
    params$rt$tolerance$min <- as.numeric(arguments$rt.tol)
  }
  if (!is.null(arguments$force)) {
    params$force <- TRUE
  }
}
stopifnot(
  "Your --ms.mode parameter (in command line arguments or in 'annotate_params.yaml' must be 'pos' or 'neg'" = params$ms$mode %in% c("pos", "neg")
)
stopifnot(
  "Your --ms.level parameter (in command line arguments or in 'annotate_params.yaml' must be 1" = params$ms$level == 1
)
stopifnot(
  "Your --ms.annotate parameter (in command line arguments or in 'annotate_params.yaml' must be TRUE or FALSE" = params$ms$annotate %in% c(TRUE, FALSE)
)
if (params$force == FALSE) {
  stopifnot(
    "Your --ms.tolreance.ppm parameter (in command line arguments or in 'annotate_params.yaml' must be lower or equal to 20" = params$ms$tolerance$ppm <=
      20
  )
  stopifnot(
    "Your --rt.tolreance.ppm parameter (in command line arguments or in 'annotate_params.yaml' must be lower or equal to 0.1" = params$rt$tolerance$min <=
      0.1
  )
}
## for later on
# stopifnot(
#  adducts
# )

log_debug(x = "... functions")
source(file = "R/biological_weighting.R")
source(file = "R/chemical.R")
source(file = "R/cleaning.R")
source(file = "R//decoration.R")
source(file = "R/dist_groups.R")
source(file = "R/features.R")
source(file = "R/ms1.R")

log_debug(x = "... files ...")
log_debug(x = "... metadata_table_spectral_annotation")
metadata_table_spectral_annotation <- data.table::fread(
  file = params$file$annotation$interim,
  sep = "\t",
  colClasses = "character"
) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

log_debug(x = "... metadata_table_biological_annotation")
taxed_features_table <- data.table::fread(
  file = params$file$features$taxed,
  sep = "\t",
  colClasses = "character"
) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

taxed_features_table[is.na(taxed_features_table)] <- "ND"

log_debug(x = "... edges table")
edges_table <- data.table::fread(
  file = params$file$edges$interim,
  sep = "\t"
)

log_debug(x = "... structure-organism pairs table")
structure_organism_pairs_table <-
  data.table::fread(
    file = params$file$library,
    colClasses = "character"
  ) |>
  dplyr::filter(!is.na(structure_exact_mass)) |>
  dplyr::mutate(dplyr::across(structure_exact_mass, as.numeric))

structure_organism_pairs_table[is.na(structure_organism_pairs_table)] <-
  "notClassified"

## for MS1 only
# metadata_table_spectral_annotation <-
#   metadata_table_spectral_annotation |>
#   dplyr::mutate(
#     inchikey_2D = NA,
#     score_input = NA,
#     library = NA,
#     mz_error = NA
#   ) |>
#   dplyr::mutate(across(c(
#     inchikey, score_input, library_name, mz_error
#   ),
#   as.character))

if (params$ms$annotate == TRUE) {
  log_debug("... single charge adducts table")
  if (params$ms$mode == "pos") {
    adduct_file <- data_interim_adducts_pos
  }
  if (params$ms$mode == "neg") {
    adduct_file <- data_interim_adducts_neg
  }

  adductsTable <- data.table::fread(
    file = adduct_file,
    sep = "\t"
  )

  log_debug("... adducts masses for in source dimers and multicharged")
  adductsMassTable <-
    data.table::fread(
      file = data_source_adducts_file,
      sep = "\t"
    )

  log_debug("... neutral lossses")
  neutral_losses_table <-
    data.table::fread(
      file = data_source_neutral_losses_file,
      sep = "\t"
    )

  adductsM <- adductsMassTable$mass
  names(adductsM) <- adductsMassTable$adduct

  if (params$ms$mode == "pos") {
    adduct_db_file <- data_interim_adducts_db_pos
  }
  if (params$ms$mode == "neg") {
    adduct_db_file <- data_interim_adducts_db_neg
  }

  log_debug(x = "... exact masses for MS1 annotation")
  structure_exact_mass_table <-
    data.table::fread(
      file = adduct_db_file,
      sep = "\t"
    ) |>
    dplyr::filter(exact_mass %in% structure_organism_pairs_table[["structure_exact_mass"]])

  log_debug(x = "performing MS1 annotation")
  annotation_table_ms1 <-
    ms1_annotation(
      annotationTable = metadata_table_spectral_annotation,
      structureExactMassTable = structure_exact_mass_table,
      structureOrganismPairsTable = structure_organism_pairs_table,
      adducts = unlist(params$ms$adducts[[params$ms$mode]]),
      neutralLosses = neutral_losses_table
    )

  ms1_decoration()
}

if (params$ms$annotate == FALSE) {
  annotation_table_ms1 <-
    non_ms1_annotation(annotationTable = metadata_table_spectral_annotation)
}

log_debug(x = "adding biological organism metadata")
annotation_table_ms1_taxed <-
  dplyr::left_join(annotation_table_ms1, taxed_features_table) |>
  dplyr::left_join(
    metadata_table_spectral_annotation |> distinct(
      inchikey_2D,
      smiles_2D,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class
    )
  )

log_debug(x = "performing taxonomically informed scoring")
annotation_table_weighted_bio <-
  biological_weighting(
    annotationTable = annotation_table_ms1_taxed,
    structureOrganismPairsTable = structure_organism_pairs_table
  )

taxo_decoration()

log_debug(x = "cleaning taxonomically informed results and preparing for chemically informed scoring")
annotation_table_weighted_bio_cleaned <-
  biological_cleaning(
    annotationTableWeightedBio = annotation_table_weighted_bio,
    structureOrganismPairsTable = structure_organism_pairs_table,
    edgesTable = edges_table
  )

log_debug(x = "performing chemically informed scoring")
annotation_table_weighted_chemo <-
  chemical_weighting(annotationTableWeightedBioCleaned = annotation_table_weighted_bio_cleaned)

chemical_decoration()

log_debug(x = "cleaning for cytoscape export")
results2cytoscape <-
  chemical_cleaning(
    annotationTableWeightedChemo = annotation_table_weighted_chemo,
    structureOrganismPairsTable = structure_organism_pairs_table
  )

log_debug(x = "exporting results and parameters used ...")
log_debug("ensuring directories exist ...")
ifelse(
  test = !dir.exists(data_processed),
  yes = dir.create(data_processed),
  no = paste(data_processed, "exists")
)
ifelse(
  test = !dir.exists(params$file$annotation$directory),
  yes = dir.create(params$file$annotation$directory),
  no = paste(params$file$annotation$directory, "exists")
)
log_debug(x = "... results are saved in", crayon::green(params$file$annotation$directory))
data.table::fwrite(
  x = results2cytoscape,
  file = file.path(
    params$file$annotation$directory,
    params$file$annotation$weighted
  ),
  sep = "\t"
)

log_debug(x = "... parameters used are saved in", crayon::green(params$file$annotation$directory))
yaml::write_yaml(
  x = params,
  file = file.path(params$file$annotation$directory, "annotate_params.yaml")
)

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
