start <- Sys.time()

source(file = "R/biological_weighting.R")
source(file = "R/chemical_weighting.R")
source(file = "R/cleaning.R")
source(file = "R/decoration.R")
source(file = "R/dist_groups.R")
source(file = "R/helpers.R")
source(file = "R/ms1.R")

library(crayon)
library(data.table)
library(dplyr)
library(docopt)
library(readr)
library(splitstackshape)
library(stringr)
library(tidyr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "process_annotations")

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
    "Your --rt.tolreance.ppm parameter (in command line arguments or in 'annotate_params.yaml' must be lower or equal to 0.1" = params$rt$tolerance$rt <=
      0.1
  )
}
## for later on
# stopifnot(
#  adducts
# )

log_debug(x = "... files ...")
log_debug(x = "... metadata_table_spectral_annotation")
metadata_table_spectral_annotation <-
  readr::read_delim(
    file = params$annotation,
    col_types = "c"
  ) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

log_debug(x = "... metadata_table_biological_annotation")
taxed_features_table <- readr::read_delim(
  file = params$taxa,
  col_types = "c"
) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric)) |>
  dplyr::mutate_if(is.logical, as.character)

taxed_features_table[is.na(taxed_features_table)] <- "ND"

log_debug(x = "... edges table")
edges_table <- readr::read_delim(file = params$edges)

log_debug(x = "... structure-organism pairs table")
structure_organism_pairs_table <-
  readr::read_delim(
    file = params$library,
    col_types = "c"
  ) |>
  dplyr::filter(!is.na(structure_exact_mass)) |>
  dplyr::mutate(dplyr::across(structure_exact_mass, as.numeric)) |>
  dplyr::mutate_if(is.logical, as.character)

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
    adduct_file <- paths$data$interim$adducts$pos
  }
  if (params$ms$mode == "neg") {
    adduct_file <- paths$data$interim$adducts$neg
  }

  adductsTable <- readr::read_delim(file = adduct_file)

  log_debug("... adducts masses for in source dimers and multicharged")
  adductsMassTable <-
    readr::read_delim(file = paths$data$source$adducts)

  log_debug("... neutral lossses")
  neutral_losses_table <-
    readr::read_delim(file = paths$data$source$neutral_losses)

  adductsM <- adductsMassTable$mass
  names(adductsM) <- adductsMassTable$adduct

  if (params$ms$mode == "pos") {
    adduct_db_file <-
      file.path(
        paths$data$interim$adducts$path,
        paste0(params$name, "_pos.tsv.gz")
      )
  }
  if (params$ms$mode == "neg") {
    adduct_db_file <-
      file.path(
        paths$data$interim$adducts$path,
        paste0(params$name, "_neg.tsv.gz")
      )
  }

  log_debug(x = "... exact masses for MS1 annotation")
  structure_exact_mass_table <-
    readr::read_delim(file = adduct_db_file) |>
    dplyr::filter(exact_mass %in% structure_organism_pairs_table[["structure_exact_mass"]])

  log_debug(x = "performing MS1 annotation")
  annotation_table_ms1 <- ms1_annotation()

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
annotation_table_weighted_bio <- biological_weighting()

taxo_decoration()

log_debug(x = "cleaning taxonomically informed results and preparing for chemically informed scoring")
annotation_table_weighted_bio_cleaned <- biological_cleaning()

log_debug(x = "performing chemically informed scoring")
annotation_table_weighted_chemo <- chemical_weighting()

chemical_decoration()

log_debug(x = "cleaning for cytoscape export")
results2cytoscape <- chemical_cleaning()

log_debug(x = "exporting results and parameters used ...")
log_debug("ensuring directories exist ...")
ifelse(
  test = !dir.exists(paths$data$processed$path),
  yes = dir.create(paths$data$processed$path),
  no = paste(paths$data$processed$path, "exists")
)

time <- format(Sys.time(), "%y%m%d_%H%M%OS")
dir_time <- file.path(paths$data$processed$path, time)

ifelse(
  test = !dir.exists(dir_time),
  yes = dir.create(dir_time),
  no = paste(
    dir_time,
    "exists"
  )
)
log_debug(
  x = "... results are saved in",
  crayon::green(dir_time)
)
readr::write_delim(
  x = results2cytoscape,
  file = file.path(
    dir_time,
    params$output
  )
)

log_debug(x = "... parameters used are saved in", crayon::green(dir_time))
yaml::write_yaml(
  x = params,
  file = file.path(
    dir_time,
    "process_annotations.yaml"
  )
)

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
