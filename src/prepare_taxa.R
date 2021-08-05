start <- Sys.time()

source(file = "src/R/clean_gnverifier.R")
source(file = "src/R/get_gnps.R")
source(file = "src/R/helpers.R")
source(file = "src/R/manipulating_taxo_otl.R")

log_debug("This script informs taxonomically features")
log_debug("Authors: AR, P-MA")
log_debug("Contributors: ...")

log_debug("Loading packages")
library(docopt)
library(dplyr)
library(jsonlite)
library(purrr)
library(readr)
library(splitstackshape)
library(tidyr)
library(yaml)

step <- "prepare_taxa"
paths <- parse_yaml_paths()
params <- get_params(step = step)

stopifnot(
  "Your --tool.metadata parameter (in command line arguments or in 'inform_params.yaml' must be either 'gnps' or 'manual'" = params$tool %in% c("gnps", "manual")
)
stopifnot(
  "Your --top_k.organism_per_feature parameter (in command line arguments or in 'inform_params.yaml' should be lower or equal to 5" = params$top_k <=
    5
)

log_debug(x = "Loading taxa ranks dictionary")
taxa_ranks_dictionary <-
  readr::read_delim(file = paths$data$source$dictionaries$ranks)

if (params$tool == "gnps") {
  log_debug(x = "Loading feature table")
  feature_table <- read_features(id = params$gnps)
  
  if (!is.null(params$force)) {
    log_debug(x = "Forcing all features to given organism")
    metadata_table <- data.frame(params$force)
    colnames(metadata_table) <- params$column_name
  } else{
    log_debug(x = "Loading metadata table")
    metadata_table <- read_metadata(id = params$gnps)
    
    log_debug(x = "Formatting feature table ...")
    log_debug(x = "... WARNING: requires 'Peak area' in columns (MZmine format)")
    feature_table <- feature_table %>%
      dplyr::select(`row ID`,
                    matches(" Peak area")) |>
      tibble::column_to_rownames(var = "row ID")
    colnames(feature_table) <-
      gsub(
        pattern = " Peak area",
        replacement = "",
        x = colnames(feature_table)
      )
    log_debug(x = "... filtering top K intensities per feature")
    top_n <- feature_table |>
      tibble::rownames_to_column() |>
      tidyr::gather(column, value, -rowname) |>
      dplyr::filter(value != 0) |>
      dplyr::group_by(rowname) |>
      dplyr::mutate(rank = rank(-value)) |>
      dplyr::filter(rank <= params$top_k) |>
      dplyr::arrange(rowname, rank)
  }
}
}

if (params$tool == "manual") {
  metadata_table <-
    readr::read_delim(file = params$input)
}

log_debug(x = "Keeping list of organisms to submit to GNVerifier")
organism_table <- metadata_table |>
  dplyr::filter(!is.na(dplyr::all_of(params$column_name))) |>
  dplyr::distinct(dplyr::across(dplyr::all_of(params$column_name))) |>
  dplyr::select(organism = all_of(params$column_name)) |>
  splitstackshape::cSplit(splitCols = "organism",
                          sep = "|",
                          direction = "long") |>
  dplyr::mutate(organism = gsub(
    pattern = " x ",
    replacement = " ",
    x = organism
  )) |>
  dplyr::distinct()

log_debug(x = "Exporting organisms for GNVerifier submission")
ifelse(
  test = !dir.exists(paths$data$interim$path),
  yes = dir.create(paths$data$interim$path),
  no = paste(paths$data$interim$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$taxa$path),
  yes = dir.create(paths$data$interim$taxa$path),
  no = paste(paths$data$interim$taxa$path, "exists")
)
readr::write_delim(
  x = organism_table,
  file = paths$data$interim$taxa$original,
  quote = "none"
)

log_debug("submitting to GNVerifier")
system(command = paste("bash", paths$src$gnverifier))

log_debug("cleaning GNVerifier results")
dataOrganismVerified_3 <- clean_gnverifier()

log_debug("Formatting obtained OTL taxonomy")
organism_cleaned_manipulated <-
  manipulating_taxo_otl(dfsel = dataOrganismVerified_3)

if (is.null(params$force) &
    params$extension == FALSE) {
  log_debug("Removing filename extensions")
  metadata_table <- metadata_table |>
    mutate(filename = gsub(
      pattern = ".mzML",
      replacement = "",
      x = filename,
      fixed = TRUE
    )) |>
    mutate(filename = gsub(
      pattern = ".mzxML",
      replacement = "",
      x = filename,
      fixed = TRUE
    ))
}
log_debug(x = "Joining top K with metadata table")
if (params$tool == "gnps") {
  if (!is.null(params$force)) {
    metadata_table_joined <- cbind(
      feature_table |> mutate(feature_id = `row ID`),
      dataOrganismVerified_3 |> select(organismOriginal = organismCleaned)
    )
  } else{
    metadata_table_joined <-
      dplyr::left_join(top_n, metadata_table, by = c("column" = "filename")) |>
      dplyr::select(
        feature_id := rowname,
        organismOriginal = dplyr::all_of(params$column_name),
        dplyr::everything()
      )
  }
}

if (params$tool == "manual") {
  metadata_table_joined <- metadata_table |>
    dplyr::select(feature_id,
                  organismOriginal = dplyr::all_of(params$column_name),
                  dplyr::everything())
}

log_debug(x = "Joining with cleaned taxonomy table")
metadata_table_joined_summarized <-
  dplyr::left_join(
    metadata_table_joined,
    organism_cleaned_manipulated,
    by = c("organismOriginal" = "organismCleaned")
  ) |>
  dplyr::distinct() %>%
  dplyr::mutate_at(vars(-dplyr::group_cols()), as.character) |>
  dplyr::select(
    feature_id,
    sample_organism_01_domain = organism_01_domain,
    sample_organism_02_kingdom = organism_02_kingdom,
    sample_organism_03_phylum = organism_03_phylum,
    sample_organism_04_class = organism_04_class,
    sample_organism_05_order = organism_05_order,
    # sample_organism_05_1_infraorder = organism_05_1_infraorder,
    sample_organism_06_family = organism_06_family,
    # sample_organism_06_1_subfamily = organism_06_1_subfamily,
    sample_organism_07_tribe = organism_07_tribe,
    # sample_organism_07_1_subtribe = organism_07_1_subtribe,
    sample_organism_08_genus = organism_08_genus,
    # sample_organism_08_1_subgenus = organism_08_1_subgenus,
    sample_organism_09_species = organism_09_species,
    # sample_organism_09_1_subspecies = organism_09_1_subspecies,
    sample_organism_10_varietas = organism_10_variety
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::summarise_all(function(x) {
    x <- list(paste(unique(x[!is.na(x)]), collapse = "|"))
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate_all(as.character)

log_debug(x = "joining with cleaned taxonomy table")
metadata_table_joined_summarized[] <-
  lapply(metadata_table_joined_summarized,
         function(x) {
           y_as_na(x, y = "")
         })

log_debug(x = "Exporting ...")
ifelse(
  test = !dir.exists(paths$data$path),
  yes = dir.create(paths$data$path),
  no = paste(paths$data$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$path),
  yes = dir.create(paths$data$interim$path),
  no = paste(paths$data$interim$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$config$path),
  yes = dir.create(paths$data$interim$config$path),
  no = paste(paths$data$interim$config$path, "exists")
)

log_debug(x = "... path to export is",
          params$output)
readr::write_delim(x = metadata_table_joined_summarized,
                   file = params$output,)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
