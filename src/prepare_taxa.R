start <- Sys.time()
language <- "r"

source(file = "R/functions/helpers.R")
log_debug(x = "sourcing files")
source(file = "paths.md")
source(file = "R/functions/features.R")
source(file = "R/functions/biological.R")
source(file = "R/functions/manipulating_taxo_otl.R")

log_debug(x = "loading libraries")
library(data.table)
library(docopt)
library(dplyr)
library(jsonlite)
library(purrr)
library(RCurl)
library(splitstackshape)
library(stringi)
library(stringr)
library(tibble)
library(tidyr)
library(yaml)

readChar(
  con = docopt_2_inform_features_taxa,
  nchars = file.info(docopt_2_inform_features_taxa)$size
) -> doc

arguments <- docopt::docopt(doc, version = "Taxo_scorer alpha 0.01")

log_debug("This script informs taxonomically features")

log_debug("Authors: AR, P-MA")
log_debug("Contributors: ...")

params <-
  yaml::read_yaml(file = config_default_inform, handlers = list(
    seq = function(x) {
      purrr::flatten(x)
    }
  ))
params <-
  yaml::read_yaml(file = config_params_inform, handlers = list(
    seq = function(x) {
      purrr::flatten(x)
    }
  ))
log_debug("checking command line arguments")
if (exists("arguments")) {
  if (!is.null(arguments$extension)) {
    params$file$features$extension <- arguments$extension
  }
  if (!is.null(arguments$tool)) {
    params$tool$metadata <- arguments$tool
  }
  if (!is.null(arguments$input)) {
    params$file$features$source <- arguments$input
  }
  if (!is.null(arguments$output)) {
    params$file$features$taxed <- arguments$output
  }
  if (!is.null(arguments$column.name)) {
    params$file$column_name$organism <-
      arguments$column.name
  }
  if (!is.null(arguments$k.top)) {
    params$top_k$organism_per_feature <-
      as.numeric(arguments$k.top)
  }
  if (!is.null(arguments$gnps)) {
    params$job$gnps <- arguments$gnps
  }
}
stopifnot(
  "Your --tool.metadata parameter (in command line arguments or in 'inform_params.yaml' must be either'gnps' or 'manual'" = params$tool$metadata %in% c("gnps", "manual")
)
stopifnot(
  "Your --top_k.organism_per_feature parameter (in command line arguments or in 'inform_params.yaml' should be lower or equal to 5" = params$top_k$organism_per_feature <=
    5
)

log_debug(x = "loading taxa ranks dictionary")
taxa_ranks_dictionary <-
  data.table::fread(
    file = data_source_dictionaries_ranks,
    sep = "\t"
  )

if (params$tool$metadata == "gnps") {
  log_debug(x = "loading feature table")
  feature_table <- read_featuretable(id = params$job$gnps)
  
  log_debug(x = "loading metadata table")
  metadata_table <- read_metadatatable(id = params$job$gnps)
  
  log_debug(x = "removing \" Peak area\" from column names")
  colnames(feature_table) <-
    gsub(
      pattern = ".Peak.area",
      replacement = "",
      x = colnames(feature_table)
    )
  
  log_debug(x = "removing \"row m/z\" and from \"row retention time\" columns")
  feature_table <- feature_table %>%
    dplyr::select(
      -"row.m.z",
      -"row.retention.time"
    ) |>
    tibble::column_to_rownames(var = "row.ID")
  
  log_debug(x = "finding top N intensities per feature")
  top_n <- feature_table |>
    tibble::rownames_to_column() |>
    tidyr::gather(column, value, -rowname) |>
    dplyr::mutate(column = gsub(
      pattern = "^X",
      replacement = "",
      x = column
    )) |>
    dplyr::group_by(rowname) |>
    dplyr::mutate(rank = rank(-value)) |>
    dplyr::filter(rank <= params$top_k$organism_per_feature) |>
    dplyr::arrange(rowname, rank)
}

if (params$tool$metadata == "manual") {
  metadata_table <-
    data.table::fread(
      file = params$file$features$source,
      sep = "\t"
    )
}

log_debug(x = "selecting source organism column")
organism_table <- metadata_table |>
  dplyr::filter(!is.na(dplyr::all_of(params$file$column_name$organism))) |>
  dplyr::distinct(dplyr::across(dplyr::all_of(params$file$column_name$organism))) |>
  dplyr::select(organism = all_of(params$file$column_name$organism)) |>
  splitstackshape::cSplit(
    splitCols = "organism",
    sep = "|",
    direction = "long"
  ) |>
  dplyr::mutate(organism = gsub(
    pattern = " x ",
    replacement = " ",
    x = organism
  )) |>
  dplyr::distinct()

log_debug(x = "exporting source organism for GNFinder submission")
log_debug("ensuring directories exist ...")
ifelse(
  test = !dir.exists(data_interim),
  yes = dir.create(data_interim),
  no = paste(data_interim, "exists")
)
ifelse(
  test = !dir.exists(data_interim_organisms),
  yes = dir.create(data_interim_organisms),
  no = paste(data_interim_organisms, "exists")
)
data.table::fwrite(
  x = organism_table,
  file = data_interim_organisms_original,
  sep = "\t"
)

log_debug("submitting to GNVerifier")
system(command = paste("bash", gnverifier_script))

log_debug("cleaning GNVerifier results")
verified <-
  jsonlite::stream_in(con = file(data_interim_organisms_verified))

verified_df <- verified |>
  dplyr::select(
    -curation,
    -matchType
  ) |>
  tidyr::unnest(preferredResults, names_repair = "minimal") |>
  dplyr::filter(dataSourceTitleShort != "IRMNG (old)" &
                  dataSourceTitleShort != "IPNI") |>
  # filter(!matchedName %in% wrongVerifiedDictionary$wrongOrganismsVerified) |>
  dplyr::mutate(organismType = "clean") %>%
  dplyr::select(
    organismType,
    organismValue = input,
    organismCleaned = currentCanonicalFull,
    organismDbTaxo = dataSourceTitleShort,
    taxonId = currentRecordId,
    organismCleanedCurrent = currentName,
    taxonomy = classificationPath,
    rank = classificationRanks
  )

## example ID 165 empty, maybe fill later on
verified_df$organismDbTaxo <-
  y_as_na(verified_df$organismDbTaxo, "")

dataOrganismVerified <- dplyr::left_join(organism_table,
                                         verified_df,
                                         by = c("organism" = "organismValue")
) %>%
  dplyr::select(
    organism,
    organismCleaned,
    organismDbTaxo,
    taxonId,
    organismCleanedCurrent,
    organismCleaned_dbTaxoTaxonomy = taxonomy,
    organismCleaned_dbTaxoTaxonRanks = rank
  )

warning <- dataOrganismVerified |>
  dplyr::filter(!is.na(organism)) |>
  dplyr::mutate(
    organismCleaned = ifelse(
      test = organismDbTaxo == "Open Tree of Life",
      yes = organismCleaned,
      no = NA
    )
  ) |>
  dplyr::distinct(organism, organismCleaned) |>
  dplyr::group_by(organism) |>
  dplyr::add_count() |>
  dplyr::ungroup() |>
  dplyr::filter(n == 1) |>
  dplyr::select(-n) |>
  dplyr::filter(is.na(organismCleaned))

if (nrow(warning) != 0) {
  log_debug(
    "Warning:",
    warning$organism,
    "had no translation,",
    "trying with a more flexible option"
  )
  
  organism_table_2 <- dataOrganismVerified |>
    dplyr::distinct(organism, organismCleaned)
  
  organism_table_3 <- organism_table_2 |>
    dplyr::distinct(organismCleaned)
  
  data.table::fwrite(
    x = organism_table_3,
    file = data_interim_organisms_original_2,
    sep = "\t"
  )
  
  log_debug("submitting to GNVerifier")
  system(command = paste("bash", gnverifier_script_2))
  
  log_debug("cleaning GNVerifier results")
  verified_2 <-
    jsonlite::stream_in(con = file(data_interim_organisms_verified_2))
  
  verified_df_2 <- verified_2 |>
    dplyr::select(
      -curation,
      -matchType
    ) |>
    tidyr::unnest(preferredResults, names_repair = "minimal") |>
    dplyr::filter(dataSourceTitleShort != "IRMNG (old)" &
                    dataSourceTitleShort != "IPNI") |>
    # filter(!matchedName %in% wrongVerifiedDictionary$wrongOrganismsVerified) |>
    dplyr::mutate(organismType = "clean") %>%
    dplyr::select(
      organismType,
      organismValue = input,
      organismCleaned = currentCanonicalFull,
      organismDbTaxo = dataSourceTitleShort,
      taxonId = currentRecordId,
      organismCleanedCurrent = currentName,
      taxonomy = classificationPath,
      rank = classificationRanks
    )
  
  ## example ID 165 empty, maybe fill later on
  verified_df_2$organismDbTaxo <-
    y_as_na(verified_df_2$organismDbTaxo, "")
  
  dataOrganismVerified_2 <- dplyr::right_join(
    organism_table,
    organism_table_2
  )
  dataOrganismVerified_2 <-
    dplyr::left_join(dataOrganismVerified_2,
                     verified_df_2,
                     by = c("organismCleaned" = "organismValue")
    ) |>
    dplyr::select(
      organism,
      organismCleaned,
      organismDbTaxo,
      taxonId,
      organismCleanedCurrent,
      organismCleaned_dbTaxoTaxonomy = taxonomy,
      organismCleaned_dbTaxoTaxonRanks = rank
    )
  
  dataOrganismVerified_3 <-
    rbind(dataOrganismVerified, dataOrganismVerified_2) |>
    dplyr::filter(organismDbTaxo == "Open Tree of Life") |>
    dplyr::distinct()
  
  warning_2 <-
    dplyr::left_join(organism_table, dataOrganismVerified_3) %>%
    dplyr::filter(!is.na(organism)) %>%
    dplyr::filter(is.na(organismDbTaxo))
  
  if (nrow(warning_2) != 0) {
    log_debug(
      "Warning:",
      warning_2$organism,
      "had no translation,check for names at",
      "https://tree.opentreeoflife.org/"
    )
  }
} else {
  dataOrganismVerified_3 <- dataOrganismVerified
  log_debug("Good news, all your organisms were found!")
}

organism_cleaned_manipulated <-
  manipulating_taxo_otl(dfsel = dataOrganismVerified_3)

if (params$file$features$extension == FALSE) {
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
log_debug(x = "joining topN with metadata table")
if (params$tool$metadata == "gnps") {
  metadata_table_joined <-
    dplyr::left_join(top_n, metadata_table, by = c("column" = "filename")) |>
    dplyr::select(
      feature_id := rowname,
      organismOriginal = dplyr::all_of(params$file$column_name$organism),
      dplyr::everything()
    )
}

if (params$tool$metadata == "manual") {
  metadata_table_joined <- metadata_table |>
    dplyr::select(
      feature_id,
      organismOriginal = dplyr::all_of(params$file$column_name$organism),
      dplyr::everything()
    )
}

log_debug(x = "joining with cleaned taxonomy table")
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

metadata_table_joined_summarized[] <-
  lapply(
    metadata_table_joined_summarized,
    function(x) {
      y_as_na(x, y = "")
    }
  )

log_debug(x = "exporting metadata_table_biological_annotation and parameters used ...")
log_debug("ensuring directories exist ...")
ifelse(
  test = !dir.exists(data_interim),
  yes = dir.create(data_interim),
  no = paste(data_interim, "exists")
)
ifelse(
  test = !dir.exists(data_processed),
  yes = dir.create(data_processed),
  no = paste(data_processed, "exists")
)
ifelse(
  test = !dir.exists(data_processed_params),
  yes = dir.create(data_processed_params),
  no = paste(data_processed_params, "exists")
)
log_debug(
  x = "... metadata_table_biological_annotation is saved in",
  params$file$features$taxed
)
data.table::fwrite(
  x = metadata_table_joined_summarized,
  file = params$file$features$taxed,
  sep = "\t"
)

log_debug(x = "... parameters used are saved in", data_processed_params_inform)
yaml::write_yaml(x = params, file = data_processed_params_inform)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
