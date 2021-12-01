start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script informs taxonomically features")
log_debug("Authors: AR, P-MA")
log_debug("Contributors: ...")

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
  } else {
    log_debug(x = "Loading metadata table")
    metadata_table <- read_metadata(id = params$gnps)

    log_debug(x = "Formatting feature table ...")
    log_debug(x = "... WARNING: requires 'Peak area' in columns (MZmine format)")
    feature_table <- feature_table |>
      dplyr::select(
        `row ID`,
        matches(" Peak area")
      ) |>
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

if (params$tool == "manual") {
  metadata_table <-
    readr::read_delim(file = params$input)
}

log_debug(x = "Keeping list of organisms to submit to OTL")
organism_table <- metadata_table |>
  dplyr::filter(!is.na(dplyr::all_of(params$column_name))) |>
  dplyr::distinct(dplyr::across(dplyr::all_of(params$column_name))) |>
  dplyr::select(organism = all_of(params$column_name)) |>
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
  dplyr::distinct() |>
  dplyr::mutate(search_string = tolower(organism)) |>
  dplyr::distinct(
    organism,
    search_string
  ) |>
  dplyr::select(
    canonical_name = organism,
    search_string
  ) |>
  data.frame()

organisms <- organism_table$canonical_name

new_matched_otl_exact <- rotl::tnrs_match_names(
  names = organisms,
  do_approximate_matching = FALSE,
  include_suppressed = FALSE
)

new_ott_id <- new_matched_otl_exact |>
  dplyr::filter(!is.na(ott_id)) |>
  dplyr::distinct(ott_id)

otts <- new_ott_id$ott_id

taxon_info <- rotl::taxonomy_taxon_info(
  ott_ids = otts,
  include_lineage = TRUE,
  include_terminal_descendants = TRUE
)

taxon_lineage <- taxon_info |>
  rotl::tax_lineage()

list_df <- list()

for (i in seq_along(1:length(taxon_lineage))) {
  list_df[[i]] <- dplyr::bind_rows(
    data.frame(
      id = otts[i],
      rank = taxon_info[[i]]$rank,
      name = taxon_info[[i]]$name,
      unique_name = taxon_info[[i]]$unique_name,
      ott_id = as.character(taxon_info[[i]]$ott_id)
    ),
    data.frame(id = otts[i], taxon_lineage[[i]])
  )
}

otl <- dplyr::bind_rows(list_df) |>
  dplyr::mutate(ott_id = as.integer(ott_id))

biological_metadata <-
  dplyr::left_join(organism_table, new_matched_otl_exact) |>
  dplyr::left_join(otl, by = c("ott_id" = "id")) |>
  dplyr::filter(
    rank %in% c(
      "domain",
      "kingdom",
      "phylum",
      "class",
      "order",
      "infraorder",
      "family",
      "subfamily",
      "tribe",
      "subtribe",
      "genus",
      "subgenus",
      "species",
      "subspecies",
      "varietas"
    )
  ) |>
  dplyr::distinct() |>
  purrr::map_df(rev) |>
  ## feeling it is better that way
  dplyr::distinct(canonical_name, ott_id, rank, .keep_all = TRUE) |>
  ## canonical_name important for synonyms
  tidyr::pivot_wider(
    names_from = "rank",
    values_from = c("name", "unique_name.y", "ott_id.y")
  ) |>
  dplyr::select(
    organism_name = canonical_name,
    organism_taxonomy_ottid = ott_id,
    organism_taxonomy_01domain = dplyr::matches("name_domain"),
    organism_taxonomy_02kingdom = dplyr::matches("name_kingdom"),
    organism_taxonomy_03phylum = dplyr::matches("name_phylum"),
    organism_taxonomy_04class = dplyr::matches("name_class"),
    organism_taxonomy_05order = dplyr::matches("name_order"),
    organism_taxonomy_06family = dplyr::matches("name_family"),
    organism_taxonomy_07tribe = dplyr::matches("name_tribe"),
    organism_taxonomy_08genus = dplyr::matches("name_genus"),
    organism_taxonomy_09species = dplyr::matches("name_species"),
    organism_taxonomy_10varietas = dplyr::matches("name_varietas")
  ) |>
  purrr::map_df(rev) |>
  dplyr::coalesce()

if (is.null(params$force) &
  params$extension == FALSE) {
  log_debug("Removing filename extensions")
  metadata_table <- metadata_table |>
    dplyr::mutate(filename = gsub(
      pattern = ".mzML",
      replacement = "",
      x = filename,
      fixed = TRUE
    )) |>
    dplyr::mutate(filename = gsub(
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
      feature_table |> dplyr::mutate(feature_id = `row ID`),
      biological_metadata |>
        dplyr::select(organismOriginal = organism_name)
    )
  } else {
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
      dplyr::everything()
    )
}

log_debug(x = "Joining with cleaned taxonomy table")
metadata_table_joined_summarized <-
  dplyr::left_join(
    metadata_table_joined,
    biological_metadata,
    by = c("organismOriginal" = "organism_name")
  ) |>
  dplyr::distinct() |>
  dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), as.character) |>
  dplyr::select(
    feature_id,
    sample_organism_01_domain = dplyr::matches("organism_taxonomy_01domain"),
    sample_organism_02_kingdom = dplyr::matches("organism_taxonomy_02kingdom"),
    sample_organism_03_phylum = dplyr::matches("organism_taxonomy_03phylum"),
    sample_organism_04_class = dplyr::matches("organism_taxonomy_04class"),
    sample_organism_05_order = dplyr::matches("organism_taxonomy_05order"),
    sample_organism_06_family = dplyr::matches("organism_taxonomy_06family"),
    sample_organism_07_tribe = dplyr::matches("organism_taxonomy_07tribe"),
    sample_organism_08_genus = dplyr::matches("organism_taxonomy_08genus"),
    sample_organism_09_species = dplyr::matches("organism_taxonomy_09species"),
    sample_organism_10_varietas = dplyr::matches("organism_taxonomy_10varietas")
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::summarise_all(function(x) {
    x <- list(paste(unique(x[!is.na(x)]), collapse = "|"))
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate_all(as.character)

log_debug(x = "joining with cleaned taxonomy table")
metadata_table_joined_summarized[] <-
  lapply(
    metadata_table_joined_summarized,
    function(x) {
      y_as_na(x, y = "")
    }
  )

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
ifelse(
  test = !dir.exists(dirname(params$output)),
  yes = dir.create(dirname(params$output)),
  no = paste(dirname(params$output), "exists")
)

log_debug(
  x = "... path to export is",
  params$output
)
readr::write_delim(
  x = metadata_table_joined_summarized,
  file = params$output,
  delim = "\t"
)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
