# Helper fixtures for benchmark_taxize_spectra tests ----

# Creates a features table and writes to disk
create_bench_features <- function(dir, feature_ids, inchikey_layers) {
  stopifnot(length(feature_ids) == length(inchikey_layers))
  path <- file.path(dir, "features.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = feature_ids,
      inchikey_connectivity_layer = inchikey_layers
    ),
    path,
    sep = "\t"
  )
  path
}

# Creates a SOP keys table and writes to disk
# structure_inchikey must be full; connectivity layer is first 14 chars
create_bench_keys <- function(dir, structure_inchikeys, organisms) {
  stopifnot(length(structure_inchikeys) == length(organisms))
  path <- file.path(dir, "keys.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = structure_inchikeys,
      organism_name = organisms
    ),
    path,
    sep = "\t"
  )
  path
}

# Creates a taxonomy table with required columns; organisms may repeat
create_bench_taxonomy <- function(dir, organisms, with_missing = FALSE) {
  path <- file.path(dir, "tax.tsv")
  if (length(organisms) == 0) {
    tax <- tidytable::tidytable(
      organism_name = character(),
      organism_taxonomy_01domain = character(),
      organism_taxonomy_02kingdom = character(),
      organism_taxonomy_03phylum = character(),
      organism_taxonomy_04class = character(),
      organism_taxonomy_05order = character(),
      organism_taxonomy_06family = character(),
      organism_taxonomy_07tribe = character(),
      organism_taxonomy_08genus = character(),
      organism_taxonomy_09species = character(),
      organism_taxonomy_10varietas = character()
    )
  } else {
    tax <- tidytable::tidytable(
      organism_name = organisms,
      organism_taxonomy_01domain = if (with_missing) {
        c(rep("Eukaryota", length(organisms) - 1), NA_character_)
      } else {
        rep("Eukaryota", length(organisms))
      },
      organism_taxonomy_02kingdom = rep("Plantae", length(organisms)),
      organism_taxonomy_03phylum = rep("TestPhylum", length(organisms)),
      organism_taxonomy_04class = rep("TestClass", length(organisms)),
      organism_taxonomy_05order = rep("TestOrder", length(organisms)),
      organism_taxonomy_06family = rep("TestFamily", length(organisms)),
      organism_taxonomy_07tribe = rep("TestTribe", length(organisms)),
      organism_taxonomy_08genus = rep("TestGenus", length(organisms)),
      organism_taxonomy_09species = organisms,
      organism_taxonomy_10varietas = NA_character_
    )
  }
  tidytable::fwrite(x = tax, file = path, sep = "\t")
  path
}

# Generates full set (features, keys, taxonomy) for a basic successful case
create_bench_full_set <- function(
  dir,
  feature_ids,
  inchikey_layers,
  structure_inchikeys,
  organisms,
  with_missing_tax = FALSE
) {
  list(
    features = create_bench_features(dir, feature_ids, inchikey_layers),
    keys = create_bench_keys(dir, structure_inchikeys, organisms),
    taxonomy = create_bench_taxonomy(
      dir,
      organisms,
      with_missing = with_missing_tax
    ),
    output = file.path(dir, "output.tsv")
  )
}

# Reads output quickly
read_bench_output <- function(path) {
  tidytable::fread(path, na.strings = c("", "NA"), colClasses = "character")
}
