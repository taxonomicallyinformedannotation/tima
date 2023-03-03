start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("does everything you ever dreamt of. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("PMA"), "\n")

## Prepare params
paths <- parse_yaml_paths()
step <- "prepare_params"
params <- get_params(step = step)
prepare_params()

## Get all files
get_gnps_tables(gnps_job_id = params$gnps$id)
#### spectra
get_file(
  url = paths$url$examples$spectra_mini,
  export = paths$data$source$spectra
)
get_file(
  url = paths$url$examples$spectral_lib_mini$with_rt,
  export = paths$data$source$libraries$spectra$exp$with_rt
)

### SIRIUS
sirius_mini <- paths$data$interim$annotations$example_sirius |>
  gsub(pattern = ".zip", replacement = "_mini.zip")
get_file(
  url = paths$urls$examples$sirius_mini,
  export = sirius_mini
)
message("Unzipping")
utils::unzip(
  zipfile = sirius_mini,
  exdir = dirname(sirius_mini)
)

### LOTUS
log_debug("Getting LOTUS")
get_last_version_from_zenodo(
  doi = paths$url$lotus$doi,
  pattern = paths$urls$lotus$pattern,
  path = paths$data$source$libraries$sop$lotus
)

### HMDB
# log_debug("Getting HMDB")
# get_hmdb()

### LOTUS ISDB
log_debug("Getting LOTUS ISDB...")
create_dir(paths$data$source$libraries$spectra$is$lotus$pos)
utils::download.file(
  url = paths$url$examples$spectral_lib$pos,
  destfile = paths$data$source$libraries$spectra$is$lotus$pos
)
utils::download.file(
  url = paths$url$examples$spectral_lib$neg,
  destfile = paths$data$source$libraries$spectra$is$lotus$neg
)

### HMDB ISDB
# log_debug("Getting HMDB ISDB...")
# get_isdb_hmdb()

### MONA
# log_debug("Getting MONA spectra ..")
# get_mona() ## not available actually

## Prepare all files
### LOTUS
log_debug("Preparing LOTUS")
prepare_libraries_sop_lotus()

### HMDB
# log_debug("Preparing HMDB")
# prepare_hmdb()

### Closed
log_debug("Preparing closed")
step <- "prepare_libraries_sop_closed"
params <- get_params(step = step)
prepare_libraries_sop_closed()

### SOP library
log_debug("Preparing sop library")
step <- "prepare_libraries_sop_merged"
params <- get_params(step = step)
prepare_libraries_sop_merged()

### ISDB LOTUS
# log_debug("Preparing ISDB LOTUS")
step <- "prepare_libraries_spectra"
params <- get_params(step = step)
prepare_libraries_spectra()
prepare_libraries_spectra(polarity = "neg")
prepare_libraries_spectra(
  input = paths$data$source$libraries$spectra$is$lotus$pos,
  output = paths$data$interim$libraries$spectra$is$lotus$pos,
  col_ce = NULL,
  col_ci = "FILENAME",
  col_em = "EXACTMASS",
  col_in = NULL,
  col_io = "INCHI",
  col_ik = NULL,
  col_il = "NAME",
  col_mf = "MOLECULAR_FORMULA",
  col_na = NULL,
  col_po = "IONMODE",
  col_sm = NULL,
  col_sn = "SMILES",
  col_si = NULL,
  col_sp = NULL,
  col_sy = NULL,
  col_xl = NULL,
  metad = CompoundDb::make_metadata(
    source = "LOTUS",
    url = "https://doi.org/10.5281/zenodo.5607185",
    source_version = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")$doi_url,
    source_date = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")[["metadata"]][["publication_date"]],
    organism = "Life"
  )
)
prepare_libraries_spectra(
  input = paths$data$source$libraries$spectra$is$lotus$neg,
  output = paths$data$interim$libraries$spectra$is$lotus$neg,
  polarity = "neg",
  col_ce = NULL,
  col_ci = "FILENAME",
  col_em = "EXACTMASS",
  col_in = NULL,
  col_io = "INCHI",
  col_ik = NULL,
  col_il = "NAME",
  col_mf = "MOLECULAR_FORMULA",
  col_na = NULL,
  col_po = "IONMODE",
  col_sm = NULL,
  col_sn = "SMILES",
  col_si = NULL,
  col_sp = NULL,
  col_sy = NULL,
  col_xl = NULL,
  metad = CompoundDb::make_metadata(
    source = "LOTUS",
    url = "https://doi.org/10.5281/zenodo.5607185",
    source_version = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")$doi_url,
    source_date = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")[["metadata"]][["publication_date"]],
    organism = "Life"
  )
)

### ISDB HMDB
# log_debug("Preparing ISDB HMDB")
# prepare_isdb_hmdb()

### MONA
# log_debug("Preparing MONA")
# prepare_mona()

### Adducts
log_debug("Preparing adducts")
step <- "prepare_libraries_adducts"
params <- get_params(step = step)
prepare_libraries_adducts()

## Preparing features
log_debug("Preparing features")
step <- "prepare_features_tables"
params <- get_params(step = step)
prepare_features_tables()

## Performing MS1 annotation
log_debug("Performing MS1 annotation")
step <- "annotate_masses"
params <- get_params(step = step)
annotate_masses()

## Performing MS2 annotation
log_debug("Performing MS2 annotation")
step <- "annotate_spectra"
params <- get_params(step = step)
annotate_spectra()

## Create MS2 based edges
log_debug("Creating MS2-based edges")
step <- "create_edges_spectra"
params <- get_params(step = step)
create_edges_spectra()

### GNPS results
log_debug("Preparing GNPS")
step <- "prepare_annotations_gnps"
params <- get_params(step = step)
prepare_annotations_gnps()

### SIRIUS results
log_debug("Preparing SIRIUS")
step <- "prepare_annotations_sirius"
params <- get_params(step = step)
prepare_annotations_sirius()

### Spectral matches results
log_debug("Preparing spectral matches")
step <- "prepare_annotations_spectra"
params <- get_params(step = step)
prepare_annotations_spectra()

### Edges
log_debug("Preparing edges")
step <- "prepare_features_edges"
params <- get_params(step = step)
prepare_features_edges()

### Features components
log_debug("Preparing features components")
step <- "prepare_features_components"
params <- get_params(step = step)
prepare_features_components()

### Taxa
log_debug("Preparing taxa")
step <- "prepare_taxa"
params <- get_params(step = step)
prepare_taxa()

## Perform TIMA
log_debug("Processing annotations")
step <- "weight_annotations"
params <- get_params(step = step)
weight_annotations(
  candidates_final = 1,
  minimal_ms1_bio = 0.8
)

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
