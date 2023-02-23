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
step <- "prepare_config"
params <- get_params(step = step)
prepare_config()

## Get all files
#### spectra
download_file(
  url = paths$url$examples$spectra_mini,
  export = paths$data$source$examples$spectra
)

### SIRIUS
sirius_mini <- paths$data$interim$annotations$example_sirius |>
  gsub(pattern = ".zip", replacement = "_mini.zip")
download_file(
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
  path = paths$data$source$libraries$lotus
)

### HMDB
# log_debug("Getting HMDB")
# get_hmdb()

### LOTUS ISDB
log_debug("Getting LOTUS ISDB...")
create_dir(paths$data$source$spectra$lotus$pos)
utils::download.file(
  url = paths$url$examples$spectral_lib$pos,
  destfile = paths$data$source$spectra$lotus$pos
)
utils::download.file(
  url = paths$url$examples$spectral_lib$neg,
  destfile = paths$data$source$spectra$lotus$neg
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
prepare_lotus()

### HMDB
# log_debug("Preparing HMDB")
# prepare_hmdb()

### Closed
log_debug("Preparing closed")
step <- "prepare_closed"
params <- get_params(step = step)
prepare_closed()

### Structural library
log_debug("Preparing structural library")
step <- "prepare_libraries"
params <- get_params(step = step)
prepare_libraries()

### ISDB LOTUS
# log_debug("Preparing ISDB LOTUS")
prepare_isdb_lotus()

### ISDB HMDB
# log_debug("Preparing ISDB HMDB")
# prepare_isdb_hmdb()

### MONA
# log_debug("Preparing MONA")
# prepare_mona()

### Adducts
log_debug("Preparing adducts")
step <- "prepare_adducts"
params <- get_params(step = step)
prepare_adducts()

## Performing MS2 annotation
log_debug("Performing MS2 annotation")
step <- "process_spectra"
params <- get_params(step = step)
process_spectra()

### GNPS results
log_debug("Preparing GNPS")
step <- "prepare_gnps"
params <- get_params(step = step)
prepare_gnps()

### SIRIUS results
log_debug("Preparing SIRIUS")
step <- "prepare_sirius"
params <- get_params(step = step)
prepare_sirius()

### Spectral matches results
log_debug("Preparing spectral matches")
step <- "prepare_spectral_matches"
params <- get_params(step = step)
prepare_spectral_matches()

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
step <- "process_annotations"
params <- get_params(step = step)
process_annotations()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
