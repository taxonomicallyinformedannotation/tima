start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("does everything you ever dreamt of. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("PMA"), "\n")

## Get all files

#### MGF
get_example_spectra(url = paths$url$examples$mgf_mini)

### SIRIUS
get_example_sirius(url = paths$urls$examples$sirius_mini)

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
  url = paths$url$examples$spectral_lib,
  destfile = paths$data$source$spectra$lotus$pos
)
utils::download.file(
  url = paths$url$examples$spectral_lib,
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
# log_debug("Preparing closed")
# prepare_closed()

### Structural library
log_debug("Preparing structural library")
step <- "prepare_library"
params <- get_params(step = step)
prepare_library()

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
step <- "prepare_edges"
params <- get_params(step = step)
prepare_edges()

### Features components
log_debug("Preparing features components")
step <- "prepare_features_components"
params <- get_params(step = step)
prepare_features_components()

### Features classification
log_debug("Preparing features classification")
step <- "prepare_features_classification"
params <- get_params(step = step)
prepare_features_classification()

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
