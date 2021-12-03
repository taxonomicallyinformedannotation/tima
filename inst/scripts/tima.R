start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat(
  "This script",
  crayon::green("does everything you ever dreamt of. \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("PMA"), "\n")

paths <- parse_yaml_paths()

## 1.1
get_lotus()

## 1.2
prepare_lotus()

## 1.3
step <- "prepare_closed"
params <- get_params(step = step)
prepare_closed()

## 1.4
step <- "prepare_library"
params <- get_params(step = step)
prepare_library()

## 1.5
step <- "prepare_adducts"
params <- get_params(step = step)
prepare_adducts()

## 2
get_example_isdb()

## 3.a
step <- "prepare_isdb"
params <- get_params(step = step)
prepare_isdb()

## 3.b
step <- "prepare_gnps"
params <- get_params(step = step)
prepare_gnps()

## 3.c
# step <- "prepare_sirius"
# params <- get_params()
# prepare_sirius()

## 4.1
step <- "prepare_edges"
params <- get_params(step = step)
prepare_edges()

## 4.2
step <- "prepare_features_components"
params <- get_params(step = step)
prepare_features_components()

## 4.3
step <- "prepare_features_classification"
params <- get_params(step = step)
prepare_features_classification()

## 4.4
step <- "prepare_taxa"
params <- get_params(step = step)
prepare_taxa()

## 4.5
step <- "process_annotations"
params <- get_params(step = step)
process_annotations()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
