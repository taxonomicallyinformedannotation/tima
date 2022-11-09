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

#' 1.1
log_debug("Getting LOTUS")
get_lotus()

#' 1.2
log_debug("Preparing LOTUS")
prepare_lotus()

#' 1.3
log_debug("Preparing closed")
step <- "prepare_closed"
params <- get_params(step = step)
prepare_closed()

#' 1.4
log_debug("Preparing library")
step <- "prepare_library"
params <- get_params(step = step)
prepare_library()

#' 1.5
log_debug("Preparing adducts")
step <- "prepare_adducts"
params <- get_params(step = step)
prepare_adducts()

#' 2
log_debug("Getting ISDB example")
get_example_isdb()

#' 3.1
log_debug("Preparing ISDB")
step <- "prepare_isdb"
params <- get_params(step = step)
prepare_isdb()

#' 3.2
log_debug("Preparing GNPS")
step <- "prepare_gnps"
params <- get_params(step = step)
prepare_gnps()

#' 3.3
# log_debug("Preparing SIRIUS")
# step <- "prepare_sirius"
# params <- get_params(step = step)
# prepare_sirius()

#' 4.1
log_debug("Preparing edges")
step <- "prepare_edges"
params <- get_params(step = step)
prepare_edges()

#' 4.2
log_debug("Preparing features components")
step <- "prepare_features_components"
params <- get_params(step = step)
prepare_features_components()

#' 4.3
log_debug("Preparing features classification")
step <- "prepare_features_classification"
params <- get_params(step = step)
prepare_features_classification()

#' 4.4
log_debug("Preparing taxa")
step <- "prepare_taxa"
params <- get_params(step = step)
prepare_taxa()

#' 5
log_debug("Processing annotations")
step <- "process_annotations"
params <- get_params(step = step)
process_annotations()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
