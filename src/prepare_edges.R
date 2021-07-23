start <- Sys.time()
language <- "r"

source(file = "R/functions/helpers.R")
log_debug(x = "sourcing files")
source(file = "paths.md")

log_debug(x = "loading libraries")
library(data.table)
library(docopt)
library(dplyr)
library(purrr)
library(yaml)

readChar(
  con = docopt_get_edges,
  nchars = file.info(docopt_get_edges)$size
) -> doc

arguments <- docopt::docopt(doc, version = "Taxo_scorer alpha 0.01")

log_debug("This script takes a list of edges and formats it for later on")
log_debug("Authors: AR")
log_debug("Contributors: ...")

params <-
  yaml::read_yaml(file = config_default_edges, handlers = list(
    seq = function(x) {
      purrr::flatten(x)
    }
  ))
params <-
  yaml::read_yaml(file = config_params_edges, handlers = list(
    seq = function(x) {
      purrr::flatten(x)
    }
  ))

log_debug("checking command line arguments")
if (exists("arguments")) {
  if (!is.null(arguments$tool)) {
    params$tool$edges <- arguments$tool
  }
  if (!is.null(arguments$input)) {
    params$file$edges$source <- arguments$input
  }
  if (!is.null(arguments$output)) {
    params$file$edges$processed <- arguments$output
  }
  if (!is.null(arguments$gnps)) {
    params$job$gnps <- arguments$gnps
  }
  if (!is.null(arguments$feature.source)) {
    params$file$column_name$source_feature <-
      arguments$feature.source
  }
  if (!is.null(arguments$feature.target)) {
    params$file$column_name$target_feature <-
      arguments$feature.target
  }
}
stopifnot(
  "Your --tool parameter (in command line arguments or in 'treat_params.yaml' must be 'manual' or 'gnps" = params$tool$edges %in% c("gnps", "manual")
)
source(file = "R/functions/features.R")

log_debug(x = "loading files ...")

if (params$tool$edges == "gnps") {
  edges_table <- read_edges(params$job$gnps)
}

if (params$tool$edges == "manual") {
  edges_table <-
    data.table::fread(
      file = params$file$edges$source,
      sep = "\t"
    )
}

edges_table_treated <- edges_table |>
  dplyr::select(
    feature_source = params$file$column_name$source_feature,
    feature_target = params$file$column_name$target_feature
  ) |>
  dplyr::filter(feature_source != feature_target)

log_debug(x = "exporting formatted edge table ...")
log_debug("ensuring directories exist ...")
ifelse(
  test = !dir.exists(data),
  yes = dir.create(data),
  no = paste(data, "exists")
)
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
  x = "... formatted edge table is saved in",
  params$file$edges$processed
)

data.table::fwrite(
  x = edges_table_treated,
  file = params$file$edges$processed,
  sep = "\t"
)

log_debug(x = "... parameters used are saved in", data_processed_params_edges)
yaml::write_yaml(x = params, file = data_processed_params_edges)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
