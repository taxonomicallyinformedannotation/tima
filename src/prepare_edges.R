start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/get_gnps.R")


log_debug("This script takes a list of edges and formats it for later on")
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_edges")

stopifnot(
  "Your --tool parameter (in command line arguments or in 'treat_params.yaml' must be 'manual' or 'gnps" = params$tool %in% c("gnps", "manual")
)

log_debug(x = "loading files")

if (params$tool == "gnps") {
  edges_table <- read_edges(params$gnps)
}

if (params$tool == "manual") {
  edges_table <-
    readr::read_delim(file = params$input)
}

edges_table_treated <- edges_table |>
  dplyr::select(
    feature_source = params$source_name,
    feature_target = params$target_name
  ) |>
  dplyr::filter(feature_source != feature_target)

log_debug(x = "exporting formatted edge table ...")
log_debug("ensuring directories exist ...")
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
  test = !dir.exists(paths$data$interim$edges$path),
  yes = dir.create(paths$data$interim$edges$path),
  no = paste(paths$data$interim$edges$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$config$path),
  yes = dir.create(paths$data$interim$config$path),
  no = paste(paths$data$interim$config$path, "exists")
)

log_debug(
  x = "... formatted edge table is saved in",
  params$output
)

readr::write_delim(
  x = edges_table_treated,
  file = params$output
)

log_debug(x = "... parameters used are saved in", paths$data$interim$config$path)
yaml::write_yaml(
  x = params,
  file = file.path(
    paths$data$interim$config$path,
    paste(
      format(Sys.time(), "%y%m%d_%H%M%OS"),
      "prepare_edges.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
