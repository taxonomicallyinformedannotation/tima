start <- Sys.time()

require(package = "timaR",
        quietly = TRUE)

log_debug("This script takes a list of edges and formats it for later on")
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug("Loading packages")
if (!require(docopt)) {
  install.packages("docopt")
  library(
    package = "docopt",
    quietly = TRUE
  )
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(
    package = "dplyr",
    quietly = TRUE,
    warn.conflicts = FALSE
  )
}
if (!require(purrr)) {
  install.packages("purrr")
  library(
    package = "purrr",
    quietly = TRUE
  )
}
if (!require(readr)) {
  install.packages("readr")
  library(
    package = "readr",
    quietly = TRUE
  )
}
if (!require(yaml)) {
  install.packages("yaml")
  library(
    package = "yaml",
    quietly = TRUE
  )
}

step <- "prepare_edges"
paths <- parse_yaml_paths()
params <- get_params(step = step)

stopifnot(
  "Your --tool parameter (in command line arguments or in 'treat_params.yaml' must be 'manual' or 'gnps" = params$tool %in% c("gnps", "manual")
)

log_debug(x = "Loading edge table")
if (params$tool == "gnps") {
  edges_table <- read_edges(params$gnps)
}

if (params$tool == "manual") {
  edges_table <-
    readr::read_delim(file = params$input)
}

log_debug(x = "Formatting edge table")
edges_table_treated <- edges_table |>
  dplyr::select(
    feature_source = params$source_name,
    feature_target = params$target_name
  ) |>
  dplyr::filter(feature_source != feature_target)

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
  test = !dir.exists(paths$data$interim$edges$path),
  yes = dir.create(paths$data$interim$edges$path),
  no = paste(paths$data$interim$edges$path, "exists")
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
  x = edges_table_treated,
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
