start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script prepares a custom library made of all prepared libraries. \n")
log_debug("Authors: AR")
log_debug("Contributors: ...")

step <- "prepare_library"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(x = "Loading and concatenating prepared libraries")
files <- list.files(
  path = paths$data$interim$libraries$path,
  pattern = "_prepared.tsv.gz",
  full.names = TRUE,
  recursive = TRUE
)
libraries <- list()
for (i in seq_along(files)) {
  libraries[[i]] <-
    readr::read_delim(file = files[[i]])
}

custom_library <- data.table::rbindlist(libraries)

if (params$filter$mode == TRUE) {
  log_debug(x = "Filtering library")
  custom_library <- custom_library |>
    dplyr::filter(grepl(
      x = !!as.name(colnames(custom_library)[grepl(
        pattern = params$filter$level,
        x = colnames(custom_library)
      )]),
      pattern = params$filter$value
    ))
}

log_debug(x = "Exporting ...")
ifelse(
  test = !dir.exists(paths$data$interim$libraries$path),
  yes = dir.create(paths$data$interim$libraries$path),
  no = paste(paths$data$interim$libraries$path, "exists")
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
  file = file.path(
    paths$data$interim$libraries$path,
    params$output
  )
)
readr::write_delim(
  x = custom_library,
  file = file.path(
    paths$data$interim$libraries$path,
    params$output
  ),
  delim = "\t"
)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
