start <- Sys.time()

source(file = "R/helpers.R")

log_debug("This script prepares a custom library made of all prepared libraries. \n")
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(data.table)
library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_library")

log_debug(x = "loading files")

files <- list.files(
  path = paths$data$interim$libraries$path,
  pattern = "_prepared.tsv.gz",
  full.names = TRUE,
  recursive = TRUE
)

libraries <- list()

for (i in seq_along(files)) {
  libraries[[i]] <-
    readr::read_delim(files[[i]])
}

custom_library <- data.table::rbindlist(libraries)

if (params$filter$mode == TRUE) {
  custom_library <- custom_library |>
    dplyr::filter(grepl(
      x = !!as.name(colnames(custom_library)[grepl(
        pattern = params$filter$level,
        x = colnames(custom_library)
      )]),
      pattern = params$filter$value
    ))
}

log_debug("exporting ...")
log_debug("ensuring directories exist ...")
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
readr::write_delim(
  x = custom_library,
  file = file.path(
    paths$data$interim$libraries$path,
    params$output
  )
)

log_debug(x = "... parameters used are saved in", paths$data$interim$config$path)
yaml::write_yaml(
  x = params,
  file = file.path(
    paths$data$interim$config$path,
    paste(
      format(Sys.time(), "%y%m%d_%H%M%OS"),
      "prepare_library.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
