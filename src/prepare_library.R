start <- Sys.time()
language <- "r"

library(docopt)
source(file = "R/functions/helpers.R")
source(file = "paths.md")

doc <- readChar(
    con = docopt_prepare_lib,
    nchars = file.info(docopt_prepare_lib)$size
)

arguments <- docopt(doc, version = "beta")

log_debug(x = "loading libraries")
library(data.table)
library(dplyr)
library(purrr)
library(yaml)

log_debug("This script prepares a custom library made of all prepared libraries. \n")
log_debug("Authors: AR")
log_debug("Contributors: ...")

params <-
    yaml::read_yaml(file = config_default_library, handlers = list(
        seq = function(x) {
            purrr::flatten(x)
        }
    ))
params <-
    yaml::read_yaml(file = config_params_library, handlers = list(
        seq = function(x) {
            purrr::flatten(x)
        }
    ))

log_debug("checking command line arguments")
if (exists("arguments")) {
    if (!is.null(arguments$filter)) {
        params$filter$mode <- arguments$filter
    }
    if (!is.null(arguments$level)) {
        params$filter$level <- arguments$level
    }
    if (!is.null(arguments$value)) {
        params$filter$value <- arguments$value
    }
    if (!is.null(arguments$output)) {
        params$file$output <- arguments$output
    }
}

log_debug(x = "loading files")

files <- list.files(
    path = data_interim,
    pattern = "_prepared.tsv.gz",
    full.names = TRUE,
    recursive = TRUE
)

libraries <- list()

for (i in seq_along(files)) {
    libraries[[i]] <-
        data.table::fread(files[[i]])
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
    test = !dir.exists(data_processed),
    yes = dir.create(data_processed),
    no = paste(data_processed, "exists")
)
data.table::fwrite(
    x = custom_library,
    file = file.path(
        data_processed,
        paste0(params$file$output, "_prepared.tsv.gz")
    ),
    sep = "\t"
)

log_debug(x = "... parameters used are saved in", data_processed)
yaml::write_yaml(
    x = params,
    file = file.path(data_processed, paste(
        format(Sys.time(), "%y%m%d_%H%M%OS"),
        "library_params.yaml",
        sep = "_"
    ))
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
