#' Title
#'
#' @param filter TODO
#' @param level TODO
#' @param value TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_library <-
  function(filter = params$filter$mode,
           level = params$filter$level,
           value = params$filter$value,
           output = params$output) {
    stopifnot("Your filter parameter must be 'true' or 'false'" = filter %in% c(TRUE, FALSE))
    if (isTRUE(filter)) {
      stopifnot(
        "Your level parameter must be one of
        'domain',
        'kingdom',
        'phylum',
        'class',
        'order',
        'family',
        'tribe',
        'genus',
        'species',
        'varietas'
        " = annotate %in% c(
          "domain",
          "kingdom",
          "phylum",
          "class",
          "order",
          "family",
          "tribe",
          "genus",
          "species",
          "varietas"
        )
      )
    }

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

    if (filter == TRUE) {
      log_debug(x = "Filtering library")
      custom_library <- custom_library |>
        dplyr::filter(grepl(
          x = !!as.name(colnames(custom_library)[grepl(
            pattern = level,
            x = colnames(custom_library)
          )]),
          pattern = value
        ))
    }

    log_debug(x = "Exporting ...")
    ifelse(
      test = !dir.exists(dirname(paths$data$interim$libraries$path)),
      yes = dir.create(dirname(paths$data$interim$libraries$path)),
      no = paste(dirname(paths$data$interim$libraries$path), "exists")
    )
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
      test = !dir.exists(dirname(output)),
      yes = dir.create(dirname(output)),
      no = paste(dirname(output), "exists")
    )

    log_debug(
      x = "... path to export is",
      file = file.path(
        paths$data$interim$libraries$path,
        output
      )
    )
    readr::write_delim(
      x = custom_library,
      file = file.path(
        paths$data$interim$libraries$path,
        output
      ),
      delim = "\t"
    )

    export_params(
      parameters = params,
      directory = paths$data$interim$config$path,
      step = "prepare_library"
    )
  }
