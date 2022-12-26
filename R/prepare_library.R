#' @title Prepare library
#'
#' @param filter TODO
#' @param level TODO
#' @param value TODO
#' @param output TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
prepare_library <- function(filter = params$filter$mode,
                            level = params$filter$level,
                            value = params$filter$value,
                            output = params$output) {
  # Check if the filter parameter is valid
  stopifnot("Your filter parameter must be 'true' or 'false'" = filter %in% c(TRUE, FALSE))

  # If filter is TRUE, check if the level parameter is valid
  if (isTRUE(filter)) {
    stopifnot(
      "Your level parameter must be one of:
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
      " = level %in% c(
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

  # Load and concatenate the prepared libraries
  log_debug(x = "Loading and concatenating prepared libraries")
  files <- list.files(
    path = paths$data$interim$libraries$path,
    pattern = "_prepared.tsv.gz",
    full.names = TRUE,
    recursive = TRUE
  )
  libraries <- list()
  for (i in seq_along(files)) {
    libraries[[i]] <- readr::read_delim(file = files[[i]])
  }

  custom_library <- data.table::rbindlist(libraries)

  # If filter is TRUE, filter the library based on the specified level and value
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

  # Export the library
  log_debug(x = "Exporting ...")
  export_params(step = "prepare_library")
  export_output(
    x = custom_library,
    file = file.path(
      paths$data$interim$libraries$path,
      output
    )
  )
}
