#' @title Prepare library
#'
#' @description This function prepares the library made of all sub-libraries containing structure-organism pairs
#'
#' @details It can be restricted to specific taxa to have more biologically meaningful annotation.
#'
#' @param files List of libraries to be merged
#' @param filter Boolean. TRUE or FALSE if you want to filter the library
#' @param level Biological rank to be filtered. Kingdom, phylum, family, genus, ...
#' @param value Name of the taxon or taxa to be kept, e.g. 'Gentianaceae|Apocynaceae'
#' @param output Output file
#' @param parameters Param
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr filter
#' @importFrom readr read_delim write_delim
#'
#' @examples NULL
prepare_library <-
  function(files = params$files$libraries$sop$processed,
           filter = params$organisms$filter$mode,
           level = params$organisms$filter$level,
           value = params$organisms$filter$value,
           output = params$files$libraries$sop$merged,
           parameters = params) {
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
    params <<- parameters
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
      if (nrow(custom_library) == 0) {
        stop("Your filter led to no entries, try to change it.")
      }
    }

    # Export the library
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_library")
    export_output(
      x = custom_library,
      file = output
    )

    return(output)
  }
