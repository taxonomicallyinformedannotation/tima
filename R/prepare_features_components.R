import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, select, .into = environment())

#' @title Prepare features components
#'
#' @description This function prepares the components
#'    (clusters in molecular network) for further use
#'
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable fread
#' @importFrom tidytable select
#'
#' @include get_params.R
#'
#' @param input Input file
#' @param output Output file
#'
#' @return The path to the prepared features' components
#'
#' @export
#'
#' @examples NULL
prepare_features_components <-
  function(input = get_params(step = "prepare_features_components")$files$networks$spectral$components$raw,
           output = get_params(step = "prepare_features_components")$files$networks$spectral$components$prepared) {
    stopifnot("Input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |> unlist()))

    log_debug(x = "Loading files ...")
    log_debug(x = "... components table")
    table <- lapply(
      X = input,
      FUN = fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      bind_rows() |>
      select(feature_id = `cluster index`, component_id = componentindex) |>
      distinct()

    log_debug(x = "Exporting ...")
    export_params(
      parameters = get_params(step = "prepare_features_components"),
      step = "prepare_features_components"
    )
    export_output(x = table, file = output)
    rm(table)

    return(output)
  }
