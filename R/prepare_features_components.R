#' @title Prepare features components
#'
#' @description This function prepares the components
#'    (clusters in molecular network) for further use
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
#' @examples
#' go_to_cache(dir = system.file(package = "tima"))
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' input <- get_params(step = "prepare_features_components")$files$networks$spectral$components$raw
#' get_file(url = paste0(dir, input), export = input)
#' prepare_features_components(
#'   input = input
#' )
#' unlink("data", recursive = TRUE)
prepare_features_components <-
  function(input = get_params(step = "prepare_features_components")$files$networks$spectral$components$raw,
           output = get_params(step = "prepare_features_components")$files$networks$spectral$components$prepared) {
    stopifnot("Input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |> unlist()))

    log_debug(x = "Loading files ...")
    log_debug(x = "... components table")
    table <- lapply(
      X = input,
      FUN = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows() |>
      tidytable::select(feature_id = `cluster index`, component_id = componentindex) |>
      tidytable::distinct()

    export_params(
      parameters = get_params(step = "prepare_features_components"),
      step = "prepare_features_components"
    )
    export_output(x = table, file = output)
    rm(table)

    return(output)
  }
