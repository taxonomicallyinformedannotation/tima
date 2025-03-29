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
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' input <- get_params(step = "prepare_features_components")$files$networks$spectral$components$raw
#' get_file(url = paste0(dir, input), export = input)
#' prepare_features_components(
#'   input = input
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_features_components <-
  function(
    input = get_params(
      step = "prepare_features_components"
    )$files$networks$spectral$components$raw,
    output = get_params(
      step = "prepare_features_components"
    )$files$networks$spectral$components$prepared
  ) {
    stopifnot(
      "Input file(s) do(es) not exist" = all(
        purrr::map(.x = input, .f = file.exists) |> unlist()
      )
    )

    logger::log_trace("Loading files")
    logger::log_trace("Components table")
    table <- purrr::map(
      .x = input,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows() |>
      tidytable::select(
        feature_id = `cluster index`,
        component_id = componentindex
      ) |>
      tidytable::distinct()

    export_params(
      parameters = get_params(step = "prepare_features_components"),
      step = "prepare_features_components"
    )
    export_output(x = table, file = output)
    rm(table)

    return(output)
  }
