utils::globalVariables(
  c(
    "cluster index",
    "componentindex",
    "params"
  )
)

#' @title Prepare features components
#'
#' @description This function prepares the components (clusters in molecular network) for further use
#'
#' @include export_output.R
#' @include export_params.R
#'
#' @param input Input file
#' @param output Output file
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_features_components <-
  function(input = params$files$networks$spectral$components$raw,
           output = params$
             files$
             networks$
             spectral$
             components$
             prepared,
           parameters = params) {
    # Check if input file(s) exists
    stopifnot(
      "Input file(s) do(es) not exist" =
        rep(TRUE, length(input)) ==
          lapply(X = input, file.exists)
    )
    params <<- parameters

    log_debug(x = "Loading files ...")
    log_debug(x = "... components table")
    table <- lapply(
      X = input,
      FUN = tidytable::fread,
      na.strings = c("", "NA")
    ) |>
      tidytable::bind_rows() |>
      tidytable::select(
        feature_id = `cluster index`,
        component_id = componentindex
      ) |>
      tidytable::distinct()

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_features_components")
    export_output(x = table, file = output)

    return(output)
  }
