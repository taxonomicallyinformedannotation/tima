#' @title Prepare features edges
#'
#' @description This function prepares edges for further use
#'
#' @include get_params.R
#'
#' @param input Input file if 'manual'
#' @param output Output file
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#'
#' @return The path to the prepared edges
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
#' input_1 <- get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw$ms1
#' input_2 <- get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw$spectral
#' get_file(url = paste0(dir, input_1), export = input_1)
#' get_file(url = paste0(dir, input_2), export = input_2)
#' prepare_features_edges(
#'   input = list("ms1" = input_1, "spectral" = input_2)
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_features_edges <-
  function(
    input = get_params(
      step = "prepare_features_edges"
    )$files$networks$spectral$edges$raw,
    output = get_params(
      step = "prepare_features_edges"
    )$files$networks$spectral$edges$prepared,
    name_source = get_params(step = "prepare_features_edges")$names$source,
    name_target = get_params(step = "prepare_features_edges")$names$target
  ) {
    stopifnot(
      "Your input file(s) do(es) not exist" = all(
        purrr::map(.x = input, .f = file.exists) |> unlist()
      )
    )
    ## Load edges table
    logger::log_trace("Loading edge table")
    edges_tables <- purrr::map(
      .x = input,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    edges_ms1 <- edges_tables[["ms1"]]
    edges_ms2 <- edges_tables[["spectral"]]
    rm(edges_tables)
    features_entropy <- edges_ms2 |>
      tidytable::select(
        tidyselect::all_of(c(name_source)),
        feature_spectrum_entropy,
        feature_spectrum_peaks
      ) |>
      tidytable::distinct()

    ## Format edges table
    logger::log_trace("Formatting edge table")
    edges_table_treated <- edges_ms1 |>
      tidytable::full_join(features_entropy) |>
      tidytable::full_join(edges_ms2) |>
      tidytable::rename(
        feature_source = !!as.name(name_source),
        feature_target = !!as.name(name_target)
      ) |>
      tidytable::mutate(
        feature_target := tidytable::coalesce(feature_target, feature_source)
      )
    rm(edges_ms1, edges_ms2, features_entropy)

    export_params(
      parameters = get_params(step = "prepare_features_edges"),
      step = "prepare_features_edges"
    )
    export_output(x = edges_table_treated, file = output)
    rm(edges_table_treated)

    return(output)
  }
