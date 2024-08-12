import::from(tidytable, all_of, .into = environment())
import::from(tidytable, coalesce, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, full_join, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, rename, .into = environment())
import::from(tidytable, select, .into = environment())

#' @title Prepare features edges
#'
#' @description This function prepares edges for further use
#'
#' @importFrom tidytable all_of
#' @importFrom tidytable coalesce
#' @importFrom tidytable distinct
#' @importFrom tidytable fread
#' @importFrom tidytable full_join
#' @importFrom tidytable mutate
#' @importFrom tidytable rename
#' @importFrom tidytable select
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
#' @examples NULL
prepare_features_edges <-
  function(input = get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw,
           output = get_params(step = "prepare_features_edges")$files$networks$spectral$edges$prepared,
           name_source = get_params(step = "prepare_features_edges")$names$source,
           name_target = get_params(step = "prepare_features_edges")$names$target) {
    stopifnot("Your input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |> unlist()))
    ## Load edges table
    log_debug(x = "Loading edge table")
    edges_tables <- lapply(
      X = input,
      FUN = fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    edges_ms1 <- edges_tables[["ms1"]]
    edges_ms2 <- edges_tables[["spectral"]]
    rm(edges_tables)
    features_entropy <- edges_ms2 |>
      select(
        all_of(c(name_source)),
        feature_spectrum_entropy,
        feature_spectrum_peaks
      ) |>
      distinct()

    ## Format edges table
    log_debug(x = "Formatting edge table")
    edges_table_treated <- edges_ms1 |>
      full_join(features_entropy) |>
      full_join(edges_ms2) |>
      rename(
        feature_source = !!as.name(name_source),
        feature_target = !!as.name(name_target)
      ) |>
      mutate(feature_target := coalesce(feature_target, feature_source))
    rm(edges_ms1, edges_ms2, features_entropy)

    try(expr = {
      export_params(
        parameters = get_params(step = "prepare_features_edges"),
        step = "prepare_features_edges"
      )
    }, silent = TRUE)
    export_output(x = edges_table_treated, file = output)
    rm(edges_table_treated)

    return(output)
  }
