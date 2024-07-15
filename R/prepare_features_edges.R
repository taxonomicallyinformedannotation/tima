#' @title Prepare features edges
#'
#' @description This function prepares edges for further use
#'
#' @param input Input file if 'manual'
#' @param output Output file
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_features_edges <-
  function(input = get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw,
           output = get_params(step = "prepare_features_edges")$files$networks$spectral$edges$prepared,
           name_source = get_params(step = "prepare_features_edges")$names$source,
           name_target = get_params(step = "prepare_features_edges")$names$target) {
    stopifnot(
      "Your input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |> unlist())
    )
    ## Load edges table
    log_debug(x = "Loading edge table")
    edges_tables <- lapply(
      X = input,
      FUN = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    edges_ms1 <- edges_tables[["ms1"]]
    edges_ms2 <- edges_tables[["spectral"]]
    rm(edges_tables)
    features_entropy <- edges_ms2 |>
      tidytable::select(
        tidytable::all_of(c(
          name_source
        )),
        feature_spectrum_entropy,
        feature_spectrum_peaks
      ) |>
      tidytable::distinct()

    ## Format edges table
    log_debug(x = "Formatting edge table")
    edges_table_treated <- edges_ms1 |>
      tidytable::full_join(features_entropy) |>
      tidytable::full_join(edges_ms2) |>
      tidytable::rename(
        feature_source = !!as.name(name_source),
        feature_target = !!as.name(name_target)
      ) |>
      tidytable::mutate(
        feature_target := tidytable::coalesce(
          feature_target,
          feature_source
        )
      )
    rm(edges_ms1, edges_ms2, features_entropy)

    ## Export edges table
    log_debug(x = "Exporting ...")
    export_params(parameters = get_params(step = "prepare_features_edges"), step = "prepare_features_edges")
    export_output(x = edges_table_treated, file = output)
    rm(edges_table_treated)

    return(output)
  }
