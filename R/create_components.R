#' @title Create components
#'
#' @description This function create components from edges
#'
#' @param input Input file(s) containing edges
#' @param output Output file.
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_components <-
  function(input = params$files$networks$spectral$edges$processed,
           output = params$files$networks$spectral$components$raw,
           parameters = params) {
    stopifnot(
      "Your input file(s) do(es) not exist" =
        rep(TRUE, length(unlist(input))) ==
          lapply(X = unlist(input), file.exists)
    )

    edges <- lapply(
      X = input,
      readr::read_delim,
      col_select = c(feature_source, feature_target)
    ) |>
      dplyr::bind_rows() |>
      dplyr::distinct()

    g <- igraph::graph.data.frame(
      d = edges,
      directed = FALSE
    )

    feature_source <- split(
      igraph::V(graph = g) |>
        names(),
      igraph::components(graph = g)$membership
    )

    clusters_ready <- feature_source |>
      rbind() |>
      t() |>
      data.frame() |>
      tibble::rownames_to_column("ComponentIndex") |>
      tidyr::unnest(cols = c(feature_source)) |>
      dplyr::distinct(
        `cluster index` = feature_source,
        ComponentIndex
      ) |>
      dplyr::mutate_all(as.numeric) |>
      dplyr::arrange(`cluster index`)

    # Export
    log_debug(x = "Exporting ...")
    export_params(step = "create_components")
    export_output(
      x = clusters_ready,
      file = output
    )

    return(output)
  }
