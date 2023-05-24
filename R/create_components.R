utils::globalVariables(
  c(
    "cluster index",
    "ComponentIndex",
    "feature_target"
  )
)

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
  function(input = params$files$networks$spectral$edges$prepared,
           output = params$files$networks$spectral$components$raw,
           parameters = params) {
    stopifnot(
      "Your input file(s) do(es) not exist" =
        rep(TRUE, length(unlist(input))) ==
          lapply(X = unlist(input), file.exists)
    )
    params <<- parameters

    edges <- lapply(
      X = input,
      FUN = tidytable::fread,
      na.strings = ""
    ) |>
      dplyr::bind_rows() |>
      dplyr::select(feature_source, feature_target) |>
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
      tidyfst::rn_col("ComponentIndex") |>
      tidytable::unnest(feature_source) |>
      dplyr::distinct(
        `cluster index` = feature_source,
        componentindex = ComponentIndex
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |>
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
