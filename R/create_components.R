#' @title Create components
#'
#' @description This function create components from edges
#'
#' @include get_params.R
#'
#' @param input Input file(s) containing edges
#' @param output Output file.
#'
#' @return The path to the created components
#'
#' @export
#'
#' @examples
#' go_to_cache(dir = system.file(package = "tima"))
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' data_interim <- "data/interim/"
#' dir <- paste0(github, repo)
#' dir <- paste0(dir, data_interim)
#' get_file(
#'   url = paste0(dir, "features/example_edges.tsv"),
#'   export = get_params(step = "create_components")$files$networks$spectral$edges$prepared
#' )
#' create_components()
#' unlink("data", recursive = TRUE)
create_components <-
  function(input = get_params(step = "create_components")$files$networks$spectral$edges$prepared,
           output = get_params(step = "create_components")$files$networks$spectral$components$raw) {
    stopifnot("Your input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |>
      unlist()))

    edges <- input |>
      lapply(
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::bind_rows() |>
      tidytable::select(feature_source, feature_target) |>
      tidytable::distinct()

    g <- edges |>
      igraph::graph_from_data_frame(directed = FALSE)
    rm(edges)

    feature_source <- g |>
      igraph::V() |>
      names() |>
      split(igraph::components(graph = g)$membership)
    rm(g)

    clusters_ready <- feature_source |>
      rbind() |>
      t() |>
      data.frame() |>
      tidyfst::rn_col("ComponentIndex") |>
      tidytable::unnest(feature_source) |>
      tidytable::distinct(`cluster index` = feature_source, componentindex = ComponentIndex) |>
      tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.character), .fns = as.numeric)) |>
      tidytable::arrange(`cluster index`)
    rm(feature_source)

    export_params(
      parameters = get_params(step = "create_components"),
      step = "create_components"
    )
    export_output(x = clusters_ready, file = output)
    rm(clusters_ready)

    return(output)
  }
