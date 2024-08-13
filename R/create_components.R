import::from(igraph, components, .into = environment())
import::from(igraph, graph_from_data_frame, .into = environment())
import::from(igraph, V, .into = environment())
import::from(tidyfst, rn_col, .into = environment())
import::from(tidytable, across, .into = environment())
import::from(tidytable, arrange, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, unnest, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Create components
#'
#' @description This function create components from edges
#'
#' @importFrom igraph components
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph V
#' @importFrom tidyfst rn_col
#' @importFrom tidytable across
#' @importFrom tidytable arrange
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable fread
#' @importFrom tidytable mutate
#' @importFrom tidytable select
#' @importFrom tidytable unnest
#' @importFrom tidytable where
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
create_components <-
  function(input = get_params(step = "create_components")$files$networks$spectral$edges$prepared,
           output = get_params(step = "create_components")$files$networks$spectral$components$raw) {
    stopifnot("Your input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |>
      unlist()))

    edges <- input |>
      lapply(
        FUN = fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      bind_rows() |>
      select(feature_source, feature_target) |>
      distinct()

    g <- edges |>
      graph_from_data_frame(directed = FALSE)
    rm(edges)

    feature_source <- g |>
      V() |>
      names() |>
      split(components(graph = g)$membership)
    rm(g)

    clusters_ready <- feature_source |>
      rbind() |>
      t() |>
      data.frame() |>
      rn_col("ComponentIndex") |>
      unnest(feature_source) |>
      distinct(`cluster index` = feature_source, componentindex = ComponentIndex) |>
      mutate(across(.cols = where(is.character), .fns = as.numeric)) |>
      arrange(`cluster index`)
    rm(feature_source)

    export_params(
      parameters = get_params(step = "create_components"),
      step = "create_components"
    )
    export_output(x = clusters_ready, file = output)
    rm(clusters_ready)

    return(output)
  }
