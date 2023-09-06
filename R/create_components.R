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
          lapply(X = unlist(input), FUN = file.exists)
    )
    params <- parameters

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

    feature_source <- g |>
      igraph::V() |>
      names() |>
      split(igraph::components(graph = g)$membership)

    clusters_ready <- feature_source |>
      rbind() |>
      t() |>
      data.frame() |>
      tidyfst::rn_col("ComponentIndex") |>
      tidytable::unnest(feature_source) |>
      tidytable::distinct(
        `cluster index` = feature_source,
        componentindex = ComponentIndex
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = as.numeric
        )
      ) |>
      tidytable::arrange(`cluster index`)

    log_debug(x = "Exporting ...")
    export_params(step = "create_components")
    export_output(
      x = clusters_ready,
      file = output
    )

    return(output)
  }
