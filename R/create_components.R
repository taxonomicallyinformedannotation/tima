#' @title Create components
#'
#' @description This function create components from edges
#'
#' @importFrom igraph components graph_from_data_frame V
#' @importFrom tidyfst rn_col
#' @importFrom tidytable across arrange bind_rows distinct fread mutate select unnest where
#'
#' @param input Input file(s) containing edges
#' @param output Output file.
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_components <-
  function(input = get_params(step = "create_components")$files$networks$spectral$edges$prepared,
           output = get_params(step = "create_components")$files$networks$spectral$components$raw) {
    stopifnot(
      "Your input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |> unlist())
    )

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
      distinct(
        `cluster index` = feature_source,
        componentindex = ComponentIndex
      ) |>
      mutate(
        across(
          .cols = where(is.character),
          .fns = as.numeric
        )
      ) |>
      arrange(`cluster index`)
    rm(feature_source)

    log_debug(x = "Exporting ...")
    export_params(
      parameters = get_params(step = "create_components"),
      step = "create_components"
    )
    export_output(
      x = clusters_ready,
      file = output
    )
    rm(clusters_ready)

    return(output)
  }
