#' @title Create components
#'
#' @description This function creates network components (connected subgraphs)
#'     from edge lists using igraph. Each component represents a set of
#'     features that are connected through spectral similarity or other
#'     relationships.
#'
#' @include get_params.R
#'
#' @param input Character vector of file path(s) containing edge data.
#'     Files should have feature_source and feature_target columns.
#' @param output Character string path for the output components file
#'
#' @return Character string path to the created components file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' data_interim <- "data/interim/"
#' dir <- paste0(github, repo, data_interim)
#' get_file(
#'   url = paste0(dir, "features/example_edges.tsv"),
#'   export = get_params(step = "create_components")$files$networks$spectral$edges$prepared
#' )
#' create_components()
#' unlink("data", recursive = TRUE)
#' }
create_components <- function(
  input = get_params(
    step = "create_components"
  )$files$networks$spectral$edges$prepared,
  output = get_params(
    step = "create_components"
  )$files$networks$spectral$components$raw
) {
  # Validate input files exist
  input_exists <- vapply(input, file.exists, logical(1L))

  if (!all(input_exists)) {
    missing_files <- input[!input_exists]
    stop(
      "Input file(s) not found: ",
      paste(missing_files, collapse = ", ")
    )
  }

  logger::log_info("Creating components from ", length(input), " edge file(s)")

  # Load and combine all edge files
  logger::log_trace("Loading edge data")
  edges <- purrr::map(
    .x = input,
    .f = tidytable::fread,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::bind_rows() |>
    tidytable::select(feature_source, feature_target) |>
    tidytable::distinct()

  n_edges <- nrow(edges)
  n_unique_features <- length(unique(c(
    edges$feature_source,
    edges$feature_target
  )))

  logger::log_info(
    "Loaded ",
    n_edges,
    " edges connecting ",
    n_unique_features,
    " unique features"
  )

  # Create undirected graph from edges
  logger::log_trace("Building graph structure")
  network_graph <- igraph::graph_from_data_frame(edges, directed = FALSE)

  # Find connected components
  logger::log_trace("Identifying connected components")
  component_membership <- igraph::components(graph = network_graph)$membership
  feature_names <- names(igraph::V(network_graph))

  # Organize features by component
  features_by_component <- split(feature_names, component_membership)

  logger::log_info("Found ", length(features_by_component), " components")

  # Convert to tidy format
  logger::log_trace("Formatting component assignments")
  components_table <- features_by_component |>
    rbind() |>
    t() |>
    data.frame() |>
    tidyfst::rn_col("ComponentIndex") |>
    tidytable::unnest(features_by_component) |>
    tidytable::distinct(
      `cluster index` = features_by_component,
      componentindex = ComponentIndex
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = as.numeric
    )) |>
    tidytable::arrange(`cluster index`)

  logger::log_info(
    "Component sizes - Min: ",
    min(table(components_table$componentindex)),
    ", Max: ",
    max(table(components_table$componentindex)),
    ", Mean: ",
    round(mean(table(components_table$componentindex)), 1)
  )

  # Export parameters and results
  export_params(
    parameters = get_params(step = "create_components"),
    step = "create_components"
  )

  export_output(x = components_table, file = output)

  logger::log_info("Components written to: ", output)

  return(output)
}
