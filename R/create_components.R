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
  ctx <- log_operation("create_components", n_input_files = length(input))

  # Input Validation ----

  # File existence check
  input_exists <- vapply(
    X = input,
    FUN = file.exists,
    logical(1L),
    USE.NAMES = FALSE
  )

  if (!all(input_exists)) {
    missing_files <- input[!input_exists]
    stop(
      "Input file(s) not found: ",
      paste(missing_files, collapse = ", ")
    )
  }

  # Load Edge Data ----

  log_info("Creating components from %d edge file(s)", length(input))

  # Load and combine all edge files
  # log_trace("Loading edge data")
  edges <- purrr::map2(
    .x = input,
    .y = seq_along(input),
    .f = ~ safe_fread(
      file = .x,
      file_type = paste0("edge file ", .y),
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  ) |>
    tidytable::bind_rows() |>
    tidytable::select(feature_source, feature_target) |>
    tidytable::filter(!is.na(feature_source)) |>
    tidytable::distinct()

  # Calculate statistics
  n_edges <- nrow(edges)
  n_unique_features <- length(unique(c(
    edges$feature_source,
    edges$feature_target
  )))

  log_info(
    "Loaded %d edges connecting %d unique features",
    n_edges,
    n_unique_features
  )

  # Early exit for empty edges
  if (n_edges == 0L) {
    log_warn("No edges found, creating empty components file")
    components_table <- tidytable::tidytable(
      `cluster index` = character(0),
      componentindex = character(0)
    )
    export_output(x = components_table, file = output)
    return(output)
  }

  # Build Graph and Find Components ----

  # Create undirected graph from edges
  # log_trace("Building graph structure")
  network_graph <- igraph::graph_from_data_frame(
    d = edges,
    directed = FALSE
  )

  # Find connected components
  # log_trace("Identifying connected components")
  component_membership <- igraph::components(
    graph = network_graph
  )$membership
  feature_names <- names(igraph::V(graph = network_graph))

  # Organize features by component
  features_by_component <- split(feature_names, component_membership)

  log_info("Found %d components", length(features_by_component))

  # Format Component Assignments ----

  # Convert to tidy format
  # log_trace("Formatting component assignments")

  # Convert list to matrix, then to data frame with rownames as column
  components_matrix <- features_by_component |>
    rbind() |>
    t()

  components_df <- as.data.frame(components_matrix, stringsAsFactors = FALSE)
  components_df$ComponentIndex <- rownames(components_df)
  rownames(components_df) <- NULL

  components_table <- components_df |>
    tidytable::as_tidytable() |>
    tidytable::unnest(features_by_component) |>
    tidytable::distinct(
      `cluster index` = features_by_component,
      componentindex = ComponentIndex
    ) |>
    tidytable::arrange(`cluster index`)

  # Calculate component size statistics
  component_sizes <- table(components_table$componentindex)
  log_info(
    "Component sizes - Min: %d, Max: %d, Mean: %s",
    min(component_sizes),
    max(component_sizes),
    round(mean(component_sizes), 1)
  )

  # Export Results ----

  export_params(
    parameters = get_params(step = "create_components"),
    step = "create_components"
  )

  export_output(x = components_table, file = output)

  log_info("Components written to: %s", output)

  log_complete(
    ctx,
    n_components = length(unique(components_table$componentindex)),
    n_features = nrow(components_table)
  )

  return(output)
}
