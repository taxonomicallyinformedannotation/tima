EDGE_WEIGHT_COLUMNS <- c(
  "candidate_score_similarity",
  "score",
  "weight",
  "edge_weight",
  "edge_score",
  "similarity",
  "cosine",
  "spectral_similarity",
  "ms2_similarity"
)

EDGE_MATCHED_PEAKS_COLUMNS <- c(
  "candidate_count_similarity_peaks_matched",
  "matched_peaks",
  "n_matched_peaks",
  "matched_peak_count"
)

.resolve_edge_weights <- function(edges) {
  edge_weights <- rep(NA_real_, nrow(edges))

  weight_col <- intersect(EDGE_WEIGHT_COLUMNS, names(edges))

  if (length(weight_col) > 0L) {
    edge_weights <- suppressWarnings(as.numeric(edges[[weight_col[[1L]]]]))
  }

  matched_col <- intersect(EDGE_MATCHED_PEAKS_COLUMNS, names(edges))
  matched_factor <- rep(1, nrow(edges))

  if (length(matched_col) > 0L) {
    matched_values <- suppressWarnings(as.numeric(edges[[matched_col[[1L]]]]))
    matched_values[!is.finite(matched_values)] <- 1
    matched_values[matched_values < 1] <- 1

    peak_columns <- c(
      "feature_spectrum_peaks",
      "source_feature_spectrum_peaks",
      "target_feature_spectrum_peaks"
    )
    peak_col <- intersect(peak_columns, names(edges))
    denominator <- rep(6, nrow(edges))

    if (length(peak_col) > 0L) {
      peak_values <- suppressWarnings(as.numeric(edges[[peak_col[[1L]]]]))
      peak_values[!is.finite(peak_values)] <- 6
      peak_values[peak_values < 1] <- 6
      denominator <- peak_values
    }

    matched_fraction <- matched_values / denominator
    matched_factor <- pmin(1, pmax(1e-6, matched_fraction))
  }

  edge_weights[!is.finite(edge_weights)] <- NA_real_
  edge_weights[is.na(edge_weights)] <- 1
  edge_weights <- pmin(1, pmax(1e-6, edge_weights))
  # Combine spectral similarity with matched-fragment evidence: the similarity
  # score provides the main signal, while the matched-peak fraction acts as a
  # secondary evidence term that strongly penalizes low-count links.
  # composite_weight = (similarity * matched_fraction)^2
  edge_weights <- edge_weights * matched_factor
  edge_weights <- pmin(1, pmax(1e-6, edge_weights))
  edge_weights <- edge_weights^2
  edge_weights
}

.prune_intra_community_edges <- function(
  edges,
  component_membership,
  name_source,
  name_target
) {
  if (nrow(edges) == 0L || length(component_membership) == 0L) {
    return(edges)
  }

  cm_names <- names(component_membership)
  cm_vals <- as.character(unname(component_membership))
  edge_source_community <- cm_vals[
    fastmatch::fmatch(edges[[name_source]], cm_names)
  ]
  edge_target_community <- cm_vals[
    fastmatch::fmatch(edges[[name_target]], cm_names)
  ]
  attr(cm_names, ".match.hash") <- NULL

  same_community <- !is.na(edge_source_community) &
    !is.na(edge_target_community) &
    nzchar(edge_source_community, keepNA = FALSE) &
    nzchar(edge_target_community, keepNA = FALSE) &
    edge_source_community == edge_target_community

  intra_edges <- edges[same_community, , drop = FALSE]
  if (nrow(intra_edges) <= 2L) {
    return(intra_edges)
  }

  edge_weights <- .resolve_edge_weights(intra_edges)
  if (!any(is.finite(edge_weights)) || all(edge_weights <= 1e-6)) {
    return(intra_edges[seq_len(min(2L, nrow(intra_edges))), , drop = FALSE])
  }

  min_strength <- max(
    0.05,
    stats::quantile(
      edge_weights,
      probs = 0.5,
      na.rm = TRUE,
      names = FALSE
    )
  )
  cutoff <- stats::quantile(
    edge_weights,
    probs = 0.75,
    na.rm = TRUE,
    names = FALSE
  )
  keep_edge <- edge_weights >= pmax(cutoff, min_strength)
  if (!any(keep_edge)) {
    keep_edge[which.max(edge_weights)] <- TRUE
  }

  intra_edges[keep_edge, , drop = FALSE]
}

.build_components_from_edges <- function(
  edges,
  name_source = "feature_source",
  name_target = "feature_target",
  resolution = 0.1,
  n_iterations = 2L,
  seed = NULL,
  label_column = "componentindex",
  cut_to_communities = FALSE,
  populate_edges = TRUE
) {
  if (nrow(edges) == 0L) {
    return(list(
      components_table = tidytable::tidytable(
        `cluster index` = character(0),
        componentindex = character(0)
      ),
      method_used = "no edges",
      edges = edges
    ))
  }

  if (label_column %in% names(edges) && !all(is.na(edges[[label_column]]))) {
    all_features <- unique(c(edges[[name_source]], edges[[name_target]]))
    component_membership <- stats::setNames(
      rep(NA_character_, length(all_features)),
      all_features
    )

    edge_label_values <- as.character(edges[[label_column]])
    edge_label_values[
      is.na(edge_label_values) | !nzchar(edge_label_values)
    ] <- NA_character_

    source_features <- as.character(edges[[name_source]])
    target_features <- as.character(edges[[name_target]])

    feature_rows <- data.frame(
      feature = c(source_features, target_features),
      label = c(edge_label_values, edge_label_values),
      # Rows are all sources, then all targets
      edge_order = rep(seq_len(nrow(edges)), times = 2L),
      side_order = rep(c(1L, 2L), each = nrow(edges)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    feature_rows <- feature_rows[
      !is.na(feature_rows$feature) & nzchar(feature_rows$feature),
      ,
      drop = FALSE
    ]
    feature_rows <- feature_rows[
      !is.na(feature_rows$label) & nzchar(feature_rows$label),
      ,
      drop = FALSE
    ]
    if (nrow(feature_rows) > 0L) {
      feature_rows <- feature_rows[
        order(feature_rows$edge_order, feature_rows$side_order),
        ,
        drop = FALSE
      ]
      feature_rows <- feature_rows[
        !duplicated(feature_rows$feature),
        ,
        drop = FALSE
      ]
      component_membership[feature_rows$feature] <- feature_rows$label
    }

    component_membership <- component_membership[!is.na(component_membership)]
    method_used <- "edge labels"
  } else {
    src_col <- as.character(edges[[name_source]])
    tgt_col <- as.character(edges[[name_target]])
    all_features <- unique(c(src_col, tgt_col))
    all_features <- all_features[!is.na(all_features)]

    edge_weights <- .resolve_edge_weights(edges)
    edge_weights <- pmax(edge_weights, 1e-6)

    if (nrow(edges) > 200000L) {
      q_cut <- stats::quantile(
        edge_weights,
        probs = max(0, 1 - 200000 / nrow(edges)),
        na.rm = TRUE,
        names = FALSE
      )
      keep_g <- which(
        !is.na(src_col) &
          !is.na(tgt_col) &
          src_col != tgt_col &
          edge_weights >= q_cut
      )
      if (length(keep_g) == 0L) {
        keep_g <- which(!is.na(src_col) & !is.na(tgt_col))
      }
      src_id <- fastmatch::fmatch(src_col[keep_g], all_features)
      tgt_id <- fastmatch::fmatch(tgt_col[keep_g], all_features)
      edge_weights <- edge_weights[keep_g]
    } else {
      valid_e <- !is.na(src_col) & !is.na(tgt_col)
      src_id <- fastmatch::fmatch(src_col[valid_e], all_features)
      tgt_id <- fastmatch::fmatch(tgt_col[valid_e], all_features)
      edge_weights <- edge_weights[valid_e]
    }

    attr(all_features, ".match.hash") <- NULL
    network_graph <- igraph::make_graph(
      edges = as.vector(rbind(src_id, tgt_id)),
      n = length(all_features),
      directed = FALSE
    )
    igraph::V(network_graph)$name <- all_features

    if (length(edge_weights) > 0L) {
      weight_min <- min(edge_weights, na.rm = TRUE)
      weight_max <- max(edge_weights, na.rm = TRUE)
      if (weight_max > weight_min) {
        edge_weights <- (edge_weights - weight_min) / (weight_max - weight_min)
        edge_weights <- edge_weights * 0.99 + 0.01
      } else {
        edge_weights <- rep(1, length(edge_weights))
      }
      igraph::E(network_graph)$weight <- edge_weights
    }

    if (igraph::ecount(network_graph) > 0L) {
      resolution_grid <- if (nrow(edges) > 200000L) {
        unique(c(resolution, 1))
      } else {
        unique(c(
          resolution,
          resolution / 2,
          resolution / 4,
          resolution * 2,
          resolution * 4,
          0.025,
          0.05,
          0.1,
          0.2,
          0.5,
          1
        ))
      }
      resolution_grid <- resolution_grid[
        is.finite(resolution_grid) & resolution_grid > 0
      ]
      resolution_grid <- sort(unique(round(resolution_grid, 6)))
      eff_iterations <- if (nrow(edges) > 200000L) 1L else n_iterations

      detect_communities <- function(resolution_value) {
        # Re-seed per call so each resolution is reproducible on its own
        if (!is.null(seed)) {
          set.seed(seed)
        }
        tryCatch(
          {
            if ("cluster_leiden" %in% getNamespaceExports("igraph")) {
              # Optimise modularity, the same criterion used below to pick
              # the resolution (igraph's default objective is CPM)
              igraph::cluster_leiden(
                network_graph,
                objective_function = "modularity",
                weights = igraph::E(network_graph)$weight,
                resolution = resolution_value,
                n_iterations = eff_iterations
              )
            } else {
              igraph::cluster_louvain(
                network_graph,
                weights = igraph::E(network_graph)$weight,
                resolution = resolution_value
              )
            }
          },
          error = function(e) {
            log_warn(
              "Weighted community detection failed at resolution %.3f (%s)",
              resolution_value,
              conditionMessage(e)
            )
            NULL
          }
        )
      }

      best_partition <- NULL
      best_modularity <- -Inf
      best_resolution <- resolution

      for (resolution_value in resolution_grid) {
        community_detection <- detect_communities(resolution_value)
        if (is.null(community_detection)) {
          next
        }

        membership <- igraph::membership(community_detection)
        modularity_value <- igraph::modularity(
          network_graph,
          membership = membership,
          weights = igraph::E(network_graph)$weight
        )

        if (
          is.finite(modularity_value) &&
            modularity_value > best_modularity + 1e-8
        ) {
          best_partition <- community_detection
          best_modularity <- modularity_value
          best_resolution <- resolution_value
        }
      }

      if (!is.null(best_partition)) {
        community_detection <- best_partition
        if (best_resolution != resolution) {
          log_info(
            "Selected community partition at resolution %.3f (modularity %.3f)",
            best_resolution,
            best_modularity
          )
        }
      } else {
        community_detection <- NULL
      }
    } else {
      community_detection <- NULL
    }

    if (is.null(community_detection)) {
      connected_components <- igraph::components(network_graph, mode = "weak")
      component_membership <- as.integer(connected_components$membership)
      names(component_membership) <- names(connected_components$membership)
      method_used <- "connected components"
    } else {
      component_membership <- as.integer(igraph::membership(
        community_detection
      ))
      names(component_membership) <- igraph::V(network_graph)$name
      method_used <- "weighted Louvain/Leiden clustering"
    }

    if (is.null(names(component_membership))) {
      names(component_membership) <- all_features
    }

    component_membership_vector <- as.integer(component_membership)
    names(component_membership_vector) <- names(component_membership)
    component_membership <- component_membership_vector[order(names(
      component_membership_vector
    ))]
    component_membership <- stats::setNames(
      as.character(component_membership),
      names(component_membership)
    )
  }

  if (isTRUE(populate_edges) || isTRUE(cut_to_communities)) {
    if (length(component_membership) > 0L) {
      cm_names <- names(component_membership)
      cm_vals <- as.character(unname(component_membership))
      edge_membership_source <- cm_vals[
        fastmatch::fmatch(edges[[name_source]], cm_names)
      ]
      edge_membership_target <- cm_vals[
        fastmatch::fmatch(edges[[name_target]], cm_names)
      ]
      attr(names(component_membership), ".match.hash") <- NULL

      if (isTRUE(cut_to_communities)) {
        keep_edge <- !is.na(edge_membership_source) &
          !is.na(edge_membership_target) &
          nzchar(edge_membership_source) &
          nzchar(edge_membership_target) &
          edge_membership_source == edge_membership_target
        edges <- edges[keep_edge, , drop = FALSE]
      } else {
        edges[[label_column]] <- edge_membership_source
      }
    } else {
      edges[[label_column]] <- character(nrow(edges))
    }
  }

  components_table <- data.frame(
    "cluster index" = names(component_membership),
    componentindex = as.character(component_membership),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  components_table <- components_table[
    order(components_table[["cluster index"]]),
    ,
    drop = FALSE
  ]

  list(
    components_table = components_table,
    method_used = method_used,
    edges = edges,
    component_membership = component_membership
  )
}

#' @title Build components from edges
#'
#' @description Public wrapper around the internal community-detection logic
#'     used to build components from edge lists.
#'
#' @param edges A data frame-like object with source/target edge columns and
#'     an optional weight column.
#' @param name_source Column name containing source feature identifiers.
#' @param name_target Column name containing target feature identifiers.
#' @param resolution Resolution parameter for weighted community detection.
#' @param n_iterations Number of iterations for the community-detection
#'     algorithm.
#' @param seed Random seed for reproducible clustering.
#' @param label_column Column name used for pre-labelled components.
#' @param cut_to_communities Whether to retain only intra-community edges.
#'
#' @return A list with the community table, detected method, filtered edges,
#'     and component membership.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an edge data frame
#' edges <- data.frame(
#'   feature_source = c("f1", "f1", "f2"),
#'   feature_target = c("f2", "f3", "f3"),
#'   weight = c(0.8, 0.6, 0.9),
#'   stringsAsFactors = FALSE
#' )
#' result <- build_components_from_edges(edges)
#' result$components_table
#' }
build_components_from_edges <- function(
  edges,
  name_source = "feature_source",
  name_target = "feature_target",
  resolution = 0.1,
  n_iterations = 2L,
  seed = NULL,
  label_column = "componentindex",
  cut_to_communities = FALSE
) {
  .build_components_from_edges(
    edges = edges,
    name_source = name_source,
    name_target = name_target,
    resolution = resolution,
    n_iterations = n_iterations,
    seed = seed,
    label_column = label_column,
    cut_to_communities = cut_to_communities
  )
}

#' @title Create components
#'
#' @description This function creates network components (connected subgraphs)
#'     from edge lists using igraph. Each component represents a set of
#'     features that are connected through spectral similarity or other
#'     relationships.
#'
#' @include get_params.R
#' @include logs_utils.R
#' @include safe_fread.R
#' @include export_params.R
#' @include export_output.R
#'
#' @param input Character vector of file path(s) containing edge data.
#'     Files should have feature_source and feature_target columns.
#' @param output Character string path for the output components file
#'
#' @return Character string path to the created components file
#'
#' @family workflow
#' @concept EDAM:operation_3927 Network analysis
#' @concept EDAM:operation_3766 Weighted correlation network analysis
#' @concept EDAM:data_2600 Pathway or network
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
#'   export = get_params(
#'     step =
#'       "create_components"
#'   )$files$networks$spectral$edges$prepared
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

  input_exists <- vapply(
    X = input,
    FUN = file.exists,
    logical(1L),
    USE.NAMES = FALSE
  )

  if (!all(input_exists)) {
    missing_files <- input[!input_exists]
    cli::cli_abort(
      c(
        "input file(s) not found",
        "x" = paste(missing_files, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Creating components from %d edge file(s)", length(input))

  edges <- Map(
    function(x, y) {
      safe_fread(
        file = x,
        file_type = paste0("edge file ", y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    input,
    seq_along(input)
  ) |>
    tidytable::bind_rows()

  # Keep the columns .resolve_edge_weights() reads, so that community
  # detection is weighted by spectral similarity (and matched peaks)
  selected_edge_columns <- c(
    "feature_source",
    "feature_target",
    "componentindex",
    utils::head(intersect(EDGE_WEIGHT_COLUMNS, names(edges)), 1L),
    utils::head(intersect(EDGE_MATCHED_PEAKS_COLUMNS, names(edges)), 1L)
  )

  edges <- edges |>
    tidytable::select(tidyselect::any_of(selected_edge_columns)) |>
    tidytable::filter(!is.na(feature_source), !is.na(feature_target)) |>
    tidytable::mutate(
      feature_source = as.character(feature_source),
      feature_target = as.character(feature_target)
    ) |>
    tidytable::filter(feature_source != feature_target) |>
    tidytable::distinct()

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

  if (n_edges == 0L) {
    log_warn("No edges found, creating empty components file")
    components_table <- tidytable::tidytable(
      `cluster index` = character(0),
      componentindex = character(0)
    )
    export_output(x = components_table, file = output)
    return(output)
  }

  community_result <- .build_components_from_edges(
    edges = edges,
    name_source = "feature_source",
    name_target = "feature_target",
    seed = 42L
  )
  components_table <- community_result$components_table
  method_used <- community_result$method_used

  log_info(
    "Found %d communities using %s",
    length(unique(components_table$componentindex)),
    method_used
  )

  component_sizes <- table(components_table$componentindex)
  log_info(
    "Component sizes - Min: %d, Max: %d, Mean: %s",
    min(component_sizes),
    max(component_sizes),
    round(mean(component_sizes), 1)
  )

  export_params(
    parameters = get_params(step = "create_components"),
    step = "create_components"
  )

  export_output(x = components_table, file = output)

  n_components <- length(unique(components_table$componentindex))
  n_features <- nrow(components_table)
  rm(components_table)
  log_info("Components written to: %s", output)

  log_complete(
    ctx,
    n_components = n_components,
    n_features = n_features
  )

  output
}
