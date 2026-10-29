# Test Suite: create_components ----

library(testthat)

## Validation ----

test_that("test-create_components validates input files exist", {
  output <- temp_test_path("components.tsv")

  expect_error(
    create_components(
      input = temp_test_path("missing.tsv"),
      output = output
    ),
    "input file\\(s\\) not found"
  )
})

test_that("test-create_components validates multiple input files", {
  file1 <- temp_test_path("edges1.tsv")
  tidytable::fwrite(
    tidytable::tidytable(feature_source = "F1", feature_target = "F2"),
    file1,
    sep = "\t"
  )

  output <- temp_test_path("components.tsv")

  expect_error(
    create_components(
      input = c(file1, temp_test_path("missing.tsv")),
      output = output
    ),
    "input file\\(s\\) not found"
  )
})

## Behavior ----

test_that("test-create_components processes minimal edge data", {
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F2", "F3"),
    feature_target = c("F2", "F3", "F4")
  )
  input <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = input, sep = "\t")

  output <- temp_test_path("components.tsv")

  res <- create_components(input = input, output = output)

  expect_equal(res, output)
  expect_true(file.exists(output))

  components <- tidytable::fread(output)
  expect_true("cluster index" %in% names(components))
  expect_true("componentindex" %in% names(components))
})

test_that("test-create_components handles empty edges", {
  edges <- tidytable::tidytable(
    feature_source = character(),
    feature_target = character()
  )
  input <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = input, sep = "\t")

  output <- temp_test_path("components.tsv")

  res <- create_components(input = input, output = output)

  expect_true(file.exists(output))
  components <- tidytable::fread(output)
  expect_equal(nrow(components), 0)
})

test_that("test-create_components combines multiple edge files", {
  edges1 <- tidytable::tidytable(
    feature_source = c("F1"),
    feature_target = c("F2")
  )
  input1 <- temp_test_path("edges1.tsv")
  tidytable::fwrite(x = edges1, file = input1, sep = "\t")

  edges2 <- tidytable::tidytable(
    feature_source = c("F3"),
    feature_target = c("F4")
  )
  input2 <- temp_test_path("edges2.tsv")
  tidytable::fwrite(x = edges2, file = input2, sep = "\t")

  output <- temp_test_path("components.tsv")

  res <- create_components(input = c(input1, input2), output = output)

  expect_true(file.exists(output))
  components <- tidytable::fread(output)
  expect_true(nrow(components) >= 2)
})

test_that("test-create_components handles disconnected components", {
  # Create two separate components
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F3"),
    feature_target = c("F2", "F4")
  )
  input <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = input, sep = "\t")

  output <- temp_test_path("components.tsv")

  res <- create_components(input = input, output = output)

  expect_true(file.exists(output))
  components <- tidytable::fread(output)

  # Should have at least 1 component
  n_components <- length(unique(components$componentindex))
  expect_true(n_components >= 1)
})

test_that("build_components_from_edges uses canonical similarity weights", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "A", "C"),
    feature_target = c("B", "C", "D"),
    candidate_score_similarity = c(0.95, 0.1, 0.9),
    candidate_count_similarity_peaks_matched = c(8, 1, 7)
  )

  result <- build_components_from_edges(
    edges = edges,
    name_source = "feature_source",
    name_target = "feature_target",
    resolution = 0.1,
    n_iterations = 10L,
    seed = 1,
    label_column = "componentindex",
    cut_to_communities = TRUE
  )

  expect_equal(nrow(result$edges), 2)
  expect_true(all(
    c("A", "B") %in%
      unlist(result$edges[, c("feature_source", "feature_target")])
  ))
  expect_true(all(
    c("C", "D") %in%
      unlist(result$edges[, c("feature_source", "feature_target")])
  ))
})

test_that("test-create_components filters NA values", {
  edges <- tidytable::tidytable(
    feature_source = c("F1", NA, "F3"),
    feature_target = c("F2", "F2", "F4")
  )
  input <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = input, sep = "\t")

  output <- temp_test_path("components.tsv")

  res <- create_components(input = input, output = output)

  expect_true(file.exists(output))
  components <- tidytable::fread(output)
  # Should only contain F1, F2, F3, F4 (NA filtered out)
  expect_false(anyNA(components$`cluster index`))
})

test_that("test-create_components creates distinct edges", {
  # Duplicate edges should be removed
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F2"),
    feature_target = c("F2", "F2", "F3")
  )
  input <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = input, sep = "\t")

  output <- temp_test_path("components.tsv")

  expect_no_error(create_components(input = input, output = output))
})

## Edge weights ----

test_that(".resolve_edge_weights squares the similarity and defaults to 1", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "C", "D"),
    feature_target = c("B", "C", "D", "E"),
    similarity = c("0.5", NA, "not a number", "-3")
  )

  weights <- .resolve_edge_weights(edges)

  # 0.5^2; missing or unparsable similarity counts as full support;
  # negative similarity is clamped to the 1e-6 floor before squaring
  expect_equal(weights, c(0.25, 1, 1, 1e-12))
})

test_that(".resolve_edge_weights uses the first column of EDGE_WEIGHT_COLUMNS", {
  edges <- tidytable::tidytable(
    feature_source = "A",
    feature_target = "B",
    score = 0.2,
    candidate_score_similarity = 0.8
  )

  # candidate_score_similarity precedes score in EDGE_WEIGHT_COLUMNS
  expect_equal(.resolve_edge_weights(edges), 0.8^2)
})

test_that(".resolve_edge_weights penalizes few matched peaks per spectrum peak", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "C", "D"),
    feature_target = c("B", "C", "D", "E"),
    similarity = c(0.8, 0.8, 0.8, 0.8),
    matched_peaks = c(3, 3, 12, 3),
    feature_spectrum_peaks = c("6", NA, "6", "0")
  )

  weights <- .resolve_edge_weights(edges)

  # 3/6 matched; NA and 0 peak counts fall back to a denominator of 6;
  # a matched fraction above 1 is capped at 1
  expect_equal(weights, c((0.8 * 0.5)^2, (0.8 * 0.5)^2, 0.8^2, (0.8 * 0.5)^2))
})

test_that(".resolve_edge_weights divides matched peaks by spectrum peaks", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B"),
    feature_target = c("B", "C"),
    similarity = c(1, 1),
    matched_peaks = c(5, 5),
    feature_spectrum_peaks = c(10, 20)
  )

  expect_equal(.resolve_edge_weights(edges), c(0.5^2, 0.25^2))
})

## Intra-community pruning ----

test_that(".prune_intra_community_edges returns edges unchanged without membership", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B"),
    feature_target = c("B", "C")
  )

  out <- .prune_intra_community_edges(
    edges = edges,
    component_membership = character(0),
    name_source = "feature_source",
    name_target = "feature_target"
  )

  expect_identical(out, edges)
})

test_that(".prune_intra_community_edges drops cross-community edges", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B"),
    feature_target = c("B", "C"),
    similarity = c(0.1, 0.99)
  )
  membership <- c(A = "1", B = "1", C = "2")

  out <- .prune_intra_community_edges(
    edges = edges,
    component_membership = membership,
    name_source = "feature_source",
    name_target = "feature_target"
  )

  # With two or fewer intra edges nothing is pruned on weight, so the weak
  # A-B edge survives while the strong cross-community B-C edge is dropped
  expect_equal(out$feature_source, "A")
  expect_equal(out$feature_target, "B")
})

test_that(".prune_intra_community_edges keeps only the strongest intra edges", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "A", "C", "E"),
    feature_target = c("B", "C", "C", "D", "F"),
    similarity = c(0.9, 0.5, 0.3, 0.95, 0.99)
  )
  membership <- c(A = "1", B = "1", C = "1", D = "2", E = "3", F = NA)

  out <- .prune_intra_community_edges(
    edges = edges,
    component_membership = membership,
    name_source = "feature_source",
    name_target = "feature_target"
  )

  # Intra weights are 0.81, 0.25, 0.09: only edges at or above the upper
  # quartile (0.53) survive. C-D crosses communities, E-F has NA membership.
  expect_equal(paste(out$feature_source, out$feature_target), "A B")
})

test_that(".prune_intra_community_edges keeps two edges when all are ~zero", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "A"),
    feature_target = c("B", "C", "C"),
    similarity = c(0, 0, 0)
  )
  membership <- c(A = "1", B = "1", C = "1")

  out <- .prune_intra_community_edges(
    edges = edges,
    component_membership = membership,
    name_source = "feature_source",
    name_target = "feature_target"
  )

  expect_equal(paste(out$feature_source, out$feature_target), c("A B", "B C"))
})

## Pre-labelled components ----

test_that("build_components_from_edges takes components from edge labels", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "C", "E", "G"),
    feature_target = c("B", "D", "F", "H"),
    componentindex = c("5", "7", NA, "")
  )

  result <- build_components_from_edges(edges = edges)

  expect_equal(result$method_used, "edge labels")
  # E-F carries a missing label and G-H a blank one: none of them gets a
  # component
  expect_equal(
    result$component_membership[c("A", "B", "C", "D")],
    c(A = "5", B = "5", C = "7", D = "7")
  )
  expect_equal(
    result$components_table[["cluster index"]],
    c("A", "B", "C", "D")
  )
  expect_equal(result$components_table$componentindex, c("5", "5", "7", "7"))
})

test_that("a feature on several labelled edges takes its first edge label", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B"),
    feature_target = c("B", "C"),
    componentindex = c("5", "7")
  )

  result <- build_components_from_edges(edges = edges)

  # B appears first (as target) on the "5" edge, then as source on "7"
  expect_equal(
    result$component_membership[c("A", "B", "C")],
    c(A = "5", B = "5", C = "7")
  )
})

test_that("build_components_from_edges cuts edges spanning labelled communities", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "C", "B", "D"),
    feature_target = c("B", "D", "C", "X"),
    componentindex = c("5", "7", NA, NA)
  )

  result <- build_components_from_edges(
    edges = edges,
    cut_to_communities = TRUE
  )

  # B-C links "5" to "7"; X has no label at all
  expect_equal(
    paste(result$edges$feature_source, result$edges$feature_target),
    c("A B", "C D")
  )
})

test_that("build_components_from_edges yields no components for blank labels", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "C"),
    feature_target = c("B", "D"),
    componentindex = c("", "")
  )

  result <- build_components_from_edges(edges = edges)

  expect_equal(result$method_used, "edge labels")
  expect_length(result$component_membership, 0L)
  expect_equal(nrow(result$components_table), 0L)
  expect_equal(result$edges$componentindex, c("", ""))
})

## Weighted community detection ----

two_triangles_bridged <- function(similarity = NULL) {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "A", "C", "D", "D", "E"),
    feature_target = c("B", "C", "C", "D", "E", "F", "F")
  )
  if (!is.null(similarity)) {
    edges$similarity <- similarity
  }
  edges
}

test_that("unweighted Leiden splits two triangles at their bridge", {
  result <- build_components_from_edges(
    edges = two_triangles_bridged(),
    seed = 42L
  )
  m <- result$component_membership

  expect_equal(result$method_used, "weighted Louvain/Leiden clustering")
  expect_true(m[["A"]] == m[["B"]] && m[["B"]] == m[["C"]])
  expect_true(m[["D"]] == m[["E"]] && m[["E"]] == m[["F"]])
  expect_false(m[["C"]] == m[["D"]])
})

test_that("a heavy bridge edge keeps its two endpoints together", {
  # Same topology, but the bridge C-D is the only strong spectral link
  result <- build_components_from_edges(
    edges = two_triangles_bridged(
      similarity = c(0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1)
    ),
    seed = 42L
  )
  m <- result$component_membership

  expect_equal(m[["C"]], m[["D"]])
  expect_false(m[["A"]] == m[["C"]])
  expect_false(m[["F"]] == m[["D"]])
})

test_that("build_components_from_edges is reproducible for a fixed seed", {
  set.seed(1)
  n <- 40L
  pairs <- t(utils::combn(sprintf("N%02d", seq_len(n)), 2L))
  pairs <- pairs[sample(nrow(pairs), 120L), ]
  edges <- tidytable::tidytable(
    feature_source = pairs[, 1L],
    feature_target = pairs[, 2L],
    similarity = round(stats::runif(nrow(pairs), 0.1, 1), 3)
  )

  first <- build_components_from_edges(edges = edges, seed = 7L)
  # Disturb the global RNG between runs: the seed must still pin the result
  stats::runif(10L)
  second <- build_components_from_edges(edges = edges, seed = 7L)

  expect_identical(first$component_membership, second$component_membership)
  expect_gt(length(unique(first$component_membership)), 1L)
})

test_that("build_components_from_edges falls back to connected components", {
  local_mocked_bindings(
    cluster_leiden = function(...) stop("leiden unavailable"),
    .package = "igraph"
  )
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "D"),
    feature_target = c("B", "C", "E"),
    similarity = c(0.9, 0.8, 0.7)
  )

  result <- build_components_from_edges(edges = edges, seed = 1L)
  m <- result$component_membership

  expect_equal(result$method_used, "connected components")
  expect_true(m[["A"]] == m[["B"]] && m[["B"]] == m[["C"]])
  expect_equal(m[["D"]], m[["E"]])
  expect_false(m[["A"]] == m[["D"]])
  expect_equal(result$edges$componentindex, unname(m[c("A", "B", "D")]))
})

test_that("create_components weights communities by the similarity column", {
  dir <- withr::local_tempdir()
  input <- file.path(dir, "edges.tsv")
  output <- file.path(dir, "components.tsv")
  edges <- two_triangles_bridged(
    similarity = c(0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1)
  )
  # An unrelated column must not interfere with weight selection
  edges$label <- "x"
  tidytable::fwrite(x = edges, file = input, sep = "\t")

  create_components(input = input, output = output)

  components <- tidytable::fread(output, colClasses = "character")
  m <- stats::setNames(components$componentindex, components$`cluster index`)
  expect_setequal(names(m), c("A", "B", "C", "D", "E", "F"))
  # Unweighted, C sits with A and B (see the triangle test above); the
  # similarity column must reach community detection and pull C to D
  expect_equal(m[["C"]], m[["D"]])
  expect_false(m[["A"]] == m[["C"]])
})

test_that("create_components writes the same partition on repeated runs", {
  dir <- withr::local_tempdir()
  input <- file.path(dir, "edges.tsv")
  set.seed(3)
  pairs <- t(utils::combn(sprintf("N%02d", 1:30), 2L))
  pairs <- pairs[sample(nrow(pairs), 80L), ]
  tidytable::fwrite(
    x = tidytable::tidytable(
      feature_source = pairs[, 1L],
      feature_target = pairs[, 2L],
      similarity = round(stats::runif(nrow(pairs), 0.1, 1), 3)
    ),
    file = input,
    sep = "\t"
  )

  out1 <- create_components(input = input, output = file.path(dir, "c1.tsv"))
  out2 <- create_components(input = input, output = file.path(dir, "c2.tsv"))

  expect_identical(
    tidytable::fread(out1, colClasses = "character"),
    tidytable::fread(out2, colClasses = "character")
  )
})
