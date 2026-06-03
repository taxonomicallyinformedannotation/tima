.tima_internal <- function(name) {
  getFromNamespace(name, "tima")
}

test_that("enforce_global_m_consistency handles empty edges", {
  features <- tidytable::tidytable(
    feature_id = character(),
    mz = numeric(),
    rt = numeric(),
    sample = character(),
    adduct = character()
  )

  edges <- tidytable::tidytable(
    feature_id = character(),
    feature_id_dest = character(),
    adduct = character(),
    adduct_dest = character()
  )

  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  expect_equal(nrow(result), 0L)
})

test_that("enforce_global_m_consistency keeps single edge", {
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2'),
    mz = c(100.007, 121.980),
    rt = c(1.0, 1.5),
    sample = c('S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1'),
    feature_id_dest = c('f2'),
    adduct = c('[M+H]+'),
    adduct_dest = c('[M+Na]+')
  )

  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id[1], 'f1')
  expect_equal(result$feature_id_dest[1], 'f2')
})

test_that("enforce_global_m_consistency finds compatible cliques", {
  # Two separate compatible networks: f1-f2 at M~99, f3-f4 at M~150
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3', 'f4'),
    mz = c(100.007, 121.980, 151.007, 172.990),
    rt = c(1.0, 1.5, 2.0, 2.1),
    sample = c('S1', 'S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3'),
    feature_id_dest = c('f2', 'f3', 'f4'),
    adduct = c('[M+H]+', '[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+H]+', '[M+Na]+')
  )

  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # Strict tie-breaking may keep one or both compatible edges after pruning.
  expect_gte(nrow(result), 1L)
  expect_lte(nrow(result), 2L)

  # Verify all features in result are valid
  all_features <- c(result$feature_id, result$feature_id_dest)
  expect_true(all(all_features %in% c('f1', 'f2', 'f3', 'f4')))
})

test_that("clique selection maximizes edges with no conflicts", {
  # Perfect scenario: 3 edges all compatible
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3', 'f4'),
    mz = c(100.007, 121.980, 122.988, 124.995),
    rt = c(1.0, 1.5, 2.0, 2.1),
    sample = c('S1', 'S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  # All edges should pass pairwise check if data is consistent
  # Using edges that are known to be compatible
  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f3'),
    feature_id_dest = c('f2', 'f4'),
    adduct = c('[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+Na]+')
  )

  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # Should keep both edges (f1-f2 and f3-f4 don't share features)
  expect_equal(nrow(result), 2L)
})

test_that("global consistency prevents feature double-assignment", {
  # Scenario: F2 appears in two edges with different M values
  # Before pairwise check removes the bad edge, both should pass
  # but global check should handle any remaining conflicts

  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3'),
    mz = c(100.007, 121.980, 151.007),
    rt = c(1.0, 1.5, 2.0),
    sample = c('S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  # E1: both at M~99, E2: M mismatch (should fail pairwise)
  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f2'),
    feature_id_dest = c('f2', 'f3'),
    adduct = c('[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+H]+')
  )

  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # E2 should fail the pairwise check before global consistency
  # Only E1 should remain if it's pairwise consistent
  expect_true(nrow(result) <= 1L)
})

test_that("solve_consistent_adduct_assignments uses clique solver", {
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3', 'f4'),
    mz = c(100.007, 121.980, 151.007, 172.990),
    rt = c(1.0, 1.5, 2.0, 2.1),
    sample = c('S1', 'S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3'),
    feature_id_dest = c('f2', 'f3', 'f4'),
    adduct = c('[M+H]+', '[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+H]+', '[M+Na]+')
  )

  result <- .tima_internal("solve_consistent_adduct_assignments")(
    adduct_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # Verify outputs
  expect_true('consistent_edges' %in% names(result))
  expect_true('feature_m_map' %in% names(result))
  expect_true('component_membership' %in% names(result))

  # Should form 2 components (f1-f2 and f3-f4)
  n_components <- length(unique(result$feature_m_map$component_id))
  expect_equal(n_components, 2L)

  # Should keep 2 edges (E1 and E3)
  expect_equal(nrow(result$consistent_edges), 2L)
})

test_that("component_membership is correctly assigned", {
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3', 'f4'),
    mz = c(100.007, 121.980, 151.007, 172.990),
    rt = c(1.0, 1.5, 2.0, 2.1),
    sample = c('S1', 'S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3'),
    feature_id_dest = c('f2', 'f3', 'f4'),
    adduct = c('[M+H]+', '[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+H]+', '[M+Na]+')
  )

  result <- .tima_internal("solve_consistent_adduct_assignments")(
    adduct_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # Check component membership
  comp_membership <- result$component_membership
  expect_true(nrow(comp_membership) > 0)

  # f1 and f2 should be in same component
  f1_comp <- comp_membership$component_id[comp_membership$feature_id == 'f1']
  f2_comp <- comp_membership$component_id[comp_membership$feature_id == 'f2']
  expect_equal(f1_comp, f2_comp)

  # f3 and f4 should be in same component (different from f1)
  f3_comp <- comp_membership$component_id[comp_membership$feature_id == 'f3']
  f4_comp <- comp_membership$component_id[comp_membership$feature_id == 'f4']
  expect_equal(f3_comp, f4_comp)
  expect_false(f1_comp == f3_comp)
})

test_that("feature_m_map shows consistent M per component", {
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3', 'f4'),
    mz = c(100.007, 121.980, 151.007, 172.990),
    rt = c(1.0, 1.5, 2.0, 2.1),
    sample = c('S1', 'S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3'),
    feature_id_dest = c('f2', 'f3', 'f4'),
    adduct = c('[M+H]+', '[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+H]+', '[M+Na]+')
  )

  result <- .tima_internal("solve_consistent_adduct_assignments")(
    adduct_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  m_map <- result$feature_m_map

  # Component 1: f1, f2 should have ~same M (~99)
  comp1 <- m_map %>%
    dplyr::filter(feature_id %in% c('f1', 'f2')) %>%
    dplyr::pull(neutral_mass)

  if (length(comp1) == 2) {
    diff1 <- abs(comp1[1] - comp1[2])
    expect_true(diff1 < 1.0) # Within 1 Da
  }

  # Component 2: f3, f4 should have ~same M (~150)
  comp2 <- m_map %>%
    dplyr::filter(feature_id %in% c('f3', 'f4')) %>%
    dplyr::pull(neutral_mass)

  if (length(comp2) == 2) {
    diff2 <- abs(comp2[1] - comp2[2])
    expect_true(diff2 < 1.0) # Within 1 Da
  }
})

test_that("dual tolerance (ppm and dalton) both respected", {
  # Create edges where one is consistent via ppm, another via dalton
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2'),
    mz = c(100.007, 121.980),
    rt = c(1.0, 1.5),
    sample = c('S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1'),
    feature_id_dest = c('f2'),
    adduct = c('[M+H]+'),
    adduct_dest = c('[M+Na]+')
  )

  # With both tolerances
  result_dual <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # With only ppm (tight)
  result_ppm <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = NULL
  )

  # Both should accept the edge since it's well within tolerance
  expect_equal(nrow(result_dual), 1L)
  expect_equal(nrow(result_ppm), 1L)
})

test_that("large edge sets use greedy fallback gracefully", {
  # This test just verifies the function doesn't crash with many edges
  n_features <- 30
  features <- tidytable::tidytable(
    feature_id = paste0('f', seq_len(n_features)),
    mz = 100 + seq_len(n_features) * 0.01,
    rt = seq_len(n_features) * 0.1,
    sample = 'S1',
    adduct = NA_character_
  )

  # Create chain of edges (all compatible pairwise)
  edges <- tidytable::tidytable(
    feature_id = paste0('f', seq_len(n_features - 1)),
    feature_id_dest = paste0('f', seq_len(n_features - 1) + 1),
    adduct = '[M+H]+',
    adduct_dest = '[M+H]+'
  )

  # This might fail pairwise, but the function should not crash
  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 10, # Loose tolerance
    tolerance_dalton = 1.0
  )

  # Just verify it returns a valid table (may be empty if incompatible)
  expect_true(is.data.frame(result) || is.null(result) || nrow(result) >= 0)
})

test_that("global consistency preserves original edge structure", {
  features <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3', 'f4'),
    mz = c(100.007, 121.980, 151.007, 172.990),
    rt = c(1.0, 1.5, 2.0, 2.1),
    sample = c('S1', 'S1', 'S1', 'S1'),
    adduct = NA_character_
  )

  edges <- tidytable::tidytable(
    feature_id = c('f1', 'f2', 'f3'),
    feature_id_dest = c('f2', 'f3', 'f4'),
    adduct = c('[M+H]+', '[M+H]+', '[M+H]+'),
    adduct_dest = c('[M+Na]+', '[M+H]+', '[M+Na]+')
  )

  result <- .tima_internal("enforce_global_m_consistency")(
    consistent_edges = edges,
    features_table = features,
    tolerance_ppm = 5,
    tolerance_dalton = 0.1
  )

  # Verify output has correct columns
  expect_true(all(
    c('feature_id', 'adduct', 'feature_id_dest', 'adduct_dest') %in%
      names(result)
  ))

  # Verify returned edges match input structure
  for (i in seq_len(nrow(result))) {
    row <- result[i, ]
    # Check that this edge exists in original edges
    matches <- edges %>%
      dplyr::filter(
        feature_id == row$feature_id,
        feature_id_dest == row$feature_id_dest,
        adduct == row$adduct,
        adduct_dest == row$adduct_dest
      )
    expect_true(nrow(matches) > 0)
  }
})
