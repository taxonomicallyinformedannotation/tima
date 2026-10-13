# Test Suite: dist_groups and dist_get ----

library(testthat)

# dist_get tests ----

test_that("dist_get calculates distance correctly", {
  # Create simple distance matrix
  x <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)
  d <- as.dist(x)

  # Test various distances
  expect_equal(dist_get(d, 1, 2), 1)
  expect_equal(dist_get(d, 1, 3), 2)
  expect_equal(dist_get(d, 2, 3), 3)
})

test_that("dist_get handles identical indices", {
  x <- matrix(c(0, 1, 1, 0), nrow = 2)
  d <- as.dist(x)

  # Distance to self should be 0
  expect_equal(dist_get(d, 1, 1), 0)
  expect_equal(dist_get(d, 2, 2), 0)
})

test_that("dist_get handles reversed indices", {
  x <- matrix(c(0, 5, 5, 0), nrow = 2)
  d <- as.dist(x)

  # Should be symmetric
  expect_equal(dist_get(d, 1, 2), dist_get(d, 2, 1))
})

test_that("dist_get converts matrix to dist", {
  x <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)

  # Should work with matrix input (not just dist object)
  result <- dist_get(x, 1, 2)
  expect_equal(result, 1)
})

test_that("dist_get validates indices", {
  x <- matrix(c(0, 1, 1, 0), nrow = 2)
  d <- as.dist(x)

  # Out of bounds indices
  expect_warning(
    dist_get(d, 1, 5),
    "out of bounds"
  )

  expect_warning(
    dist_get(d, 0, 1),
    "out of bounds"
  )

  expect_warning(
    dist_get(d, -1, 1),
    "out of bounds"
  )
})

test_that("dist_get returns NA for invalid indices", {
  x <- matrix(c(0, 1, 1, 0), nrow = 2)
  d <- as.dist(x)

  # Should return NA for out of bounds
  expect_warning(result <- dist_get(d, 1, 10))
  expect_true(is.na(result))
})

test_that("dist_get works with larger matrices", {
  # Create 5x5 distance matrix
  set.seed(123)
  x <- matrix(runif(25), nrow = 5)
  x <- (x + t(x)) / 2 # Make symmetric
  diag(x) <- 0
  d <- as.dist(x)

  # Test various positions
  expect_equal(dist_get(d, 1, 2), x[1, 2])
  expect_equal(dist_get(d, 2, 5), x[2, 5])
  expect_equal(dist_get(d, 1, 5), x[1, 5])
})

test_that("dist_get handles vectorized inputs", {
  x <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)
  d <- as.dist(x)

  # Multiple pairs at once
  result <- dist_get(d, c(1, 1, 2), c(2, 3, 3))
  expect_equal(result, c(1, 2, 3))
})

# dist_groups tests ----

test_that("dist_groups calculates between-group distances", {
  # Create simple distance matrix
  x <- matrix(c(0, 1, 5, 1, 0, 6, 5, 6, 0), nrow = 3)
  d <- as.dist(x)

  # Two groups: 1,2 in group A; 3 in group B
  g <- c("A", "A", "B")

  result <- dist_groups(d, g)

  expect_s3_class(result, "data.frame")
  expect_true("Distance" %in% names(result))
  expect_true("Label" %in% names(result))
  expect_true("Item1" %in% names(result))
  expect_true("Item2" %in% names(result))
})

test_that("dist_groups separates within and between group", {
  x <- matrix(c(0, 1, 10, 1, 0, 11, 10, 11, 0), nrow = 3)
  d <- as.dist(x)
  g <- c("A", "A", "B")

  result <- dist_groups(d, g)

  # Should have both "Within" and "Between" labels
  labels <- as.character(result$Label)
  expect_true(any(grepl("Within", labels)))
  expect_true(any(grepl("Between", labels)))
})

test_that("dist_groups handles single group", {
  x <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)
  d <- as.dist(x)
  g <- c("A", "A", "A")

  result <- dist_groups(d, g)

  # Should only have "Within" labels
  labels <- as.character(result$Label)
  expect_true(all(grepl("Within", labels)))
  expect_false(any(grepl("Between", labels)))
})

test_that("dist_groups validates input length", {
  x <- matrix(c(0, 1, 1, 0), nrow = 2)
  d <- as.dist(x)

  # Group vector too short
  expect_error(
    dist_groups(d, c("A")),
    "grouping vector length must match number of observations",
    fixed = TRUE
  )

  # Group vector too long
  expect_error(
    dist_groups(d, c("A", "B", "C")),
    "grouping vector length must match number of observations",
    fixed = TRUE
  )
})

test_that("dist_groups works with numeric groups", {
  x <- matrix(c(0, 1, 5, 1, 0, 6, 5, 6, 0), nrow = 3)
  d <- as.dist(x)
  g <- c(1, 1, 2)

  result <- dist_groups(d, g)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("dist_groups handles many groups", {
  set.seed(123)
  n <- 10
  x <- matrix(runif(n * n), nrow = n)
  x <- (x + t(x)) / 2
  diag(x) <- 0
  d <- as.dist(x)

  # 5 groups
  g <- rep(1:5, each = 2)

  result <- dist_groups(d, g)

  expect_s3_class(result, "data.frame")
  # Should have n*(n-1)/2 pairwise distances
  expect_equal(nrow(result), n * (n - 1) / 2)
})

test_that("dist_groups output has correct structure", {
  x <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)
  d <- as.dist(x)
  g <- c("A", "B", "C")

  result <- dist_groups(d, g)

  # Check column types
  expect_type(result$Distance, "double")
  expect_type(result$Item1, "integer")
  expect_type(result$Item2, "integer")

  # Check Label is a factor
  expect_s3_class(result$Label, "factor")

  # No NA values in distance
  expect_false(anyNA(result$Distance))
})

test_that("dist_groups includes group information", {
  x <- matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), nrow = 3)
  d <- as.dist(x)
  g <- c("A", "B", "A")

  result <- dist_groups(d, g)

  # Should have Group1 and Group2 columns
  expect_true("Group1" %in% names(result))
  expect_true("Group2" %in% names(result))

  # Groups should be factors
  expect_s3_class(result$Group1, "factor")
  expect_s3_class(result$Group2, "factor")
})

test_that("dist_get reads the exact packed-matrix position", {
  # A 4x4 matrix whose off-diagonal entries are all distinct, so reading the
  # wrong slot is visible.
  x <- matrix(0, nrow = 4, ncol = 4)
  x[1, 2] <- x[2, 1] <- 1
  x[1, 3] <- x[3, 1] <- 2
  x[1, 4] <- x[4, 1] <- 3
  x[2, 3] <- x[3, 2] <- 4
  x[2, 4] <- x[4, 2] <- 5
  x[3, 4] <- x[4, 3] <- 6
  d <- as.dist(x)

  expect_equal(dist_get(d, 1, 2), 1)
  expect_equal(dist_get(d, 1, 3), 2)
  expect_equal(dist_get(d, 1, 4), 3)
  expect_equal(dist_get(d, 2, 3), 4)
  expect_equal(dist_get(d, 2, 4), 5)
  expect_equal(dist_get(d, 3, 4), 6)
  # Every diagonal entry is 0, never a matrix read.
  for (i in 1:4) {
    expect_equal(dist_get(d, i, i), 0)
  }
})

test_that("dist_get treats the boundary index as in range", {
  x <- matrix(0, nrow = 3, ncol = 3)
  x[1, 2] <- x[2, 1] <- 7
  x[1, 3] <- x[3, 1] <- 9
  d <- as.dist(x)

  # n == 3 is in range and must not warn.
  expect_no_warning(expect_equal(dist_get(d, 1, 3L), 9))
  expect_no_warning(expect_equal(dist_get(d, 3L, 3L), 0))

  # Beyond the range warns. The documented return is NA, but the value that
  # actually comes back is whatever the packed-matrix read lands on: a
  # far-out index reads past the end and gives NA, while a just-out-of-range
  # index wraps onto a real slot. Pinned here so the divergence from the
  # documented contract is visible rather than accidental.
  expect_warning(far <- dist_get(d, 1, 10L), "out of bounds")
  expect_true(is.na(far))
  expect_warning(near <- dist_get(d, 1, 4L), "out of bounds")
  expect_false(is.na(near))
  expect_warning(low <- dist_get(d, 0L, 1L), "out of bounds")
  expect_false(is.na(low))
})

test_that("dist_get rounds nothing and reports the raw distance", {
  x <- matrix(c(0, 1.23456789, 1.23456789, 0), nrow = 2)
  d <- as.dist(x)
  expect_equal(dist_get(d, 1, 2), 1.23456789)
})

test_that("dist_groups labels within pairs with the shared group name", {
  x <- matrix(c(0, 1, 10, 1, 0, 11, 10, 11, 0), nrow = 3)
  d <- as.dist(x)

  res <- as.data.frame(dist_groups(d, c("A", "A", "B")))
  # combn order is (1,2), (1,3), (2,3): only (1,2) shares a group.
  expect_equal(
    as.character(res$Label),
    c(
      "Within A",
      "Between A and B",
      "Between A and B"
    )
  )
})

test_that("dist_groups orders between-group labels by factor level", {
  # The group vector is deliberately unsorted (B, A, C) so that the label
  # order can only come from the factor levels, not from the input order.
  x <- matrix(c(0, 1, 10, 1, 0, 11, 10, 11, 0), nrow = 3)
  res <- as.data.frame(dist_groups(as.dist(x), c("B", "A", "C")))

  expect_equal(
    as.character(res$Label),
    c(
      "Between A and B",
      "Between B and C",
      "Between A and C"
    )
  )
  # Group1/Group2 stay in observation order, they are not sorted.
  expect_equal(as.character(res$Group1), c("B", "B", "A"))
  expect_equal(as.character(res$Group2), c("A", "C", "C"))
})

test_that("dist_groups handles numeric group labels", {
  x <- matrix(c(0, 1, 10, 1, 0, 11, 10, 11, 0), nrow = 3)
  res <- as.data.frame(dist_groups(as.dist(x), c(2, 1, 1)))

  # Observations 2 and 3 are both group 1, so that pair is within.
  expect_equal(as.character(res$Label[3L]), "Within 1")
  expect_equal(as.character(res$Label[1L]), "Between 1 and 2")
})

test_that("dist_groups rounds distances to 5 decimals", {
  x <- matrix(c(0, 1.23456789, 1.23456789, 0), nrow = 2)
  res <- as.data.frame(dist_groups(as.dist(x), c("A", "B")))
  expect_equal(res$Distance, 1.23457)
})

test_that("dist_groups enumerates every unordered pair exactly once", {
  x <- matrix(c(0, 1, 2, 3, 1, 0, 4, 5, 2, 4, 0, 6, 3, 5, 6, 0), nrow = 4)
  res <- as.data.frame(dist_groups(as.dist(x), c("a", "b", "a", "b")))

  expect_equal(nrow(res), 6L)
  expect_true(all(res$Item1 < res$Item2))
  expect_equal(res$Distance, c(1, 2, 3, 4, 5, 6))
  # Two within-group (a,a) and (b,b) pairs, four between-group.
  expect_equal(sum(as.character(res$Label) == "Within a"), 1L)
  expect_equal(sum(as.character(res$Label) == "Within b"), 1L)
  expect_equal(sum(grepl("^Between", as.character(res$Label))), 4L)
})
