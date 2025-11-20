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
  expect_no_error(result <- dist_get(x, 1, 2))
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
    "Length.*grouping vector"
  )

  # Group vector too long
  expect_error(
    dist_groups(d, c("A", "B", "C")),
    "Length.*grouping vector"
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
  expect_false(any(is.na(result$Distance)))
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
