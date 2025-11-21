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
    "Input file\\(s\\) not found"
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
    "Input file\\(s\\) not found"
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
  expect_false(any(is.na(components$`cluster index`)))
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
