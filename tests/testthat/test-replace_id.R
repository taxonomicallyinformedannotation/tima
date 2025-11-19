# ==============================================================================
# Test Suite: read_mgf_opti
# ==============================================================================

test_that("replace_id handles different input combinations", {
  # Empty GNPS ID
  result1 <- replace_id(
    x = "example/123456_features.tsv",
    user_gnps = "",
    user_filename = "Foo"
  )
  expect_type(result1, "character")

  # Non-empty GNPS ID
  result2 <- replace_id(
    x = "example/123456_features.tsv",
    user_gnps = "Foo",
    user_filename = "Foo"
  )
  expect_type(result2, "character")

  # Default example GNPS ID
  result3 <- replace_id(
    x = "example/123456_features.tsv",
    user_gnps = get_default_paths()$gnps$example,
    user_filename = "Foo"
  )
  expect_type(result3, "character")
})
