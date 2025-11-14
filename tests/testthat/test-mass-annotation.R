# Test: Mass Annotation Functions
library(testthat)

test_that("calculate_mass_of_m handles various adduct formats", {
  # Complex adduct with isotope and modifications
  result1 <- calculate_mass_of_m(
    adduct_string = "[2M1-2H2O+NaCl+H]2+",
    mz = 123.456
  )
  expect_type(result1, "double")

  # Simple sodium adduct
  result2 <- calculate_mass_of_m(
    adduct_string = "[M+Na]+",
    mz = 123.456
  )
  expect_type(result2, "double")

  # Proton adduct
  result3 <- calculate_mass_of_m(
    adduct_string = "[M+H]+",
    mz = 123.456
  )
  expect_type(result3, "double")

  # Bare M+
  result4 <- calculate_mass_of_m(
    adduct_string = "[M+]+",
    mz = 123.456
  )
  expect_type(result4, "double")

  # Just M
  result5 <- calculate_mass_of_m(
    adduct_string = "[M]+",
    mz = 123.456
  )
  expect_type(result5, "double")
})

test_that("calculate_mass_of_m handles complex modifications", {
  # Hexose modification
  result1 <- calculate_mass_of_m(
    adduct_string = "[2M1-C6H12O6 (hexose)+NaCl+H]2+",
    mz = 123.456
  )
  expect_type(result1, "double")

  # Hexose with water loss
  result2 <- calculate_mass_of_m(
    adduct_string = "[M-C6H14O7 (hexose-H2O)+H]+",
    mz = 123.456
  )
  expect_type(result2, "double")

  # Multiple adduct possibilities
  result3 <- calculate_mass_of_m(
    adduct_string = "[M+CH3COO]-/[M-CH3]-",
    mz = 123.456
  )
  expect_type(result3, "double")

  # Negative mode with potassium
  result4 <- calculate_mass_of_m(
    adduct_string = "[M+K-2H]-",
    mz = 123.456
  )
  expect_type(result4, "double")
})

test_that("calculate_mass_of_m handles NULL input", {
  result <- calculate_mass_of_m(
    adduct_string = NULL,
    mz = 123.456
  )
  expect_type(result, "double")
})

# test_that("annotate_masses works in negative mode without RT", {
#   skip_on_cran()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Create features without RT
#   tidytable::tidytable(
#     "row ID" = 1,
#     "row m/z" = 123.4567,
#     "sample.mzML Peak area" = 98765.43
#   ) |>
#     tidytable::fwrite("data/source/example_features_no_rt.csv")
#
#   prepare_features_tables(
#     features = "data/source/example_features_no_rt.csv",
#     output = "data/interim/features/example_features_no_rt.tsv.gz"
#   )
#
#   # Setup library
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_merged()
#
#   expect_no_error(
#     annotate_masses(
#       features = "data/interim/features/example_features_no_rt.tsv.gz",
#       tolerance_ppm = 1.0,
#       tolerance_rt = 0.01,
#       ms_mode = "neg"
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# test_that("annotate_masses works in positive mode", {
#   skip_on_cran()
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   # Download and prepare data
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   prepare_features_tables()
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_merged()
#
#   expect_no_error(
#     annotate_masses(
#       tolerance_ppm = 1.0,
#       tolerance_rt = 0.01,
#       ms_mode = "pos"
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# test_that("annotate_masses handles pre-assigned adducts", {
#   copy_backbone(cache_dir = ".")
#   paths <- get_default_paths()
#
#   tidytable::tidytable(
#     "feature_id" = c(1, 2),
#     "mz" = c(123.4567, 141.4678),
#     "rt" = c(0.01, 0.02),
#     "adduct" = c("[M+XYZ]+", "[M+XYZ-H2O]+"),
#     "sample" = "sample.mzML"
#   ) |>
#     tidytable::fwrite("data/source/libraries/rt/example_features_adducts.csv")
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_merged()
#
#   expect_warning(
#     annotate_masses(
#       features = "data/source/libraries/rt/example_features_adducts.csv"
#     )
#   )
#
#   unlink("data", recursive = TRUE)
# })

# test_that("parse_adduct fails gracefully with invalid input", {
#   expect_error(parse_adduct("foo bar"))
# })
