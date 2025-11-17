# ==============================================================================
# Test Suite: RT Normalization Utilities
# ==============================================================================

test_that("normalize_rt_to_minutes leaves minute-scale values unchanged (auto)", {
  x <- c(0.5, 1.2, 10, 30, NA, 45)
  y <- normalize_rt_to_minutes(x, unit = "auto", quiet = TRUE)
  expect_equal(y[!is.na(y)], x[!is.na(x)])
})

test_that("normalize_rt_to_minutes converts seconds > heuristic thresholds", {
  secs <- c(350, 420, 500) # clearly seconds ( > 120 and > 180 95th percentile )
  mins <- normalize_rt_to_minutes(secs, unit = "auto", quiet = TRUE)
  expect_equal(mins, secs / 60)
})

test_that("normalize_rt_to_minutes respects explicit 'seconds' unit", {
  secs <- c(180, 240, 300)
  mins <- normalize_rt_to_minutes(secs, unit = "seconds", quiet = TRUE)
  expect_equal(mins, secs / 60)
})

test_that("normalize_rt_to_minutes respects explicit 'minutes' unit", {
  mins_in <- c(1, 5, 45)
  mins_out <- normalize_rt_to_minutes(mins_in, unit = "minutes", quiet = TRUE)
  expect_equal(mins_out, mins_in)
})

# test_that("prepare_features_tables auto-converts seconds to minutes", {
#   tmp <- withr::local_tempfile(fileext = ".csv")
#   df <- tidytable::tidytable(
#     id = c("1", "2", "3"),
#     rt_seconds = c(300, 420, 150),
#     mz = c(100.1, 150.2, 200.3),
#     adduct = c("[M+H]+", "[M+Na]+", "[M+H]+"),
#     `sampleA Peak area` = c(1000, 2000, 3000),
#     `sampleB Peak area` = c(900, 2100, 2500)
#   )
#   export_output(df, tmp)
#   out <- withr::local_tempfile(fileext = "_prepared.csv")
#   res_path <- prepare_features_tables(
#     features = tmp,
#     output = out,
#     name_features = "id",
#     name_rt = "rt_seconds",
#     name_mz = "mz",
#     name_adduct = "adduct",
#     candidates = 2
#   )
#   res <- tidytable::fread(res_path, colClasses = "character")
#   expect_true(all(
#     abs(as.numeric(res$rt) - as.numeric(df$rt_seconds) / 60) < 1e-9
#   ))
# })
