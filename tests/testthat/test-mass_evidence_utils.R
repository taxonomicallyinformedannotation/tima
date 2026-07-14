test_that(".has_library_match_within_tolerance uses combined dalton+ppm tolerance", {
  # Library exact masses - need to be close enough to query masses
  lib <- c(100.0, 200.0, 500.0, 1000.0)
  
  # Query masses very close to library masses (within 0.01 Da)
  masses <- c(100.005, 200.005, 500.005, 1000.005)
  
  # With dalton=0.01: all should match (0.005 < 0.01)
  hit <- .has_library_match_within_tolerance(masses, lib, tolerance_ppm = 10, tolerance_dalton = 0.01)
  expect_true(all(hit))
  
  # With dalton=NULL (ppm only, 10 ppm): 
  # At 100 Da: 10 ppm = 0.001 Da. 100.005 is 0.005 away = 50 ppm > 10 ppm -> FALSE
  # At 500 Da: 10 ppm = 0.005 Da. 500.005 is 0.005 away = exactly 10 ppm -> TRUE (interval is (lo, hi])
  # At 1000 Da: 10 ppm = 0.01 Da. 1000.005 is 0.005 away = 5 ppm < 10 ppm -> TRUE
  hit <- .has_library_match_within_tolerance(masses, lib, tolerance_ppm = 10, tolerance_dalton = NULL)
  expect_equal(hit, c(FALSE, FALSE, TRUE, TRUE))
  
  # With dalton=0.001 (smaller than ppm at high mass): 
  # At 1000 Da: 10 ppm = 0.01 Da. max(0.001, 0.01) = 0.01. 1000.005 is within 0.01 -> TRUE
  # At 100 Da: 10 ppm = 0.001 Da. max(0.001, 0.001) = 0.001. 100.005 is 0.005 away -> FALSE
  hit <- .has_library_match_within_tolerance(c(1000.005), lib, tolerance_ppm = 10, tolerance_dalton = 0.001)
  expect_true(hit)
  
  hit <- .has_library_match_within_tolerance(c(100.005), lib, tolerance_ppm = 10, tolerance_dalton = 0.001)
  expect_false(hit)
  
  # Edge cases
  expect_equal(.has_library_match_within_tolerance(numeric(0), lib, 10, 0.01), logical(0))
  expect_equal(.has_library_match_within_tolerance(c(100.0), numeric(0), 10, 0.01), c(FALSE))
})

test_that("backward compatible .has_library_match_within_ppm works", {
  lib <- c(100.0, 200.0)
  hit <- .has_library_match_within_ppm(c(100.0005, 100.02), lib, 10)
  expect_equal(hit, c(TRUE, FALSE))
})
