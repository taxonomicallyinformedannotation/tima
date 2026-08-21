# Characterization tests for the base-R reimplementations in R/string_utils.R.
#
# string_utils.R reimplements a subset of the stringi API in base R so the
# package can drop stringi as a direct dependency. These helpers are exercised
# only indirectly (via import_spectra, weight_bio, parse_adduct, ...), so the
# edge cases below pin down their current, observable behavior.
#
# For the COMMON cases the helpers match stringi::stri_* exactly. A few edge
# cases diverge from stringi (see FIXMEs); those tests lock in the *current*
# output so any future alignment with stringi is an explicit, flagged decision
# rather than a silent behavior change.

test_that("stri_detect_regex matches stringi on common inputs", {
  skip_if_not_installed("stringi")
  x <- c("hello world", "foo", "", "a.b.c")
  expect_identical(
    tima:::stri_detect_regex(x, "o"),
    stringi::stri_detect_regex(x, "o")
  )
  expect_identical(
    tima:::stri_detect_regex(c("a1", "b2", "c3"), c("[a]", "[1-9]", "[z]")),
    stringi::stri_detect_regex(c("a1", "b2", "c3"), c("[a]", "[1-9]", "[z]"))
  )
})

test_that("stri_detect_regex empty input returns a list (FIXME: stringi returns logical(0))", {
  # mapply(SIMPLIFY = TRUE) over zero elements yields an empty list rather than
  # logical(0). No caller passes character(0), but locking current behavior.
  got <- tima:::stri_detect_regex(character(0), "x")
  expect_type(got, "list")
  expect_length(got, 0L)
})

test_that("stri_detect_regex returns FALSE for NA (FIXME: stringi returns NA)", {
  # mapply()+grepl() collapses NA to FALSE. stringi preserves NA. No caller is
  # known to rely on NA here yet, but locking the current behavior.
  expect_equal(tima:::stri_detect_regex(NA_character_, "o"), FALSE)
  expect_equal(
    tima:::stri_detect_regex(c("a", NA, "b"), "a"),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("stri_length matches stringi", {
  skip_if_not_installed("stringi")
  x <- c("hello", NA, "", "cafe", "a.b.c")
  expect_identical(tima:::stri_length(x), stringi::stri_length(x))
})

test_that("stri_match_first_regex matches stringi when capture groups present", {
  skip_if_not_installed("stringi")
  x <- c("abc123", "xyz12", NA, "", "none")
  expect_identical(
    tima:::stri_match_first_regex(x, "([a-z]+)(\\d+)"),
    stringi::stri_match_first_regex(x, "([a-z]+)(\\d+)")
  )
})

test_that("stri_match_first_regex forces >=2 columns even without captures (FIXME)", {
  # Without capture groups, stringi returns a 1-column matrix; tima pads to 2
  # columns (full match + NA). Callers always use capture groups, so this is
  # currently harmless, but it is a divergence worth flagging.
  got <- tima:::stri_match_first_regex(c("abc", "xyz"), "a")
  expect_equal(dim(got), c(2L, 2L))
  expect_equal(got[, 1L], c("a", NA_character_))
})

test_that("stri_match_first_regex empty input gives 0x0 (FIXME: stringi gives 0x2)", {
  expect_equal(
    dim(tima:::stri_match_first_regex(character(0), "([a-z])")),
    c(0L, 0L)
  )
})

test_that("stri_sub matches stringi", {
  skip_if_not_installed("stringi")
  x <- c("abcdef", "ghij")
  expect_identical(tima:::stri_sub(x, 2, 4), stringi::stri_sub(x, 2, 4))
  expect_identical(tima:::stri_sub("abcdef", -3, -1), stringi::stri_sub("abcdef", -3, -1))
  expect_identical(tima:::stri_sub(NA_character_, 1, 2), stringi::stri_sub(NA_character_, 1, 2))
})

test_that("stri_sub two-argument form (default to = from) truncates to one char (FIXME)", {
  # stringi::stri_sub(x, from) defaults `to` to the end of each string; tima's
  # stri_sub(str, from, to = from) defaults `to = from`, so the 2-arg form
  # returns a single character. No caller uses the 2-arg form (all pass `to`),
  # but locking current behavior.
  expect_equal(tima:::stri_sub(c("abcdef", "ghij"), 2), c("b", "h"))
})

test_that("stri_sub recycles from/to like stringi", {
  skip_if_not_installed("stringi")
  expect_identical(
    tima:::stri_sub("abcdef", c(1, 3), c(2, 5)),
    stringi::stri_sub("abcdef", c(1, 3), c(2, 5))
  )
})

test_that("stri_split_regex matches stringi", {
  skip_if_not_installed("stringi")
  expect_identical(
    tima:::stri_split_regex(c("a,b,c", "x,y"), ","),
    stringi::stri_split_regex(c("a,b,c", "x,y"), ",")
  )
})

test_that("stri_trans_totitle matches stringi for typical inputs", {
  skip_if_not_installed("stringi")
  expect_identical(
    tima:::stri_trans_totitle(c("hello world", "mgf file")),
    stringi::stri_trans_totitle(c("hello world", "mgf file"))
  )
})

test_that("stri_trans_totitle lowercases with tools::toTitleCase (FIXME: differs from stringi)", {
  # tools::toTitleCase does not lowercase non-initial letters; stringi does.
  got <- tima:::stri_trans_totitle("aBC")
  expect_equal(got, "aBC")
  # FIXME: stringi::stri_trans_totitle("aBC") == "Abc"
})

test_that("stri_replace_all_fixed matches stringi on common inputs", {
  skip_if_not_installed("stringi")
  expect_identical(
    tima:::stri_replace_all_fixed(c("a.b.c", "x.b.y"), ".", "-"),
    stringi::stri_replace_all_fixed(c("a.b.c", "x.b.y"), ".", "-")
  )
  expect_identical(
    tima:::stri_replace_all_fixed("Hello HELLO", "hello", "X", case_insensitive = TRUE),
    stringi::stri_replace_all_fixed("Hello HELLO", "hello", "X", case_insensitive = TRUE)
  )
})

test_that("stri_replace_all_fixed recycling differs from stringi (FIXME)", {
  # vec patterns/replacements of unequal length: tima loops cumulatively,
  # stringi recycles per-element. Locks current tima behavior.
  expect_equal(
    tima:::stri_replace_all_fixed(c("a=b", "c=d"), c("="), c(":", "-")),
    c("a:b", "c:d")
  )
})

test_that("stri_replace_all_regex matches stringi on scalar pattern", {
  skip_if_not_installed("stringi")
  expect_identical(
    tima:::stri_replace_all_regex(c("a1b2", "x3"), "[0-9]", "N"),
    stringi::stri_replace_all_regex(c("a1b2", "x3"), "[0-9]", "N")
  )
})

test_that("stri_replace_all_regex recycling differs from stringi (FIXME)", {
  # tima applies patterns sequentially (cumulative); stringi returns one value
  # per pattern. Locks current tima behavior.
  expect_equal(
    tima:::stri_replace_all_regex("a1b", c("[0-9]", "[a-z]"), c("D", "~")),
    "~D~"
  )
})

test_that("stri_extract_all_regex matches stringi", {
  skip_if_not_installed("stringi")
  expect_identical(
    tima:::stri_extract_all_regex("a1b2c3", "[0-9]"),
    stringi::stri_extract_all_regex("a1b2c3", "[0-9]")
  )
})
