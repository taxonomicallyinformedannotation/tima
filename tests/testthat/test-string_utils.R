# Test Suite: string_utils ----

library(testthat)

test_that("stri_detect_token matches whole values, not substrings", {
  expect_false(stri_detect_token(str = "Arabidopsis", pattern = "Ara"))
  expect_true(stri_detect_token(str = "Arabidopsis", pattern = "Arabidopsis"))
})

test_that("stri_detect_token matches against collapsed values", {
  expect_equal(
    stri_detect_token(
      str = c("Gentiana $ Arabidopsis", "Gentiana $ Arabidopsis"),
      pattern = c("Arabidopsis", "Ara")
    ),
    c(TRUE, FALSE)
  )
})

test_that("stri_detect_token takes regex metacharacters literally", {
  expect_true(stri_detect_token(str = "a (+) b", pattern = "a (+) b"))
  expect_false(stri_detect_token(str = "ab", pattern = "a."))
  expect_false(stri_detect_token(str = NA_character_, pattern = "a"))
})

test_that("stri_detect_token handles empty inputs, scalar recycling, and whitespace", {
  expect_equal(
    stri_detect_token(str = character(0), pattern = "a"),
    logical(0)
  )
  expect_equal(
    stri_detect_token(
      str = "  alpha  $  beta $ gamma ",
      pattern = c("alpha", "beta", "gamma", "alp", NA_character_)
    ),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("stri_detect_token recycles pattern when length(pattern) < length(str) and handles multi-token patterns", {
  expect_identical(
    stri_detect_token(
      str = c("A $ B", "C $ D", "E", "B"),
      pattern = "B"
    ),
    c(TRUE, FALSE, FALSE, TRUE)
  )
  # Multi-token pattern matching against multi-token str
  expect_identical(
    stri_detect_token(
      str = c("X $ Y", "X $ Z", "NoSep1"),
      pattern = c("W $ Y", "A $ B", "NoSep2")
    ),
    c(TRUE, FALSE, FALSE)
  )
  # Non-character inputs coerced to character
  expect_identical(
    stri_detect_token(str = c(10L, 20L), pattern = 20L),
    c(FALSE, TRUE)
  )
})

test_that("stri_length and stri_sub handle positive, negative, default bounds, and NAs", {
  expect_identical(
    stri_length(c("", "a", "abcd", NA_character_)),
    c(0L, 1L, 4L, NA_integer_)
  )

  # Positive indices
  expect_identical(stri_sub("abcdef", 2L, 4L), "bcd")
  # Default to = from (single character)
  expect_identical(stri_sub("abcdef", 3L), "c")
  # Negative indices (from end of string: n + from + 1)
  expect_identical(stri_sub("abcdef", -3L, -1L), "def")
  expect_identical(stri_sub("abcdef", -1L), "f")
  expect_identical(stri_sub("abcdef", 2L, -2L), "bcde")
})

test_that("stri_match_first_regex handles empty input, no matches, zero and optional capture groups", {
  # Empty input -> 0x0 character matrix
  m0 <- stri_match_first_regex(character(0), "a")
  expect_identical(dim(m0), c(0L, 0L))
  expect_type(m0, "character")

  # No matches -> all NA matrix
  m_none <- stri_match_first_regex(c("xyz", "123"), "^([a-z]+)_([0-9]+)$")
  expect_identical(dim(m_none), c(2L, 3L))
  expect_true(all(is.na(m_none)))

  # Pattern without capture groups -> 2 columns, col 1 has full match, col 2 is NA
  m_nocap <- stri_match_first_regex(c("abc123def", "zzz"), "[0-9]+")
  expect_identical(
    m_nocap,
    matrix(
      c("123", NA_character_, NA_character_, NA_character_),
      nrow = 2L,
      ncol = 2L
    )
  )

  # Pattern with required + optional capture groups
  m_opt <- stri_match_first_regex(
    c("ID_42_v2", "ID_99", "no_hit"),
    "^ID_([0-9]+)(?:_v([0-9]+))?$"
  )
  expected_opt <- rbind(
    c("ID_42_v2", "42", "2"),
    c("ID_99", "99", NA_character_),
    c(NA_character_, NA_character_, NA_character_)
  )
  expect_identical(m_opt, expected_opt)
})

test_that("stri_split_fixed, stri_split_regex, stri_trans_totitle, and stri_extract_all_regex work accurately", {
  expect_identical(
    stri_split_fixed(c("a.b.c", "x.y"), "."),
    list(c("a", "b", "c"), c("x", "y"))
  )
  expect_identical(
    stri_split_regex(c("a12b3c", "d4e"), "[0-9]+"),
    list(c("a", "b", "c"), c("d", "e"))
  )
  expect_identical(
    stri_trans_totitle("taxonomically informed metabolite annotation"),
    "Taxonomically Informed Metabolite Annotation"
  )
  expect_identical(
    stri_extract_all_regex(c("C6H12O6", "N2"), "[A-Z][0-9]*"),
    list(c("C6", "H12", "O6"), "N2")
  )
})

test_that("stri_replace_all_fixed and stri_replace_all_regex handle empty patterns, case_insensitive metacharacters, and recycling", {
  # Empty patterns return input unchanged
  expect_identical(
    stri_replace_all_fixed(c("abc", "def"), character(0), character(0)),
    c("abc", "def")
  )
  expect_identical(
    stri_replace_all_regex(c("abc", "def"), character(0), character(0)),
    c("abc", "def")
  )

  # Case-sensitive fixed replacement with pattern/replacement recycling
  expect_identical(
    stri_replace_all_fixed("a.b+c", c(".", "+"), "_"),
    "a_b_c"
  )

  # Case-insensitive fixed replacement escaping regex metacharacters
  expect_identical(
    stri_replace_all_fixed(
      str = " Foo[1]+Bar(2)?$^* ",
      pattern = c("foo[1]", "BAR(2)?$^*"),
      replacement = c("ALPHA", "BETA"),
      case_insensitive = TRUE
    ),
    " ALPHA+BETA "
  )

  # Regex replacement with recycling
  expect_identical(
    stri_replace_all_regex(
      str = c("item_01_end", "item_99_end"),
      pattern = c("^item_", "_end$"),
      replacement = ""
    ),
    c("01", "99")
  )
})
