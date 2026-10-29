#' String helpers
#'
#' @name string_utils
#' @keywords internal
#' @noRd
NULL

# Exact token match: TRUE when any `separator`-delimited value of `pattern`
# equals a value of `str`. Unlike a regex search, "Ara" does not match
# "Arabidopsis" and regex metacharacters in names are taken literally.
stri_detect_token <- function(str, pattern, separator = " $ ") {
  str <- as.character(str)
  pattern <- as.character(pattern)
  out <- !is.na(str) & !is.na(pattern) & str == pattern
  multi <- which(
    !out &
      !is.na(str) &
      !is.na(pattern) &
      (grepl(separator, str, fixed = TRUE) |
        grepl(separator, pattern, fixed = TRUE))
  )
  if (length(multi) > 0L) {
    out[multi] <- mapply(
      FUN = function(x, p) {
        any(
          trimws(strsplit(p, separator, fixed = TRUE)[[1L]]) %in%
            trimws(strsplit(x, separator, fixed = TRUE)[[1L]])
        )
      },
      str[multi],
      pattern[multi],
      USE.NAMES = FALSE
    )
  }
  out
}

stri_length <- function(str) {
  nchar(str, type = "chars", allowNA = TRUE, keepNA = TRUE)
}

stri_match_first_regex <- function(str, pattern) {
  if (length(str) == 0L) {
    return(matrix(character(0), nrow = 0L, ncol = 0L))
  }

  m <- regexec(pattern, str, perl = TRUE)
  matches <- regmatches(str, m)
  n_cols <- max(max(lengths(m)), 2L)
  out <- matrix(NA_character_, nrow = length(str), ncol = n_cols)

  for (i in seq_along(matches)) {
    if (length(matches[[i]]) > 0L) {
      out[i, seq_along(matches[[i]])] <- matches[[i]]
    }
  }

  out
}

# jarl-ignore unused_function: used by the Shiny app (inst/app_server.R)
stri_split_fixed <- function(str, pattern) {
  strsplit(str, split = pattern, fixed = TRUE)
}

stri_split_regex <- function(str, pattern) {
  strsplit(str, split = pattern, perl = TRUE)
}

stri_sub <- function(str, from, to = from) {
  str <- as.character(str)
  n <- nchar(str, type = "chars", allowNA = TRUE, keepNA = TRUE)

  start <- ifelse(from < 0L, n + from + 1L, from)
  end <- ifelse(to < 0L, n + to + 1L, to)

  substring(str, first = start, last = end)
}

stri_trans_totitle <- function(str) {
  tools::toTitleCase(str)
}

stri_replace_all_fixed <- function(
  str,
  pattern,
  replacement,
  case_insensitive = FALSE,
  vectorize_all = FALSE
) {
  x <- str
  patterns <- as.character(pattern)
  replacements <- as.character(replacement)
  n <- max(length(patterns), length(replacements))

  if (n == 0L) {
    return(x)
  }

  for (i in seq_len(n)) {
    pat <- patterns[(i - 1L) %% length(patterns) + 1L]
    repl <- replacements[(i - 1L) %% length(replacements) + 1L]
    if (case_insensitive) {
      pat <- gsub(
        pattern = "([][{}()+*^$.|\\\\?])",
        replacement = "\\\\\\1",
        x = pat,
        perl = TRUE
      )
      x <- gsub(
        pattern = pat,
        replacement = repl,
        x = x,
        perl = TRUE,
        ignore.case = TRUE
      )
    } else {
      x <- gsub(
        pattern = pat,
        replacement = repl,
        x = x,
        fixed = TRUE
      )
    }
  }

  x
}

stri_replace_all_regex <- function(
  str,
  pattern,
  replacement,
  vectorize_all = FALSE
) {
  x <- str
  patterns <- as.character(pattern)
  replacements <- as.character(replacement)
  n <- max(length(patterns), length(replacements))

  if (n == 0L) {
    return(x)
  }

  for (i in seq_len(n)) {
    x <- gsub(
      pattern = patterns[(i - 1L) %% length(patterns) + 1L],
      replacement = replacements[(i - 1L) %% length(replacements) + 1L],
      x = x,
      perl = TRUE
    )
  }

  x
}

stri_extract_all_regex <- function(str, pattern) {
  regmatches(str, gregexpr(pattern, str, perl = TRUE))
}
