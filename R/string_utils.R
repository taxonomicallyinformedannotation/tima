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
  str <- if (is.character(str)) str else as.character(str)
  pattern <- if (is.character(pattern)) pattern else as.character(pattern)
  n_s <- length(str)
  n_p <- length(pattern)
  if (n_s == 0L || n_p == 0L) {
    return(logical(0L))
  }
  n <- max(n_s, n_p)
  if (n_s != n) {
    str <- rep_len(str, n)
  }
  if (n_p != n) {
    pattern <- rep_len(pattern, n)
  }

  valid <- !is.na(str) & !is.na(pattern)
  out <- valid & (str == pattern)
  rem <- which(valid & !out)
  if (length(rem) == 0L) {
    return(out)
  }

  sv_rem <- str[rem]
  pv_rem <- pattern[rem]
  u_s <- unique(sv_rem)
  u_p <- unique(pv_rem)
  s_has_sep <- grepl(separator, u_s, fixed = TRUE)
  p_has_sep <- grepl(separator, u_p, fixed = TRUE)
  if (!any(s_has_sep) && !any(p_has_sep)) {
    return(out)
  }

  s_id <- fastmatch::fmatch(sv_rem, u_s)
  p_id <- fastmatch::fmatch(pv_rem, u_p)
  can_match <- which(s_has_sep[s_id] | p_has_sep[p_id])

  s_sub <- s_id[can_match]
  p_sub <- p_id[can_match]
  n_up <- length(u_p)
  sp_key <- as.double(s_sub - 1L) * n_up + p_sub
  u_sp_idx <- which(!duplicated(sp_key))
  sp_grp <- fastmatch::fmatch(sp_key, sp_key[u_sp_idx])

  u_s_used <- unique(s_sub[u_sp_idx])
  u_p_used <- unique(p_sub[u_sp_idx])

  s_toks <- vector("list", length(u_s))
  s_toks[u_s_used] <- lapply(
    strsplit(u_s[u_s_used], separator, fixed = TRUE),
    trimws
  )
  p_toks <- vector("list", length(u_p))
  p_toks[u_p_used] <- lapply(
    strsplit(u_p[u_p_used], separator, fixed = TRUE),
    trimws
  )

  s_u_pair <- s_sub[u_sp_idx]
  p_u_pair <- p_sub[u_sp_idx]
  pair_hit <- vapply(
    seq_along(u_sp_idx),
    function(i) {
      any(p_toks[[p_u_pair[[i]]]] %in% s_toks[[s_u_pair[[i]]]])
    },
    logical(1L)
  )

  out[rem[can_match]] <- pair_hit[sp_grp]
  out
}

stri_length <- function(str) {
  nchar(str, type = "chars", allowNA = TRUE, keepNA = TRUE)
}

stri_match_first_regex <- function(str, pattern) {
  n <- length(str)
  if (n == 0L) {
    return(matrix(character(0), nrow = 0L, ncol = 0L))
  }

  str <- as.character(str)
  m <- regexpr(pattern, str, perl = TRUE)
  cap_start <- attr(m, "capture.start")
  cap_len <- attr(m, "capture.length")
  n_caps <- if (is.null(cap_start)) 0L else ncol(cap_start)
  n_cols <- max(1L + n_caps, 2L)

  out <- matrix(NA_character_, nrow = n, ncol = n_cols)
  hit <- which(!is.na(m) & m > 0L)
  if (length(hit) == 0L) {
    return(out)
  }

  match_len <- attr(m, "match.length")
  out[hit, 1L] <- substr(str[hit], m[hit], m[hit] + match_len[hit] - 1L)

  if (n_caps > 0L) {
    cs_sub <- cap_start[hit, , drop = FALSE]
    cl_sub <- cap_len[hit, , drop = FALSE]
    valid_pos <- which(cs_sub > 0L)
    if (length(valid_pos) > 0L) {
      n_hit <- length(hit)
      h_rows <- hit[(valid_pos - 1L) %% n_hit + 1L]
      c_cols <- (valid_pos - 1L) %/% n_hit + 2L
      cs_v <- cs_sub[valid_pos]
      cl_v <- cl_sub[valid_pos]
      out[h_rows + (c_cols - 1L) * n] <- substr(
        str[h_rows],
        cs_v,
        cs_v + cl_v - 1L
      )
    }
  }

  out
}

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

  Reduce(
    function(cur, i) {
      pat <- patterns[(i - 1L) %% length(patterns) + 1L]
      repl <- replacements[(i - 1L) %% length(replacements) + 1L]
      if (case_insensitive) {
        pat <- gsub(
          pattern = "([][{}()+*^$.|\\\\?])",
          replacement = "\\\\\\1",
          x = pat,
          perl = TRUE
        )
        gsub(
          pattern = pat,
          replacement = repl,
          x = cur,
          perl = TRUE,
          ignore.case = TRUE
        )
      } else {
        gsub(
          pattern = pat,
          replacement = repl,
          x = cur,
          fixed = TRUE
        )
      }
    },
    seq_len(n),
    init = x
  )
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

  Reduce(
    function(cur, i) {
      gsub(
        pattern = patterns[(i - 1L) %% length(patterns) + 1L],
        replacement = replacements[(i - 1L) %% length(replacements) + 1L],
        x = cur,
        perl = TRUE
      )
    },
    seq_len(n),
    init = x
  )
}

stri_extract_all_regex <- function(str, pattern) {
  regmatches(str, gregexpr(pattern, str, perl = TRUE))
}
