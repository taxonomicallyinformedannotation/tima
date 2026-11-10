# Pre-computed formula normalization patterns (avoid re-creating each call)
.FORMULA_SUBS_PATTERNS <- c(
  "\\+NH4",
  "\\+2NH4",
  "\\+NH3",
  "\\-NH3",
  "\\-2NH3",
  "\\-2H2O",
  "\\-3H2O",
  "\\-4H2O",
  "\\-5H2O",
  "\\+H\\-H2O\\]",
  "\\+H\\-H4O2\\]",
  "\\+H\\-H6O3\\]",
  "\\+Na\\-H2O\\]",
  "\\+K\\-H2O\\]",
  "\\-H\\-H2O\\]"
)
.FORMULA_SUBS_REPLACEMENTS <- c(
  "+H4N",
  "+H8N2",
  "+H3N",
  "-H3N",
  "-H6N2",
  "-H4O2",
  "-H6O3",
  "-H8O4",
  "-H10O5",
  "-H2O+H]",
  "-H4O2+H]",
  "-H6O3+H]",
  "-H2O+Na]",
  "-H2O+K]",
  "-H2O-H]"
)

#' @title Harmonize adduct notations
#'
#' @description Standardizes adduct notations in a dataframe by replacing
#'     various forms with canonical representations. Uses a translation
#'     table for efficient batch replacement.
#'
#' @details Common adduct variations like "M+H", "\[M+H\]", and "(M+H)+" are
#'     standardized to a consistent format (e.g., "\[M+H\]+"). This ensures
#'     compatibility across different MS tools and databases.
#'
#' @include adducts_utils.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include adduct_universe_formula.R
#' @include validations_params.R
#'
#' @param df Data frame or tibble containing adduct column
#' @param adducts_colname Character string name of the adduct column
#'     (default: "adduct")
#' @param adducts_translations Named character vector mapping original
#'     adduct notations (names) to standardized forms (values).
#'     If missing, returns dataframe unchanged.
#'
#' @return Data frame with harmonized adduct column
#'
#' @family mass-spectrometry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(adduct = c("M+H", "[M+Na]+", "(M-H)-"))
#' translations <- c("M+H" = "[M+H]+", "(M-H)-" = "[M-H]-")
#' harmonize_adducts(df, adducts_translations = translations)
#' }
harmonize_adducts <- function(
  df,
  adducts_colname = "adduct",
  adducts_translations
) {
  validate_dataframe(df, param_name = "df")
  validate_character(
    adducts_colname,
    param_name = "adducts_colname",
    allow_empty = FALSE
  )

  do_log <- nrow(df) >= 1000L
  if (do_log) {
    ctx <- log_operation("harmonize_adducts", n_rows = nrow(df))
  }

  if (!adducts_colname %in% names(df)) {
    log_debug(
      "Column '%s' not found, skipping harmonization",
      adducts_colname
    )
    return(df)
  }

  if (missing(adducts_translations) || length(adducts_translations) == 0L) {
    return(df)
  }

  if (identical(adducts_translations, .ADDUCTS_TRANSLATIONS_DEFAULT_REF)) {
    adducts_translations <- .ADDUCTS_TRANSLATIONS_DEFAULT_SORTED
  } else {
    validate_adduct_translations(adducts_translations)

    # Sort them (important)
    adducts_translations <- adducts_translations[order(
      nchar(names(adducts_translations)),
      decreasing = TRUE
    )]
  }

  adducts <- df[[adducts_colname]]
  if (is.factor(adducts)) {
    adducts <- as.character(adducts)
  }

  if (length(adducts) == 0L) {
    n_unique_before <- 0L
    n_unique_after <- 0L
  } else {
    unique_adducts <- unique(adducts)
    non_missing_unique <- !is.na(unique_adducts)
    n_unique_before <- count_unique_values(unique_adducts)
    inverse_idx <- if (all(non_missing_unique)) {
      fastmatch::fmatch(adducts, unique_adducts)
    } else {
      match(adducts, unique_adducts)
    }
    attr(unique_adducts, ".match.hash") <- NULL

    if (any(non_missing_unique)) {
      unique_non_missing <- unique_adducts[non_missing_unique]

      # Normalize internal spaces (e.g., "[M + K]+" -> "[M+K]+")
      unique_non_missing <- gsub("\\s+", "", unique_non_missing)

      # Fast exact-match lookup via fmatch()
      idx <- fastmatch::fmatch(unique_non_missing, names(adducts_translations))
      matched <- !is.na(idx)
      if (any(matched)) {
        unique_non_missing[matched] <- unname(
          adducts_translations[idx[matched]]
        )
      }

      # Second pass: substring-level formula normalization
      # Handles all combinations (dimers, losses, clusters) at once
      unique_non_missing <- stri_replace_all_regex(
        str = unique_non_missing,
        pattern = .FORMULA_SUBS_PATTERNS,
        replacement = .FORMULA_SUBS_REPLACEMENTS,
        vectorize_all = FALSE
      )

      # Third pass: collapse canceling +/- terms and map known forbidden forms.
      # Uses vectorized memoization via match() to exploit high repetition
      # (real data: 95%+ duplicate adducts, process only unique values once)
      unique_non_missing <- canonicalize_adducts_vectorized(unique_non_missing)

      unique_adducts[non_missing_unique] <- unique_non_missing
    }

    adducts <- unique_adducts[inverse_idx]
    df[[adducts_colname]] <- adducts
    n_unique_after <- count_unique_values(unique_adducts)
  }

  if (do_log) {
    # Log reduction in unique forms (indicates successful harmonization)
    if (n_unique_before != n_unique_after) {
      log_debug(
        "Harmonized: %d -> %d unique adduct forms",
        n_unique_before,
        n_unique_after
      )
    }

    log_complete(
      ctx,
      n_unique_before = n_unique_before,
      n_unique_after = n_unique_after
    )
  }

  rm(
    list = intersect(
      c(
        "unique_adducts",
        "unique_non_missing",
        "inverse_idx",
        "non_missing_unique",
        "adducts"
      ),
      ls()
    )
  )

  df
}


#' Validate adduct translations structure
#' @keywords internal
validate_adduct_translations <- function(translations) {
  if (!is.character(translations)) {
    cli::cli_abort(
      c(
        "adducts_translations must be a character vector",
        "x" = class(translations)[1]
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (is.null(names(translations))) {
    cli::cli_abort(
      "adducts_translations must be a named vector (names = original, values = replacements)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  invisible(TRUE)
}

#' Count unique non-NA values
#' @keywords internal
count_unique_values <- function(x) {
  length(unique(x[!is.na(x)]))
}

#' Rebuild a Hill-order atomic formula string from named counts
#' @keywords internal
rebuild_hill_formula <- function(atom_counts) {
  atom_counts <- atom_counts[atom_counts > 0L]
  if (length(atom_counts) == 0L) {
    return("")
  }
  els <- names(atom_counts)
  ord <- order(ifelse(els == "C", 0L, ifelse(els == "H", 1L, 2L)), els)
  paste0(
    vapply(
      els[ord],
      function(el) {
        n <- as.integer(atom_counts[[el]])
        paste0(el, if (n > 1L) as.character(n) else "")
      },
      character(1L)
    ),
    collapse = ""
  )
}

#' Subtract one formula from another, returning the residual formula string
#' @keywords internal
subtract_atomic_formula <- function(formula, sub_formula = "H3N") {
  formula_atoms <- parse_atomic_formula(formula)
  sub_atoms <- parse_atomic_formula(sub_formula)
  if (is.null(formula_atoms) || is.null(sub_atoms)) {
    return(NULL)
  }
  for (el in names(sub_atoms)) {
    if (
      !el %in% names(formula_atoms) || formula_atoms[[el]] < sub_atoms[[el]]
    ) {
      return(NULL)
    }
    formula_atoms[[el]] <- as.integer(formula_atoms[[el]] - sub_atoms[[el]])
  }
  formula_atoms <- Filter(function(x) x > 0L, formula_atoms)
  rebuild_hill_formula(unlist(formula_atoms))
}

#' Reduce (+H3N, +H) -> +H4N and (+H4N, -loss-containing-H3N) pairs
#' @keywords internal
reduce_ammonium_loss_pairs <- function(net) {
  if (nrow(net) == 0L) {
    return(net)
  }

  add_or_increment <- function(df, formula, delta) {
    idx <- match(formula, df$formula)
    if (is.na(idx)) {
      df <- data.frame(
        formula = c(df$formula, formula),
        signed_n = c(df$signed_n, as.integer(delta)),
        stringsAsFactors = FALSE
      )
    } else {
      df$signed_n[[idx]] <- as.integer(df$signed_n[[idx]] + delta)
    }
    df
  }

  # Collapse (+H3N, +H) -> +H4N so [M+H3N+H]+ and [M+NH3+H]+ unify with [M+H4N]+
  h3n_idx <- match("H3N", net$formula)
  h_idx <- match("H", net$formula)
  if (
    !is.na(h3n_idx) &&
      !is.na(h_idx) &&
      net$signed_n[[h3n_idx]] > 0L &&
      net$signed_n[[h_idx]] > 0L
  ) {
    k <- min(net$signed_n[[h3n_idx]], net$signed_n[[h_idx]])
    net$signed_n[[h3n_idx]] <- as.integer(net$signed_n[[h3n_idx]] - k)
    net$signed_n[[h_idx]] <- as.integer(net$signed_n[[h_idx]] - k)
    net <- add_or_increment(net, "H4N", k)
    net <- net[net$signed_n != 0L, , drop = FALSE]
  }

  if (!any(net$formula == "H4N" & net$signed_n > 0L)) {
    return(net)
  }

  ammonium_idx <- match("H4N", net$formula)
  while (!is.na(ammonium_idx) && net$signed_n[[ammonium_idx]] > 0L) {
    neg_idx <- which(net$signed_n < 0L)
    if (length(neg_idx) == 0L) {
      break
    }

    reducible_idx <- NA_integer_
    reducible_residual <- NULL
    for (i in neg_idx) {
      residual <- subtract_atomic_formula(net$formula[[i]], "H3N")
      if (!is.null(residual)) {
        reducible_idx <- i
        reducible_residual <- residual
        break
      }
    }
    if (is.na(reducible_idx)) {
      break
    }

    net$signed_n[[reducible_idx]] <- as.integer(
      net$signed_n[[reducible_idx]] + 1L
    )
    net$signed_n[[ammonium_idx]] <- as.integer(
      net$signed_n[[ammonium_idx]] - 1L
    )
    net <- add_or_increment(net, "H", 1L)
    if (!is.null(reducible_residual) && nzchar(reducible_residual)) {
      net <- add_or_increment(net, reducible_residual, -1L)
    }
    net <- net[net$signed_n != 0L, , drop = FALSE]
    ammonium_idx <- match("H4N", net$formula)
  }

  net[net$signed_n != 0L, , drop = FALSE]
}

#' Canonicalize one adduct string
#' @keywords internal
canonicalize_adduct_notation <- local({
  # Cached formulas for carrier ions
  .carrier_formulas <- c(
    "H",
    "H2",
    "H3",
    "H4N",
    "Na",
    "K",
    "Li",
    "Mg",
    "Ca",
    "Fe",
    "Cu"
  )
  .canon_cache <- new.env(hash = TRUE, parent = emptyenv())

  function(adduct) {
    if (length(adduct) == 0L || is.na(adduct) || !nzchar(adduct)) {
      return(adduct)
    }

    cached <- .canon_cache[[adduct]]
    if (!is.null(cached)) {
      return(cached)
    }

    adduct_input <- adduct
    adduct <- gsub("\\s+", "", adduct)

    # Prefer explicit remediation for known unstable patterns.
    if (adduct %in% names(adducts_forbidden_translations)) {
      result <- unname(adducts_forbidden_translations[[adduct]])
      .canon_cache[[adduct_input]] <- result
      return(result)
    }

    m <- regexec("^\\[(.*)\\](\\d*[+-])$", adduct, perl = TRUE)
    g <- regmatches(adduct, m)[[1L]]
    if (length(g) == 0L) {
      .canon_cache[[adduct_input]] <- adduct
      return(adduct)
    }

    inner <- g[[2L]]
    suffix <- g[[3L]]

    # Split core (up to M/isotope part) from formula modifications.
    mod_start <- regexpr("[+-]\\d*[A-Za-z]", inner, perl = TRUE)
    if (mod_start[[1L]] == -1L) {
      .canon_cache[[adduct_input]] <- adduct
      return(adduct)
    }

    core <- substr(inner, 1L, mod_start[[1L]] - 1L)
    mods <- substr(inner, mod_start[[1L]], nchar(inner))

    tok_re <- "([+-])(\\d*)([A-Za-z][A-Za-z0-9]*)"
    tok_matches <- gregexpr(tok_re, mods, perl = TRUE)
    tokens <- regmatches(mods, tok_matches)[[1L]]

    remainder <- gsub(tok_re, "", mods, perl = TRUE)
    if (!identical(remainder, "")) {
      .canon_cache[[adduct_input]] <- adduct
      return(adduct)
    }

    signs <- ifelse(startsWith(tokens, "+"), 1L, -1L)
    tok_body <- substring(tokens, 2L)
    mult_str <- sub("^(\\d*).*$", "\\1", tok_body, perl = TRUE)
    mults <- ifelse(nzchar(mult_str), as.integer(mult_str), 1L)
    formulas <- sub("^\\d*", "", tok_body, perl = TRUE)
    signed_ns <- as.integer(signs * mults)

    u_form <- unique(formulas)
    if (length(u_form) == length(formulas)) {
      net_form <- formulas
      net_n <- signed_ns
    } else {
      net_form <- u_form
      net_n <- integer(length(u_form))
      for (k in seq_along(formulas)) {
        idx_f <- match(formulas[[k]], u_form)
        net_n[[idx_f]] <- net_n[[idx_f]] + signed_ns[[k]]
      }
    }
    keep_nz <- net_n != 0L
    net <- data.frame(
      formula = net_form[keep_nz],
      signed_n = net_n[keep_nz],
      stringsAsFactors = FALSE
    )
    h_mask <- net$formula %in% c("H", "H2", "H3")
    if (
      any(h_mask) &&
        any(net$signed_n[h_mask] > 0L) &&
        any(net$signed_n[h_mask] < 0L)
    ) {
      h_mult <- c(H = 1L, H2 = 2L, H3 = 3L)[net$formula[h_mask]]
      total_h <- sum(net$signed_n[h_mask] * h_mult)
      net <- net[!h_mask, , drop = FALSE]
      if (total_h != 0L) {
        net <- rbind(
          net,
          data.frame(
            formula = "H",
            signed_n = as.integer(total_h),
            stringsAsFactors = FALSE
          )
        )
      }
    }
    net <- reduce_ammonium_loss_pairs(net)

    rebuild_terms <- character(0L)
    if (nrow(net) > 0L) {
      neg <- net[net$signed_n < 0L, , drop = FALSE]
      pos <- net[net$signed_n > 0L, , drop = FALSE]
      if (nrow(neg) > 0L) {
        neg <- neg[order(neg$formula), , drop = FALSE]
        n_neg <- abs(neg$signed_n)
        rebuild_terms <- c(
          rebuild_terms,
          paste0("-", ifelse(n_neg > 1L, as.character(n_neg), ""), neg$formula)
        )
      }
      if (nrow(pos) > 0L) {
        # Keep charge carriers last in positive modifiers so canonical forms read
        # as neutrals/clusters then carrier, e.g. [M+H2O+H]+ (not [M+H+H2O]+).
        # Use cached carrier formulas instead of recreating each call
        pos <- pos[
          order(pos$formula %in% .carrier_formulas, pos$formula),
          ,
          drop = FALSE
        ]
        n_pos <- pos$signed_n
        rebuild_terms <- c(
          rebuild_terms,
          paste0("+", ifelse(n_pos > 1L, as.character(n_pos), ""), pos$formula)
        )
      }
    }

    result <- paste0(
      "[",
      core,
      paste0(rebuild_terms, collapse = ""),
      "]",
      suffix
    )
    .canon_cache[[adduct_input]] <- result
    result
  }
})
