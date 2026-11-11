#' @title Typed adduct hypothesis system
#'
#' @description
#' This module is the structured, regex-free replacement for the historical
#' string-concatenation logic used to enumerate adduct hypotheses and to
#' compute neutral masses.
#'
#' Adducts are modelled as **orthogonal components**:
#'
#'   * `n_mer`   : oligomer multiplicity (positive integer, typically 1..3)
#'   * `carriers`: named integer vector of charge carriers (e.g. c(H = 2, Na = 1))
#'   * `clusters`: named integer vector of neutral adducts (e.g. c(H2O = 1))
#'   * `losses`  : named integer vector of neutral losses (e.g. c(H2O = 1))
#'   * `z`       : signed net charge of the resulting ion
#'
#' The canonical adduct string is generated **from** these components, never
#' parsed **into** them inside the pipeline. All mass arithmetic happens on
#' the pre-computed `adduct_mass` offset (independent of m/z and n_mer).
#'
#' Mass formula (signed, regex-free):
#'   m/z = (n_mer * M + adduct_mass) / |z|  (with adduct_mass including
#'                                            electron-mass correction: -z*m_e)
#'   M   = (|z| * m/z - adduct_mass) / n_mer
#'
#' This is physically accurate for charged ions and independent of adduct text
#' parsing.
#'
#' @include constants.R
#' @keywords internal
#' @name adduct_universe
NULL

# ---------------------------------------------------------------------------
# Element / ion lookup tables (monoisotopic, neutral atoms / radicals)
# Sources: MetaboCoreUtils element masses / CODATA 2018 (electron mass).
# ---------------------------------------------------------------------------

#' Monoisotopic neutral mass (Da) for the atomic / radical building blocks
#' used to compose charge carriers.
#' @keywords internal
ATOMIC_MONOISOTOPIC_MASS <- c(
  # Same values as calculate_formula_mass(), so that carrier masses
  # and library exact masses come from one element table
  H = 1.007825032,
  Li = 7.01600455,
  B = 11.0093054,
  C = 12.0000000,
  N = 14.003074,
  O = 15.99491462,
  F = 18.99840322,
  Na = 22.98976928,
  Mg = 23.9850417,
  Al = 26.98153863,
  Si = 27.97692653,
  P = 30.97376163,
  S = 31.972071,
  Cl = 34.96885268,
  K = 38.96370668,
  Ca = 39.96259098,
  Mn = 54.9380451,
  Fe = 55.9349375,
  Cu = 62.9295975,
  Zn = 63.9291422,
  Br = 78.9183371,
  I = 126.904473
)

#' Intrinsic ion charge for charge-carrier *names* as declared in YAML.
#' Compound carriers like "Na-2H" are decomposed by [parse_carrier_token()].
#' @keywords internal
CARRIER_INTRINSIC_CHARGE <- c(
  H = 1L,
  Na = 1L,
  K = 1L,
  Li = 1L,
  H4N = 1L,
  Ca = 2L,
  Mg = 2L,
  Zn = 2L,
  Fe = 3L,
  Cu = 1L,
  Mn = 2L,
  Al = 3L,
  F = -1L,
  Cl = -1L,
  Br = -1L,
  I = -1L
)

#' Electron rest mass (Da), CODATA 2018.
#' @keywords internal
ELECTRON_MASS_DA <- ELECTRON_MASS_DALTONS

# ---------------------------------------------------------------------------
# Formula and carrier parsing (regex used ONLY on atomic-formula tokens, never
# on adduct strings).
# ---------------------------------------------------------------------------

#' Parse a chemical formula like "H2O" or "C2H3N" into a named integer vector
#' of element counts. Returns NULL if the formula cannot be parsed.
#'
#' @keywords internal
parse_atomic_formula <- function(formula) {
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) {
    return(integer())
  }
  m <- gregexpr("([A-Z][a-z]?)([0-9]*)", formula, perl = TRUE)
  matches <- regmatches(formula, m)[[1L]]
  matches <- matches[nzchar(matches)]
  els <- sub("^([A-Z][a-z]?).*$", "\\1", matches, perl = TRUE)
  if (length(els) == 0L || !all(els %in% names(ATOMIC_MONOISOTOPIC_MASS))) {
    return(NULL)
  }
  cnt_str <- sub("^[A-Z][a-z]?([0-9]*)$", "\\1", matches, perl = TRUE)
  cnts <- ifelse(nzchar(cnt_str), as.integer(cnt_str), 1L)
  u_els <- unique(els)
  vapply(
    split(cnts, factor(els, levels = u_els)),
    sum,
    integer(1L)
  )
}

#' Compute the monoisotopic mass of a parsed atomic-formula vector.
#' @keywords internal
formula_mass <- function(formula_vec) {
  if (length(formula_vec) == 0L) {
    return(0)
  }
  sum(ATOMIC_MONOISOTOPIC_MASS[names(formula_vec)] * formula_vec)
}

#' Parse a single carrier token like "H", "Na", "H4N" or compound forms
#' like "Na-2H", "Fe-2H", "K-2H" into:
#'   list(symbols = c(symbol = signed_count, ...), z_contribution = integer)
#'
#' The signed counts represent additions/subtractions of charge carriers
#' (so "Na-2H" -> c(Na = 1, H = -2), contributing +1 -2 = -1 to z).
#'
#' Compound carriers MUST start with a positive ion and then list
#' `+/-N<symbol>` segments.
#'
#' Returns NULL for tokens that cannot be parsed.
#'
#' @keywords internal
parse_carrier_token <- function(token) {
  token <- as.character(token)
  if (length(token) == 0L || is.na(token)) {
    return(NULL)
  }
  parts <- regmatches(
    token,
    gregexpr("([+-]?)([0-9]*)([A-Za-z][A-Za-z0-9]*)", token, perl = TRUE)
  )[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(NULL)
  }

  signs <- ifelse(startsWith(parts, "-"), -1L, 1L)
  bodies <- sub("^[+-]", "", parts, perl = TRUE)
  coef_str <- sub("^([0-9]*).*$", "\\1", bodies, perl = TRUE)
  coefs <- ifelse(nzchar(coef_str), as.integer(coef_str), 1L)
  syms <- sub("^[0-9]+", "", bodies, perl = TRUE)
  syms[syms == "NH4"] <- "H4N"
  if (!all(syms %in% names(CARRIER_INTRINSIC_CHARGE))) {
    return(NULL)
  }
  signed_coefs <- as.integer(signs * coefs)
  u_syms <- unique(syms)
  symbols <- vapply(
    split(signed_coefs, factor(syms, levels = u_syms)),
    sum,
    integer(1L)
  )
  z <- sum(signed_coefs * unname(CARRIER_INTRINSIC_CHARGE[syms]))
  list(symbols = symbols, z_contribution = as.integer(z))
}

#' Compute the neutral monoisotopic mass of a (possibly compound) carrier
#' token. For "H4N" (ammonium) we sum 4*H + N; for "Na-2H" we add Na and
#' subtract 2*H, etc.
#' @keywords internal
carrier_token_mass <- function(parsed_carrier) {
  if (is.null(parsed_carrier)) {
    return(NA_real_)
  }
  syms <- names(parsed_carrier$symbols)
  cnts <- as.numeric(parsed_carrier$symbols)
  nh4_mass <- 4 *
    ATOMIC_MONOISOTOPIC_MASS[["H"]] +
    ATOMIC_MONOISOTOPIC_MASS[["N"]]
  sym_masses <- ifelse(
    syms == "H4N",
    nh4_mass,
    unname(ATOMIC_MONOISOTOPIC_MASS[syms])
  )
  if (anyNA(sym_masses)) {
    return(NA_real_)
  }
  sum(cnts * sym_masses)
}

# ---------------------------------------------------------------------------
# Canonical string serialisation
# ---------------------------------------------------------------------------

#' Format a cluster or loss component vector (always positive counts).
#' `sign_char` is "+" for clusters, "-" for losses.
#' @keywords internal
.format_neutrals <- function(vec, sign_char) {
  if (length(vec) == 0L) {
    return("")
  }
  vec <- vec[vec > 0L]
  if (length(vec) == 0L) {
    return("")
  }
  ord <- order(-vec, names(vec))
  vec <- vec[ord]
  paste0(
    sign_char,
    ifelse(vec == 1L, "", as.character(vec)),
    names(vec),
    collapse = ""
  )
}

#' Format all negative modification terms (neutral losses + negative carrier
#' components) in canonical order: alphabetical by formula name.
#'
#' Canonical ordering rule: all negatives come FIRST in the adduct body.
#'
#' @param losses Named integer vector of neutral-loss counts (positive values).
#' @param neg_carriers Named integer vector of carrier atoms with negative
#'   signed counts (e.g. `c(H = -2L)` for deprotonation).
#'
#' @return Collapsed character string like `"-H2O-2H"`.
#' @keywords internal
.format_neg_terms <- function(losses, neg_carriers) {
  # Collect: neutral losses (stored as positive count) + abs of neg carriers
  terms <- integer()
  if (length(losses) > 0L) {
    terms <- c(terms, losses[losses > 0L])
  }
  if (length(neg_carriers) > 0L) {
    terms <- c(terms, abs(neg_carriers))
  }
  if (length(terms) == 0L) {
    return("")
  }
  # Sort alphabetically by formula name (matches canonicalize_adduct_notation)
  terms <- terms[order(names(terms))]
  paste0(
    "-",
    ifelse(terms == 1L, "", as.character(terms)),
    names(terms),
    collapse = ""
  )
}

#' Format all positive modification terms (neutral clusters then positive
#' carrier components) in canonical order.
#'
#' Canonical ordering rule: neutral cluster additions come BEFORE charge
#' carriers (matching \code{canonicalize_adduct_notation()}).
#'
#' @param clusters Named integer vector of neutral cluster counts.
#' @param pos_carriers Named integer vector of carrier atoms with positive
#'   signed counts (e.g. `c(H = 1L, Na = 1L)`).
#'
#' @return Collapsed character string like `"+H2O+H"`.
#' @keywords internal
.format_pos_terms <- function(clusters, pos_carriers) {
  parts <- character(0L)
  # Neutral clusters first, alphabetical
  cl <- if (length(clusters) > 0L) clusters[clusters > 0L] else integer()
  if (length(cl) > 0L) {
    cl <- cl[order(names(cl))]
    parts <- c(
      parts,
      paste0("+", ifelse(cl == 1L, "", as.character(cl)), names(cl))
    )
  }
  # Positive carriers last, alphabetical
  ca <- if (length(pos_carriers) > 0L) {
    pos_carriers[pos_carriers > 0L]
  } else {
    integer()
  }
  if (length(ca) > 0L) {
    ca <- ca[order(names(ca))]
    parts <- c(
      parts,
      paste0("+", ifelse(ca == 1L, "", as.character(ca)), names(ca))
    )
  }
  paste(parts, collapse = "")
}
