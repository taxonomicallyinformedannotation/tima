#' @title Parse adduct
#'
#' @description This function parses mass spectrometry adduct notation strings
#'     into their components: multimer count, isotope shift, modifications,
#'     charge state, and charge sign. It handles complex adducts with multiple
#'     additions/losses.
#'
#' @param adduct_string Character string representing the adduct in standard
#'     notation (e.g., "\[M+H\]+", "\[2M+Na\]+", "\[M-H2O+H\]+")
#' @param regex Character string regular expression pattern for parsing
#'     (default handles standard adduct notation)
#'
#' @return Named numeric vector containing:
#'   \item{n_mer}{Integer number of monomers (e.g., 2 for dimer)}
#'   \item{n_iso}{Integer isotope shift (e.g., 1 for M1)}
#'   \item{los_add_clu}{Numeric total mass change from modifications}
#'   \item{n_charges}{Integer absolute number of charges}
#'   \item{charge}{Integer charge sign (+1 or -1)}
#'   Returns all zeros if parsing fails.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' parse_adduct("[M+H]+")
#' parse_adduct("[2M+Na]+")
#' parse_adduct("[2M1-C6H12O6 (hexose)+NaCl+H]2+")
#' }
parse_adduct <- function(
  adduct_string,
  regex = "\\[(\\d*)M(?![a-z])(\\d*)([+-][\\w\\d].*)?.*\\](\\d*)([+-])?"
) {
  # Validate input
  if (missing(adduct_string) || is.null(adduct_string) || nchar(adduct_string) == 0L) {
    stop("Adduct string must be provided")
  }

  # Define return value for failed parsing
  failed_parse <- c(
    n_mer = 0,
    n_iso = 0,
    los_add_clu = 0,
    n_charges = 0,
    charge = 0
  )

  # Match adduct string against regex pattern
  matches <- stringi::stri_match_all_regex(
    str = adduct_string,
    pattern = regex
  )

  if (length(matches) > 0L) {
    matches <- matches[[1L]]
  } else {
    matches <- matrix(NA_character_)
  }

  # Check if matching was successful
  if (is.na(matches[1L, 1L])) {
    logger::log_warn("Invalid adduct format: ", adduct_string)
    logger::log_warn("Returning zeros (failed parse)")
    return(failed_parse)
  }

  # Extract multimer count (e.g., 2 in [2M+H]+)
  n_mer <- if (matches[2L] != "") {
    as.integer(matches[2L])
  } else {
    1L
  }

  # Extract isotope shift (e.g., 1 in [M1+H]+)
  n_iso <- if (matches[3L] != "") {
    as.integer(matches[3L])
  } else {
    0L
  }

  # Extract and process modifications (e.g., +H, -H2O, +Na)
  modifications_raw <- matches[4L]

  # Clean up modifications string (remove comments in parentheses)
  modifications_clean <- gsub(
    pattern = " \\(.*?\\)",
    replacement = "",
    x = modifications_raw
  )

  # Define pattern for individual modifications
  mod_pattern <- "[+-](\\d*)"

  # Extract modification elements (e.g., "H", "H2O", "Na")
  mod_elements <- stringi::stri_split_regex(
    str = gsub(pattern = " .*", replacement = "", x = modifications_clean),
    pattern = mod_pattern
  )[[1L]][-1L]

  # Extract modification signs (+1 for addition, -1 for loss)
  mod_signs_raw <- stringi::stri_extract_all_regex(
    str = modifications_clean,
    pattern = mod_pattern
  )[[1L]]

  mod_signs <- ifelse(
    test = gsub(pattern = "\\d*", replacement = "", x = mod_signs_raw) == "+",
    yes = 1L,
    no = -1L
  )

  # Extract modification multiplicities (e.g., 2 in +2H)
  mod_multiplicities_raw <- gsub(
    pattern = "[+-]",
    replacement = "",
    x = mod_signs_raw
  )

  mod_multiplicities <- ifelse(
    test = mod_multiplicities_raw != "",
    yes = as.integer(mod_multiplicities_raw),
    no = 1L
  )

  # Calculate masses of modification elements
  mod_masses <- tryCatch({
    MetaboCoreUtils::calculateMass(mod_elements)
  }, error = function(e) {
    logger::log_warn("Failed to calculate modification masses: ", conditionMessage(e))
    return(failed_parse)
  })

  # If mass calculation failed, mod_masses will be the failed_parse vector
  if (length(mod_masses) == length(failed_parse) && all(names(mod_masses) == names(failed_parse))) {
    return(failed_parse)
  }

  # Calculate total mass change from all modifications
  total_mass_change <- sum(mod_masses * mod_multiplicities * mod_signs)

  # Extract charge state (e.g., 2 in [M+H]2+)
  n_charges <- if (matches[5L] != "") {
    as.integer(matches[5L])
  } else {
    1L
  }

  # Extract charge sign (+1 for positive, -1 for negative)
  charge_sign <- if (matches[6L] == "+") 1L else -1L

  # Validate all extracted values
  if (any(is.na(c(n_mer, n_iso, total_mass_change, n_charges, charge_sign)))) {
    logger::log_warn("Unexpected NA values during adduct parsing")
    return(failed_parse)
  }

  # Return parsed adduct components
  return(c(
    n_mer = n_mer,
    n_iso = n_iso,
    los_add_clu = total_mass_change,
    n_charges = n_charges,
    charge = charge_sign
  ))
}
