#' @title Parse adduct
#'
#' @description This function parses mass spectrometry adduct notation strings
#'     into their components: multimer count, isotope shift, modifications,
#'     charge state, and charge sign. It handles complex adducts with multiple
#'     additions/losses.
#'
#' @include constants.R
#' @include validations_utils.R
#'
#' @param adduct_string Character string representing the adduct in standard
#'     notation (e.g., "\[M+H\]+", "\[2M+Na\]+", "\[M-H2O+H\]+")
#' @param regex Character string regular expression pattern for parsing
#'     (default: uses ADDUCT_REGEX_PATTERN from constants)
#'
#' @return Named numeric vector containing:
#'   \item{n_mer}{Integer number of monomers (e.g., 2 for dimer, 1 for monomer)}
#'   \item{n_iso}{Integer isotope shift (e.g., 1 for M+1 isotopologue, 0 for monoisotopic)}
#'   \item{los_add_clu}{Numeric total mass change in Daltons from all modifications}
#'   \item{n_charges}{Integer absolute number of charges (always positive)}
#'   \item{charge}{Integer charge polarity (+1 for positive mode, -1 for negative mode)}
#'   Returns all zeros if parsing fails.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple adducts
#' parse_adduct("[M+H]+") # Protonated molecule
#' parse_adduct("[M-H]-") # Deprotonated molecule
#' parse_adduct("[M+Na]+") # Sodium adduct
#'
#' # Complex adducts
#' parse_adduct("[2M+Na]+") # Dimer with sodium
#' parse_adduct("[M+H-H2O]+") # Protonated with water loss
#' parse_adduct("[M1+H]+") # M+1 isotopologue
#' parse_adduct("[2M1-C6H12O6 (hexose)+NaCl+H]2+") # Complex modification
#'
#' # Error cases
#' parse_adduct(NULL) # Returns all zeros
#' parse_adduct("invalid") # Returns all zeros with warning
#' }
parse_adduct <- function(
  adduct_string,
  regex = ADDUCT_REGEX_PATTERN
) {
  # Define return value for failed parsing (DRY principle)
  failed_parse <- create_failed_parse_result()

  # Early return for NULL or missing input (guard clause pattern)
  if (
    missing(adduct_string) ||
      is.null(adduct_string) ||
      length(adduct_string) == 0L
  ) {
    # log_trace("Adduct string is NULL or missing, returning zero values")
    return(failed_parse)
  }

  # Handle NA values
  if (length(adduct_string) == 1L && is.na(adduct_string)) {
    return(failed_parse)
  }

  # Validate input type and format
  validation_result <- validate_adduct_string(adduct_string)
  if (!validation_result$valid) {
    # Suppress warnings for empty strings (common edge case)
    if (
      length(adduct_string) == 1L &&
        !is.na(adduct_string) &&
        nchar(adduct_string) > 0L
    ) {
      warning(validation_result$message, call. = FALSE)
      log_warn(validation_result$message)
    }
    return(failed_parse)
  }

  # Remove comments in parentheses before checking for alternative notations
  # This prevents splitting on '/' or '|' inside comments
  # e.g., "[M-C6H10O4 (methylpentose/desoxyhexose-H2O)+H]+"
  adduct_no_comments <- remove_comments(adduct_string)

  # Handle alternative adduct notations (e.g., "[M+CH3COO]-/[M-CH3]-")
  # These are common in databases where multiple adduct possibilities exist
  # Note: We check the string WITH comments removed to avoid splitting on
  # delimiters inside parenthetical comments
  if (grepl("[/|]", adduct_no_comments)) {
    alternatives <- strsplit(adduct_no_comments, "[/|]")[[1L]]
    alternatives <- trimws(alternatives)

    # Try each alternative until one parses successfully
    for (alt_adduct in alternatives) {
      if (nchar(alt_adduct) > 0L) {
        result <- parse_single_adduct(alt_adduct, regex, failed_parse)
        if (!all(result == 0)) {
          log_debug(
            "Successfully parsed alternative '",
            alt_adduct,
            "' from composite adduct '",
            adduct_string,
            "'"
          )
          return(result)
        }
      }
    }

    # If none of the alternatives worked, return failed parse silently
    log_debug(
      "Could not parse any alternative from: '",
      adduct_string,
      "'"
    )
    return(failed_parse)
  }

  # Standard single adduct parsing
  # Use original string to preserve any comments for logging, but they will
  # be removed during modification processing
  parse_single_adduct(adduct_string, regex, failed_parse)
}

#' Parse a single adduct (internal helper)
#' @keywords internal
parse_single_adduct <- function(adduct_string, regex, failed_parse) {
  # Parse adduct string using regex
  parse_result <- match_adduct_regex(adduct_string, regex)
  if (!parse_result$valid) {
    msg <- paste0(
      "Invalid adduct format: '",
      adduct_string,
      "'. ",
      "Expected format: [nM<isotope><modifications>]charge (e.g., [M+H]+)"
    )
    log_debug(msg)
    return(failed_parse)
  }

  matches <- parse_result$matches

  # Extract basic components
  n_mer <- extract_multimer_count(matches[2L])
  n_iso <- extract_isotope_shift(matches[3L])
  charge_info <- extract_charge_info(matches[5L], matches[6L])

  # Process modifications
  modifications <- process_modifications(matches[4L])
  if (!modifications$valid) {
    msg <- paste0(
      "Failed to process modifications in: '",
      adduct_string,
      "'. ",
      modifications$message
    )
    log_debug(msg)
    return(failed_parse)
  }

  # Validate all extracted values
  if (
    any(is.na(c(
      n_mer,
      n_iso,
      modifications$total_mass,
      charge_info$n_charges,
      charge_info$charge_sign
    )))
  ) {
    msg <- paste0(
      "Unexpected NA values during adduct parsing for: '",
      adduct_string,
      "'"
    )
    log_debug(msg)
    return(failed_parse)
  }

  # Log successful parse at trace level
  # log_trace(
  #   "Successfully parsed adduct '",
  #   adduct_string,
  #   "': ",
  #   "n_mer=",
  #   n_mer,
  #   ", n_iso=",
  #   n_iso,
  #   ", mass_change=",
  #   round(modifications$total_mass, 4),
  #   ", charges=",
  #   charge_info$n_charges,
  #   ", polarity=",
  #   charge_info$charge_sign
  # )

  # Return parsed components
  return(c(
    n_mer = n_mer,
    n_iso = n_iso,
    los_add_clu = modifications$total_mass,
    n_charges = charge_info$n_charges,
    charge = charge_info$charge_sign
  ))
}

# Helper Functions ----
# These functions implement Single Responsibility Principle

#' Create failed parse result
#' @keywords internal
create_failed_parse_result <- function() {
  c(
    n_mer = 0L,
    n_iso = 0L,
    los_add_clu = 0,
    n_charges = 0L,
    charge = 0L
  )
}

#' Validate adduct string format
#' @keywords internal
validate_adduct_string <- function(adduct_string) {
  if (!is.character(adduct_string)) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Adduct string must be character type, got: ",
        class(adduct_string)[1L]
      )
    ))
  }

  if (length(adduct_string) != 1L) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Adduct string must be a single value, got length: ",
        length(adduct_string)
      )
    ))
  }

  if (nchar(adduct_string) == 0L) {
    return(list(
      valid = FALSE,
      message = "Adduct string cannot be empty"
    ))
  }

  list(valid = TRUE, message = "")
}

#' Match adduct string against regex pattern
#' @keywords internal
match_adduct_regex <- function(adduct_string, regex) {
  matches <- tryCatch(
    {
      stringi::stri_match_all_regex(
        str = adduct_string,
        pattern = regex
      )
    },
    error = function(e) {
      log_error(
        "Regex matching failed: ",
        conditionMessage(e)
      )
      return(NULL)
    }
  )

  if (is.null(matches)) {
    return(list(valid = FALSE, matches = NULL))
  }

  if (length(matches) > 0L) {
    matches <- matches[[1L]]
  } else {
    matches <- matrix(NA_character_)
  }

  # Check if matching was successful
  if (is.na(matches[1L, 1L])) {
    return(list(valid = FALSE, matches = NULL))
  }

  list(valid = TRUE, matches = matches)
}

#' Extract multimer count from regex match
#' @keywords internal
extract_multimer_count <- function(match_value) {
  if (is.na(match_value) || match_value == "") {
    return(1L)
  }
  as.integer(match_value)
}

#' Extract isotope shift from regex match
#' @keywords internal
extract_isotope_shift <- function(match_value) {
  if (is.na(match_value) || match_value == "") {
    return(0L)
  }
  as.integer(match_value)
}

#' Extract charge information from regex matches
#' @keywords internal
extract_charge_info <- function(charge_count_match, charge_sign_match) {
  n_charges <- if (is.na(charge_count_match) || charge_count_match == "") {
    1L
  } else {
    as.integer(charge_count_match)
  }

  charge_sign <- if (is.na(charge_sign_match) || charge_sign_match == "+") {
    1L
  } else {
    -1L
  }

  list(
    n_charges = n_charges,
    charge_sign = charge_sign
  )
}

#' Process adduct modifications and calculate total mass change
#' @keywords internal
process_modifications <- function(modifications_raw) {
  # Handle case with no modifications
  if (is.na(modifications_raw) || modifications_raw == "") {
    return(list(
      valid = TRUE,
      total_mass = 0,
      message = ""
    ))
  }

  # Clean up modifications string (remove comments in parentheses)
  modifications_clean <- remove_comments(modifications_raw)

  # Parse modifications into components
  mod_components <- parse_modification_components(modifications_clean)
  if (!mod_components$valid) {
    return(list(
      valid = FALSE,
      total_mass = 0,
      message = mod_components$message
    ))
  }

  # Calculate masses of modification elements
  mass_result <- calculate_modification_masses(
    mod_components$elements,
    mod_components$signs,
    mod_components$multiplicities
  )

  if (!mass_result$valid) {
    return(list(
      valid = FALSE,
      total_mass = 0,
      message = mass_result$message
    ))
  }

  list(
    valid = TRUE,
    total_mass = mass_result$total_mass,
    message = ""
  )
}

#' Remove comments from modification string
#' @keywords internal
remove_comments <- function(modification_string) {
  gsub(
    pattern = " \\(.*?\\)",
    replacement = "",
    x = modification_string,
    fixed = FALSE
  )
}

#' Parse modification components (elements, signs, multiplicities)
#' @keywords internal
parse_modification_components <- function(modifications_clean) {
  # Pattern for individual modifications
  mod_pattern <- "[+-](\\d*)"

  # Extract modification elements (e.g., "H", "H2O", "Na")
  mod_elements <- tryCatch(
    {
      stringi::stri_split_regex(
        str = gsub(pattern = " .*", replacement = "", x = modifications_clean),
        pattern = mod_pattern
      )[[1L]][-1L]
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (is.null(mod_elements) || length(mod_elements) == 0L) {
    return(list(
      valid = FALSE,
      message = "Could not extract modification elements"
    ))
  }

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

  list(
    valid = TRUE,
    elements = mod_elements,
    signs = mod_signs,
    multiplicities = mod_multiplicities,
    message = ""
  )
}

#' Calculate total mass change from modifications
#' @keywords internal
calculate_modification_masses <- function(elements, signs, multiplicities) {
  mod_masses <- tryCatch(
    {
      MetaboCoreUtils::calculateMass(x = elements)
    },
    error = function(e) {
      log_debug(
        "Failed to calculate modification masses: ",
        conditionMessage(e)
      )
      return(NULL)
    }
  )

  if (is.null(mod_masses)) {
    return(list(
      valid = FALSE,
      total_mass = 0,
      message = "Mass calculation failed for modification elements"
    ))
  }

  # Calculate total mass change from all modifications
  total_mass_change <- sum(mod_masses * multiplicities * signs)

  list(
    valid = TRUE,
    total_mass = total_mass_change,
    message = ""
  )
}
