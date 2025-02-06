#' @title Parse adduct
#'
#' @description This function parses adducts
#'
#' @param adduct_string Adduct to be parsed
#' @param regex Regex used for parsing
#'
#' @return Parsed elements from adduct
#'
#' @export
#'
#' @examples
#' \dontrun{
#' parse_adduct("[M+H]+")
#' parse_adduct("[2M1-C6H12O6 (hexose)+NaCl+H]2+")
#' }
parse_adduct <- function(adduct_string, regex = "\\[(\\d*)M(?![a-z])(\\d*)([+-][\\w\\d].*)?.*\\](\\d*)([+-])?") {
  ## Full match
  matches <- stringi::stri_match_all_regex(str = adduct_string, pattern = regex)[[1]]

  unexpected <- c(
    "n_mer" = 0,
    "n_iso" = 0,
    "los_add_clu" = 0,
    "n_charges" = 0,
    "charge" = 0
  )
  if (is.na(matches[1, 1])) {
    log_debug("Invalid adduct format")
    log_debug("Returning 0 instead")
    return(unexpected)
  }

  ## Extract n_mer
  n_mer <- ifelse(test = matches[2] != "",
    yes = matches[2] |>
      as.integer(),
    no = 1
  )

  ## Extract n_iso
  n_iso <- ifelse(test = matches[3] != "",
    yes = matches[3] |>
      as.integer(),
    no = 0
  )

  ## Extract modifications
  modifications <- matches[4] |>
    # Safety to allow for things like "[2M1-C6H12O6 (hexose)+NaCl+H]2+"
    gsub(pattern = " .*", replacement = "")

  ## Split modifications
  modifications_regex <- "[+-](\\d*)"
  modifications_elem <- stringi::stri_split_regex(
    str = modifications |>
      # Safety to allow for things like "[2M1-C6H12O6 (hexose)+NaCl+H]2+"
      gsub(pattern = " .*", replacement = ""),
    pattern = modifications_regex
  )[[1]][-1]
  modifications_sign <- stringi::stri_extract_all_regex(str = modifications, pattern = modifications_regex)[[1]] |>
    gsub(pattern = "\\d*", replacement = "")
  modifications_sign <- ifelse(test = modifications_sign == "+",
    yes = 1,
    no = -1
  )
  modifications_mult <- stringi::stri_extract_all_regex(str = modifications, pattern = modifications_regex)[[1]] |>
    gsub(pattern = "[+-]", replacement = "")
  modifications_mult <- ifelse(test = modifications_mult != "",
    yes = modifications_mult |>
      as.integer(),
    no = 1
  )

  ## Calculate mass
  modifications_masses <- modifications_elem |>
    MetaboCoreUtils::calculateMass()

  los_add_clu <- modifications_masses * modifications_mult * modifications_sign
  ## Extract n_charges
  n_charges <- ifelse(test = matches[5] != "",
    yes = matches[5] |>
      as.integer(),
    no = 1
  )

  ## Extract charge
  charge <- matches[6]
  charge <- ifelse(test = charge == "+",
    yes = 1,
    no = -1
  )

  if (any(is.na(c(
    n_mer, n_iso, los_add_clu, n_charges, charge
  )) == TRUE)) {
    log_debug("Something went unexpected with the calculations")
    return(unexpected)
  } else {
    return(
      c(
        "n_mer" = n_mer,
        "n_iso" = n_iso,
        "los_add_clu" = sum(los_add_clu),
        "n_charges" = n_charges,
        "charge" = charge
      )
    )
  }
}
