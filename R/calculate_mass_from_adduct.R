#' @title Parse adduct
#'
#' @description This function parses adducts
#'
#' @include parse_adduct.R
#'
#' @param adduct_string Adduct to be parsed
#' @param mass Mass
#' @param electron_mass Electron mass
#'
#' @return An exact mass
#'
#' @export
#'
#' @examples NULL
calculate_mass_from_adduct <- function(adduct_string, mass, electron_mass = 5.485799E-4) {
  parsed_adduct <- parse_adduct(adduct_string)
  return(((
    parsed_adduct["n_mer"] *
      (mass +
        parsed_adduct["n_iso"]) -
      parsed_adduct["los_add_clu"] -
      (parsed_adduct["n_charges"] *
        parsed_adduct["charge"] *
        electron_mass)
  ) /
    parsed_adduct["n_charges"])[[1]])
}
