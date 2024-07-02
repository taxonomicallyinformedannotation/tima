#' @title Create adducts
#'
#' @description This function creates adducts
#'
#' @param masses_table Table containing the masses of the adducts
#' @param adducts_table Table containing the adducts
#' @param ms_mode MS mode
#'
#' @return A tidytable with columns "adduct" and "adduct_mass"
#'
#' @export
#'
#' @examples NULL
create_adducts <- function(masses_table = get("masses_table",
                             envir = parent.frame()
                           ),
                           adducts_table = get("adducts_table",
                             envir = parent.frame()
                           ),
                           ms_mode) {
  ## Calculate the masses for various positive adducts
  if (ms_mode == "pos") {
    adducts <- masses_table |>
      ## simple adducts
      tidytable::mutate(
        `[M+H3]3+` = (exact_mass + 3 * proton) / 3,
        `[M+H2Na]3+` = (exact_mass + 2 * proton + sodium) / 3,
        `[M+HNa2]3+` = (exact_mass + proton + 2 * sodium) / 3,
        `[M+Na3]3+` = (exact_mass + 3 * sodium) / 3,
        `[M+H2]2+` = (exact_mass + 2 * proton) / 2,
        `[M+HNa]2+` = (exact_mass + proton + sodium) / 2,
        `[M+Mg]2+` = (exact_mass + magnesium) / 2,
        `[M+HK]2+` = (exact_mass + proton + potassium) / 2,
        `[M+Ca]2+` = (exact_mass + calcium) / 2,
        `[M+Na2]2+` = (exact_mass + 2 * sodium) / 2,
        `[M+Fe]2+` = (exact_mass + iron) / 2,
        `[M+H]+` = exact_mass + proton,
        `[M+H4N]+` = exact_mass + ammonium,
        `[M+Na]+` = exact_mass + sodium,
        `[M+K]+` = exact_mass + potassium,
        `[M+Cu]+` = exact_mass + copper,
        `[2M+Mg]2+` = (2 * exact_mass + magnesium) / 2,
        `[2M+Ca]2+` = (2 * exact_mass + calcium) / 2,
        `[2M+Fe]2+` = (2 * exact_mass + iron) / 2,
        `[2M+H]+` = 2 * exact_mass + proton,
        `[2M+H4N]+` = 2 * exact_mass + ammonium,
        `[2M+Na]+` = 2 * exact_mass + sodium,
        `[2M+K]+` = 2 * exact_mass + potassium
      ) |>
      ## clusters
      tidytable::mutate(
        `[M+H2O+H]+` = `[M+H]+` + water,
        `[M+H2O+Na]+` = `[M+Na]+` + water,
        `[M+H2O+K]+` = `[M+K]+` + water,
        `[M+H2O+Cu]+` = `[M+Cu]+` + water,
        `[M+CH4O+H]+` = `[M+H]+` + methanol,
        `[M+CH4O+Na]+` = `[M+Na]+` + methanol,
        `[M+CH4O+K]+` = `[M+K]+` + methanol,
        `[M+CH4O+Cu]+` = `[M+Cu]+` + methanol,
        `[M+C2H3N+H]+` = `[M+H]+` + acetonitrile,
        `[M+C2H3N+Na]+` = `[M+Na]+` + acetonitrile,
        `[M+C2H3N+K]+` = `[M+K]+` + acetonitrile,
        `[M+C2H3N+Cu]+` = `[M+Cu]+` + acetonitrile,
        `[M+C2H7N+H]+` = `[M+H]+` + ethylamine,
        `[M+C2H7N+Na]+` = `[M+Na]+` + ethylamine,
        `[M+C2H7N+K]+` = `[M+K]+` + ethylamine,
        `[M+C2H7N+Cu]+` = `[M+Cu]+` + ethylamine,
        `[M+C2H6O+H]+` = `[M+H]+` + ethanol,
        `[M+C2H6O+Na]+` = `[M+Na]+` + ethanol,
        `[M+C2H6O+K]+` = `[M+K]+` + ethanol,
        `[M+C2H6O+Cu]+` = `[M+Cu]+` + ethanol,
        `[M+NaCl+H]+` = `[M+H]+` + `sodium chloride`,
        `[M+NaCl+Na]+` = `[M+Na]+` + `sodium chloride`,
        `[M+NaCl+K]+` = `[M+K]+` + `sodium chloride`,
        `[M+NaCl+Cu]+` = `[M+Cu]+` + `sodium chloride`,
        `[M+C3H8O+H]+` = `[M+H]+` + isopropanol,
        `[M+C3H8O+Na]+` = `[M+Na]+` + isopropanol,
        `[M+C3H8O+K]+` = `[M+K]+` + isopropanol,
        `[M+C3H8O+Cu]+` = `[M+Cu]+` + isopropanol,
        `[M+C2H6OS+H]+` = `[M+H]+` + dmso,
        `[M+C2H6OS+Na]+` = `[M+Na]+` + dmso,
        `[M+C2H6OS+K]+` = `[M+K]+` + dmso,
        `[M+C2H6OS+Cu]+` = `[M+Cu]+` + dmso
      )
  }
  if (ms_mode == "neg") {
    adducts <- masses_table |>
      ## simple adducts
      tidytable::mutate(
        `[M-H3]3-` = (exact_mass - 3 * proton) / 3,
        `[M-H2]2-` = (exact_mass - 2 * proton) / 2,
        `[M-H]-` = exact_mass - proton,
        `[M+F]-` = exact_mass + fluorine,
        `[M+Na-H2]-` = exact_mass + sodium - 2 * proton,
        `[M+Cl]-` = exact_mass + chlorine,
        `[M+K-H2]-` = exact_mass + potassium - 2 * proton,
        `[M+Br]-` = exact_mass + bromine,
        `[2M-H]-` = 2 * exact_mass - proton,
        `[3M-H]-` = 3 * exact_mass - proton
      ) |>
      ## clusters
      tidytable::mutate(
        `[M+H2O-H]-` = `[M-H]-` + water,
        `[M+H2O+F]-` = `[M+F]-` + water,
        `[M+H2O+Na-H2]-` = `[M+Na-H2]-` + water,
        `[M+H2O+Cl]-` = `[M+Cl]-` + water,
        `[M+H2O+K-H2]-` = `[M+K-H2]-` + water,
        `[M+H2O+Br]-` = `[M+Br]-` + water,
        `[M+CH2O2-H]-` = `[M-H]-` + formic,
        `[M+CH2O2+F]-` = `[M+F]-` + formic,
        `[M+CH2O2+Na-H2]-` = `[M+Na-H2]-` + formic,
        `[M+CH2O2+Cl]-` = `[M+Cl]-` + formic,
        `[M+CH2O2+K-H2]-` = `[M+K-H2]-` + formic,
        `[M+CH2O2+Br]-` = `[M+Br]-` + formic,
        `[M+NaCl-H]-` = `[M-H]-` + `sodium chloride`,
        `[M+NaCl+F]-` = `[M+F]-` + `sodium chloride`,
        `[M+NaCl+Na-H2]-` = `[M+Na-H2]-` + `sodium chloride`,
        `[M+NaCl+Cl]-` = `[M+Cl]-` + `sodium chloride`,
        `[M+NaCl+K-H2]-` = `[M+K-H2]-` + `sodium chloride`,
        `[M+NaCl+Br]-` = `[M+Br]-` + `sodium chloride`,
        `[M+C2H4O2-H]-` = `[M-H]-` + acetic,
        `[M+C2H4O2+F]-` = `[M+F]-` + acetic,
        `[M+C2H4O2+Na-H2]-` = `[M+Na-H2]-` + acetic,
        `[M+C2H4O2+Cl]-` = `[M+Cl]-` + acetic,
        `[M+C2H4O2+K-H2]-` = `[M+K-H2]-` + acetic,
        `[M+C2H4O2+Br]-` = `[M+Br]-` + acetic,
        `[M+H2PO4-H]-` = `[M-H]-` + phosphoric,
        `[M+H2PO4+F]-` = `[M+F]-` + phosphoric,
        `[M+H2PO4+Na-H2]-` = `[M+Na-H2]-` + phosphoric,
        `[M+H2PO4+Cl]-` = `[M+Cl]-` + phosphoric,
        `[M+H2PO4+K-H2]-1` = `[M+K-H2]-` + phosphoric,
        `[M+H2PO4+Br]-` = `[M+Br]-` + phosphoric,
        `[M+C2HF3O2-H]-` = `[M-H]-` + tfa,
        `[M+C2HF3O2+F]-` = `[M+F]-` + tfa,
        `[M+C2HF3O2+Na-H2]-` = `[M+Na-H2]-` + tfa,
        `[M+C2HF3O2+Cl]-` = `[M+Cl]-` + tfa,
        `[M+C2HF3O2+K-H2]-` = `[M+K-H2]-` + tfa,
        `[M+C2HF3O2+Br]-` = `[M+Br]-` + tfa
      )
  }

  ## Pivot the adducts table to get a long format
  ## with adduct and adduct mass as columns
  adducts <- adducts |>
    tidytable::select(-colnames(adducts_table)) |>
    tidytable::pivot_longer(
      -exact_mass,
      names_to = "adduct",
      values_to = "adduct_mass"
    )

  return(adducts)
}

## See https://github.com/markfairbanks/tidytable/issues/269
.datatable.aware <- TRUE
