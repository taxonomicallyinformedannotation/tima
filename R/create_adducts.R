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
        `[1M+(H)3]3+` = (exact_mass + 3 * proton) / 3,
        `[1M+(H)2(Na)1]3+` = (exact_mass + 2 * proton + sodium) / 3,
        `[1M+(H)1(Na)2]3+` = (exact_mass + proton + 2 * sodium) / 3,
        `[1M+(Na)3]3+` = (exact_mass + 3 * sodium) / 3,
        `[1M+(H)2]2+` = (exact_mass + 2 * proton) / 2,
        `[1M+(H)1(Na)1]2+` = (exact_mass + proton + sodium) / 2,
        `[1M+(Mg)1]2+` = (exact_mass + magnesium) / 2,
        `[1M+(H)1(K)1]2+` = (exact_mass + proton + potassium) / 2,
        `[1M+(Ca)1]2+` = (exact_mass + calcium) / 2,
        `[1M+(Na)2]2+` = (exact_mass + 2 * sodium) / 2,
        `[1M+(Fe)1]2+` = (exact_mass + iron) / 2,
        `[1M+(H)1]1+` = exact_mass + proton,
        `[1M+(Na)1]1+` = exact_mass + sodium,
        `[1M+(K)1]1+` = exact_mass + potassium,
        `[1M+(Cu)1]1+` = exact_mass + copper,
        `[2M+(Mg)1]2+` = (2 * exact_mass + magnesium) / 2,
        `[2M+(Ca)1]2+` = (2 * exact_mass + calcium) / 2,
        `[2M+(Fe)1]2+` = (2 * exact_mass + iron) / 2,
        `[2M+(H)1]1+` = 2 * exact_mass + proton,
        `[2M+(Na)1]1+` = 2 * exact_mass + sodium,
        `[2M+(K)1]1+` = 2 * exact_mass + potassium
      ) |>
      ## clusters
      tidytable::mutate(
        `[1M+(H)1]1+ + (NH3)1` = `[1M+(H)1]1+` + ammonia,
        `[1M+(Na)1]1+ + (NH3)1` = `[1M+(Na)1]1+` + ammonia,
        `[1M+(K)1]1+ + (NH3)1` = `[1M+(K)1]1+` + ammonia,
        `[1M+(Cu)1]1+ + (NH3)1` = `[1M+(Cu)1]1+` + ammonia,
        `[1M+(H)1]1+ + (H2O)1` = `[1M+(H)1]1+` + water,
        `[1M+(Na)1]1+ + (H2O)1` = `[1M+(Na)1]1+` + water,
        `[1M+(K)1]1+ + (H2O)1` = `[1M+(K)1]1+` + water,
        `[1M+(Cu)1]1+ + (H2O)1` = `[1M+(Cu)1]1+` + water,
        # `[1M+(H)1]1+ + (CH4O)1` = `[1M+(H)1]1+` + methanol,
        # `[1M+(Na)1]1+ + (CH4O)1` = `[1M+(Na)1]1+` + methanol,
        # `[1M+(K)1]1+ + (CH4O)1` = `[1M+(K)1]1+` + methanol,
        # `[1M+(Cu)1]1+ + (CH4O)1` = `[1M+(Cu)1]1+` + methanol,
        `[1M+(H)1]1+ + (C2H3N)1` = `[1M+(H)1]1+` + acetonitrile,
        `[1M+(Na)1]1+ + (C2H3N)1` = `[1M+(Na)1]1+` + acetonitrile,
        `[1M+(K)1]1+ + (C2H3N)1` = `[1M+(K)1]1+` + acetonitrile,
        `[1M+(Cu)1]1+ + (C2H3N)1` = `[1M+(Cu)1]1+` + acetonitrile,
        # `[1M+(H)1]1+ + (C2H7N)1` = `[1M+(H)1]1+` + ethylamine,
        # `[1M+(Na)1]1+ + (C2H7N)1` = `[1M+(Na)1]1+` + ethylamine,
        # `[1M+(K)1]1+ + (C2H7N)1` = `[1M+(K)1]1+` + ethylamine,
        # `[1M+(Cu)1]1+ + (C2H7N)1` = `[1M+(Cu)1]1+` + ethylamine,
        # `[1M+(H)1]1+ + (C2H6O)1` = `[1M+(H)1]1+` + ethanol,
        # `[1M+(Na)1]1+ + (C2H6O)1` = `[1M+(Na)1]1+` + ethanol,
        # `[1M+(K)1]1+ + (C2H6O)1` = `[1M+(K)1]1+` + ethanol,
        # `[1M+(Cu)1]1+ + (C2H6O)1` = `[1M+(Cu)1]1+` + ethanol,
        `[1M+(H)1]1+ + (NaCl)1` = `[1M+(H)1]1+` + `sodium chloride`,
        `[1M+(Na)1]1+ + (NaCl)1` = `[1M+(Na)1]1+` + `sodium chloride`,
        `[1M+(K)1]1+ + (NaCl)1` = `[1M+(K)1]1+` + `sodium chloride`,
        `[1M+(Cu)1]1+ + (NaCl)1` = `[1M+(Cu)1]1+` + `sodium chloride`
        # `[1M+(H)1]1+ + (C3H8O)1` = `[1M+(H)1]1+` + isopropanol,
        # `[1M+(Na)1]1+ + (C3H8O)1` = `[1M+(Na)1]1+` + isopropanol,
        # `[1M+(K)1]1+ + (C3H8O)1` = `[1M+(K)1]1+` + isopropanol,
        # `[1M+(Cu)1]1+ + (C3H8O)1` = `[1M+(Cu)1]1+` + isopropanol,
        # `[1M+(H)1]1+ + (C2H6OS)1` = `[1M+(H)1]1+` + dmso,
        # `[1M+(Na)1]1+ + (C2H6OS)1` = `[1M+(Na)1]1+` + dmso,
        # `[1M+(K)1]1+ + (C2H6OS)1` = `[1M+(K)1]1+` + dmso,
        # `[1M+(Cu)1]1+ + (C2H6OS)1` = `[1M+(Cu)1]1+` + dmso
      )
  }
  if (ms_mode == "neg") {
    adducts <- masses_table |>
      ## simple adducts
      tidytable::mutate(
        `[1M-(H)3]3-` = (exact_mass - 3 * proton) / 3,
        `[1M-(H)2]2-` = (exact_mass - 2 * proton) / 2,
        `[1M-(H)1]1-` = exact_mass - proton,
        `[1M+(F)1]1-` = exact_mass + fluorine,
        `[1M+(Na)1-(H)2]1-` = exact_mass + sodium - 2 * proton,
        `[1M+(Cl)1]1-` = exact_mass + chlorine,
        `[1M+(K)1-(H)2]1-` = exact_mass + potassium - 2 * proton,
        `[1M+(Br)1]1-` = exact_mass + bromine,
        `[2M-(H)1]1-` = 2 * exact_mass - proton,
        `[3M-(H)1]1-` = 3 * exact_mass - proton
      ) |>
      ## clusters
      tidytable::mutate(
        `[1M-(H)1]1- + (H2O)1` = `[1M-(H)1]1-` - water,
        `[1M+(F)1]1- + (H2O)1` = `[1M+(F)1]1-` + water,
        `[1M+(Na)1-(H)2]1- + (H2O)1` = `[1M+(Na)1-(H)2]1-` + water,
        `[1M+(Cl)1]1- + (H2O)1` = `[1M+(Cl)1]1-` + water,
        `[1M+(K)1-(H)2]1- + (H2O)1` = `[1M+(K)1-(H)2]1-` + water,
        `[1M+(Br)1]1- + (H2O)1` = `[1M+(Br)1]1-` + water,
        `[1M-(H)1]1- + (CH2O2)1` = `[1M-(H)1]1-` - formic,
        `[1M+(F)1]1- + (CH2O2)1` = `[1M+(F)1]1-` + formic,
        `[1M+(Na)1-(H)2]1- + (CH2O2)1` = `[1M+(Na)1-(H)2]1-` + formic,
        `[1M+(Cl)1]1- + (CH2O2)1` = `[1M+(Cl)1]1-` + formic,
        `[1M+(K)1-(H)2]1- + (CH2O2)1` = `[1M+(K)1-(H)2]1-` + formic,
        `[1M+(Br)1]1- + (CH2O2)1` = `[1M+(Br)1]1-` + formic,
        `[1M-(H)1]1- + (NaCl)1` = `[1M-(H)1]1-` - `sodium chloride`,
        `[1M+(F)1]1- + (NaCl)1` = `[1M+(F)1]1-` + `sodium chloride`,
        `[1M+(Na)1-(H)2]1- + (NaCl)1` = `[1M+(Na)1-(H)2]1-` + `sodium chloride`,
        `[1M+(Cl)1]1- + (NaCl)1` = `[1M+(Cl)1]1-` + `sodium chloride`,
        `[1M+(K)1-(H)2]1- + (NaCl)1` = `[1M+(K)1-(H)2]1-` + `sodium chloride`,
        `[1M+(Br)1]1- + (NaCl)1` = `[1M+(Br)1]1-` + `sodium chloride`,
        `[1M-(H)1]1- + (C2H4O2)1` = `[1M-(H)1]1-` - acetic,
        `[1M+(F)1]1- + (C2H4O2)1` = `[1M+(F)1]1-` + acetic,
        `[1M+(Na)1-(H)2]1- + (C2H4O2)1` = `[1M+(Na)1-(H)2]1-` + acetic,
        `[1M+(Cl)1]1- + (C2H4O2)1` = `[1M+(Cl)1]1-` + acetic,
        `[1M+(K)1-(H)2]1- + (C2H4O2)1` = `[1M+(K)1-(H)2]1-` + acetic,
        `[1M+(Br)1]1- + (C2H4O2)1` = `[1M+(Br)1]1-` + acetic,
        `[1M-(H)1]1- + (H2PO4)1` = `[1M-(H)1]1-` - phosphoric,
        `[1M+(F)1]1- + (H2PO4)1` = `[1M+(F)1]1-` + phosphoric,
        `[1M+(Na)1-(H)2]1- + (H2PO4)1` = `[1M+(Na)1-(H)2]1-` + phosphoric,
        `[1M+(Cl)1]1- + (H2PO4)1` = `[1M+(Cl)1]1-` + phosphoric,
        `[1M+(K)1-(H)2]1- + (H2PO4)1` = `[1M+(K)1-(H)2]1-` + phosphoric,
        `[1M+(Br)1]1- + (H2PO4)1` = `[1M+(Br)1]1-` + phosphoric,
        `[1M-(H)1]1- + (C2HF3O2)1` = `[1M-(H)1]1-` - tfa,
        `[1M+(F)1]1- + (C2HF3O2)1` = `[1M+(F)1]1-` + tfa,
        `[1M+(Na)1-(H)2]1- + (C2HF3O2)1` = `[1M+(Na)1-(H)2]1-` + tfa,
        `[1M+(Cl)1]1- + (C2HF3O2)1` = `[1M+(Cl)1]1-` + tfa,
        `[1M+(K)1-(H)2]1- + (C2HF3O2)1` = `[1M+(K)1-(H)2]1-` + tfa,
        `[1M+(Br)1]1- + (C2HF3O2)1` = `[1M+(Br)1]1-` + tfa
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
