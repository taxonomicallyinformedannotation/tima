#' @title Form adducts negative
#'
#' @param massesTable TODO
#' @param adductsTable TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr all_of everything mutate select
#' @importFrom tidyr pivot_longer
#'
#' @examples TODO
form_adducts_neg <- function(massesTable, adductsTable) {
  ## Calculate the masses for various negative adducts
  adducts_neg <- massesTable |>
    dplyr::mutate(
      neg_3_3proton = (exact_mass - 3 * proton) / 3,
      neg_2_2proton = ((exact_mass - 2 * proton) / 2),
      # neg_1_minus2waterminus1proton = exact_mass - 2 * water - proton,
      # neg_1_minus1waterminus1proton = exact_mass - water - proton,
      neg_1_minus1proton = exact_mass - proton,
      neg_1_minus2proton1sodium = exact_mass + sodium - 2 * proton,
      neg_1_1chlorine = exact_mass + chlorine,
      neg_1_minus2proton1potassium = exact_mass + potassium - 2 * proton,
      neg_1_minus1proton1formic = exact_mass + formic - proton,
      neg_1_minus1proton1acetic = exact_mass + acetic - proton,
      neg_1_minus2proton1sodium1formic = exact_mass + formic + sodium - 2 * proton,
      neg_1_1bromine = exact_mass + bromine,
      neg_1_minus1proton1tfa = exact_mass + tfa - proton,
      neg_2MH = 2 * exact_mass - proton,
      neg_2MFAH = 2 * exact_mass + formic - proton,
      neg_2MACH = 2 * exact_mass + acetic - proton,
      neg_3MH = 3 * exact_mass - proton
    ) |>
    dplyr::select(-colnames(adductsTable))

  ## Pivot the adducts_neg table to get a long format with adduct and adduct mass as columns
  n <- ncol(adducts_neg)
  adducts_neg <- adducts_neg |>
    tidyr::pivot_longer(2:dplyr::all_of(n)) |>
    dplyr::select(dplyr::everything(),
      adduct = name,
      adduct_mass = value
    )

  return(adducts_neg)
}
