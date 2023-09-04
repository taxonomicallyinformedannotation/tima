#' @title Complement metadata of structures
#'
#' @description This function complement structural metadata
#'
#' @include clean_collapse.R
#'
#' @param df Data frame with structural metadata to be complemented
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#'
#' @return Data frame with complemented structural metadata
#'
#' @export
#'
#' @examples NULL
complement_metadata_structures <- function(
    df,
    str_stereo = params$files$libraries$merged$structures$stereo,
    str_met = params$files$libraries$merged$structures$metadata,
    str_nam = params$files$libraries$merged$structures$names,
    str_tax_cla =
      params$files$libraries$merged$structures$taxonomies$cla,
    str_tax_npc = params$files$libraries$merged$structures$taxonomies$npc) {
  log_debug("Trying to look for already computed metadata")
  stereo <- tidytable::fread(str_stereo,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  stereo_s <- stereo |>
    tidytable::select(
      structure_inchikey_no_stereo_s = structure_inchikey_no_stereo,
      structure_smiles_no_stereo
    ) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  stereo_i <- stereo |>
    tidytable::select(structure_inchikey_no_stereo,
      structure_smiles_no_stereo_i = structure_smiles_no_stereo
    ) |>
    tidytable::distinct(structure_inchikey_no_stereo, .keep_all = TRUE)

  met_2d <- tidytable::fread(str_met,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(stereo) |>
    tidytable::distinct(
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo,
      structure_exact_mass,
      structure_xlogp,
      structure_molecular_formula
    ) |>
    ## Avoid small discrepancies
    tidytable::distinct(structure_inchikey_no_stereo,
      .keep_all = TRUE
    ) |>
    tidytable::distinct(structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  nam_2d <- tidytable::fread(str_nam,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(stereo) |>
    tidytable::distinct(
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo,
      structure_name
    ) |>
    tidytable::group_by(
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo
    ) |>
    clean_collapse(cols = c("structure_name")) |>
    ## Avoid small discrepancies
    tidytable::distinct(structure_inchikey_no_stereo,
      .keep_all = TRUE
    ) |>
    tidytable::distinct(structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  tax_cla <- tidytable::fread(str_tax_cla,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      structure_inchikey_no_stereo,
      structure_tax_cla_chemontid_i =
        structure_tax_cla_chemontid,
      structure_tax_cla_01kin_i =
        structure_tax_cla_01kin,
      structure_tax_cla_02sup_i =
        structure_tax_cla_02sup,
      structure_tax_cla_03cla_i =
        structure_tax_cla_03cla,
      structure_tax_cla_04dirpar_i =
        structure_tax_cla_04dirpar
    ) |>
    tidytable::distinct(structure_inchikey_no_stereo, .keep_all = TRUE)

  tax_npc <- tidytable::fread(str_tax_npc,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      structure_smiles_no_stereo,
      structure_tax_npc_01pat_s =
        structure_tax_npc_01pat,
      structure_tax_npc_02sup_s =
        structure_tax_npc_02sup,
      structure_tax_npc_03cla_s =
        structure_tax_npc_03cla,
    ) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  met_i <- met_2d |>
    tidytable::select(
      structure_inchikey_no_stereo,
      structure_molecular_formula_i = structure_molecular_formula,
      structure_exact_mass_i = structure_exact_mass,
      structure_xlogp_i = structure_xlogp
    ) |>
    tidytable::distinct(structure_inchikey_no_stereo, .keep_all = TRUE)

  met_s <- met_2d |>
    tidytable::select(
      structure_smiles_no_stereo,
      structure_molecular_formula_s = structure_molecular_formula,
      structure_exact_mass_s = structure_exact_mass,
      structure_xlogp_s = structure_xlogp
    ) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  nam_i <- nam_2d |>
    tidytable::select(structure_inchikey_no_stereo,
      structure_name_i = structure_name
    ) |>
    tidytable::distinct(structure_inchikey_no_stereo, .keep_all = TRUE)

  nam_s <- nam_2d |>
    tidytable::select(structure_smiles_no_stereo,
      structure_name_s = structure_name
    ) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  ## Always returning preferentially internal values
  ## (smiles > inchikey > external)
  table_final <- df |>
    tidytable::left_join(stereo_i) |>
    tidytable::left_join(stereo_s) |>
    tidytable::mutate(
      structure_smiles_no_stereo = tidytable::coalesce(
        structure_smiles_no_stereo_i,
        structure_smiles_no_stereo
      ),
      structure_inchikey_no_stereo = tidytable::coalesce(
        structure_inchikey_no_stereo_s,
        structure_inchikey_no_stereo
      )
    ) |>
    tidytable::select(-structure_smiles_no_stereo_i, -structure_inchikey_no_stereo_s) |>
    tidytable::left_join(met_i) |>
    tidytable::left_join(met_s) |>
    tidytable::mutate(
      structure_molecular_formula = tidytable::coalesce(
        structure_molecular_formula_s,
        structure_molecular_formula_i,
        structure_molecular_formula
      ),
      structure_exact_mass = tidytable::coalesce(
        structure_exact_mass_s,
        structure_exact_mass_i,
        structure_exact_mass
      ),
      structure_xlogp = tidytable::coalesce(
        structure_xlogp_s,
        structure_xlogp_i,
        structure_xlogp
      )
    ) |>
    tidytable::select(
      -structure_molecular_formula_s,
      -structure_molecular_formula_i,
      -structure_exact_mass_s,
      -structure_exact_mass_i,
      -structure_xlogp_s,
      -structure_xlogp_i
    ) |>
    tidytable::left_join(nam_i) |>
    tidytable::left_join(nam_s) |>
    tidytable::mutate(structure_name = tidytable::coalesce(
      structure_name_s,
      structure_name_i,
      structure_name
    )) |>
    tidytable::select(-structure_name_s, -structure_name_i) |>
    tidytable::left_join(tax_npc) |>
    tidytable::mutate(
      structure_tax_npc_01pat = tidytable::coalesce(
        structure_tax_npc_01pat_s,
        structure_tax_npc_01pat
      ),
      structure_tax_npc_02sup = tidytable::coalesce(
        structure_tax_npc_02sup_s,
        structure_tax_npc_02sup
      ),
      structure_tax_npc_03cla = tidytable::coalesce(
        structure_tax_npc_03cla_s,
        structure_tax_npc_03cla
      ),
    ) |>
    tidytable::select(
      -structure_tax_npc_01pat_s,
      -structure_tax_npc_02sup_s,
      -structure_tax_npc_03cla_s
    ) |>
    tidytable::left_join(tax_cla) |>
    tidytable::mutate(
      structure_tax_cla_chemontid = tidytable::coalesce(
        structure_tax_cla_chemontid_i,
        structure_tax_cla_chemontid
      ),
      structure_tax_cla_01kin = tidytable::coalesce(
        structure_tax_cla_01kin_i,
        structure_tax_cla_01kin
      ),
      structure_tax_cla_02sup = tidytable::coalesce(
        structure_tax_cla_02sup_i,
        structure_tax_cla_02sup
      ),
      structure_tax_cla_03cla = tidytable::coalesce(
        structure_tax_cla_03cla_i,
        structure_tax_cla_03cla
      ),
      structure_tax_cla_04dirpar = tidytable::coalesce(
        structure_tax_cla_04dirpar_i,
        structure_tax_cla_04dirpar
      )
    ) |>
    tidytable::select(
      -structure_tax_cla_chemontid_i,
      -structure_tax_cla_01kin_i,
      -structure_tax_cla_02sup_i,
      -structure_tax_cla_03cla_i,
      -structure_tax_cla_04dirpar_i
    ) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      )
    )

  ## TODO if (quickmode == FALSE){...}

  return(table_final)
}
