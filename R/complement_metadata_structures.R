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
#' @examples NULL
complement_metadata_structures <- function(df,
                                           str_stereo = get("str_stereo", envir = parent.frame()),
                                           str_met = get("str_met", envir = parent.frame()),
                                           str_nam = get("str_nam", envir = parent.frame()),
                                           str_tax_cla = get("str_tax_cla", envir = parent.frame()),
                                           str_tax_npc = get("str_tax_npc", envir = parent.frame())) {
  log_debug("Trying to look for already computed metadata")
  stereo <- tidytable::fread(str_stereo,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  stereo_s <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo_s = structure_inchikey_no_stereo,
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)

  stereo_i <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_smiles_no_stereo_i = structure_smiles_no_stereo
    ) |>
    tidytable::filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    tidytable::distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

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
    tidytable::filter(!is.na(structure_inchikey_no_stereo) |
      !is.na(structure_smiles_no_stereo)) |>
    ## Avoid small discrepancies
    tidytable::distinct(structure_inchikey_no_stereo, .keep_all = TRUE) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  nam_2d <- tidytable::fread(str_nam,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(stereo) |>
    tidytable::filter(!is.na(structure_inchikey_no_stereo) |
      !is.na(structure_smiles_no_stereo)) |>
    tidytable::distinct(
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo,
      structure_name
    ) |>
    tidytable::group_by(structure_inchikey_no_stereo, structure_smiles_no_stereo) |>
    clean_collapse(cols = c("structure_name")) |>
    ## Avoid small discrepancies
    tidytable::distinct(structure_inchikey_no_stereo, .keep_all = TRUE) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  tax_cla <- tidytable::fread(str_tax_cla,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_tax_cla_chemontid_i =
        structure_tax_cla_chemontid,
      candidate_structure_tax_cla_01kin_i =
        structure_tax_cla_01kin,
      candidate_structure_tax_cla_02sup_i =
        structure_tax_cla_02sup,
      candidate_structure_tax_cla_03cla_i =
        structure_tax_cla_03cla,
      candidate_structure_tax_cla_04dirpar_i =
        structure_tax_cla_04dirpar
    ) |>
    tidytable::filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    tidytable::distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  tax_npc <- tidytable::fread(str_tax_npc,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_tax_npc_01pat_s =
        structure_tax_npc_01pat,
      candidate_structure_tax_npc_02sup_s =
        structure_tax_npc_02sup,
      candidate_structure_tax_npc_03cla_s =
        structure_tax_npc_03cla,
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)

  met_i <- met_2d |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_molecular_formula_i = structure_molecular_formula,
      candidate_structure_exact_mass_i = structure_exact_mass,
      candidate_structure_xlogp_i = structure_xlogp
    ) |>
    tidytable::filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    tidytable::distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  met_s <- met_2d |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_molecular_formula_s = structure_molecular_formula,
      candidate_structure_exact_mass_s = structure_exact_mass,
      candidate_structure_xlogp_s = structure_xlogp
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)
  rm(met_2d)

  nam_i <- nam_2d |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_name_i = structure_name
    ) |>
    tidytable::filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    tidytable::distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  nam_s <- nam_2d |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_name_s = structure_name
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)
  rm(nam_2d)

  ## Always returning preferentially internal values
  ## (smiles > inchikey > external)
  table_final <- df |>
    tidytable::left_join(stereo_i) |>
    tidytable::left_join(stereo_s) |>
    tidytable::mutate(
      candidate_structure_smiles_no_stereo = tidytable::coalesce(
        candidate_structure_smiles_no_stereo_i,
        candidate_structure_smiles_no_stereo
      ),
      candidate_structure_inchikey_no_stereo = tidytable::coalesce(
        candidate_structure_inchikey_no_stereo_s,
        candidate_structure_inchikey_no_stereo
      )
    ) |>
    tidytable::select(
      -candidate_structure_smiles_no_stereo_i, -candidate_structure_inchikey_no_stereo_s
    ) |>
    tidytable::left_join(met_i) |>
    tidytable::left_join(met_s) |>
    tidytable::mutate(
      candidate_structure_molecular_formula = tidytable::coalesce(
        candidate_structure_molecular_formula_s,
        candidate_structure_molecular_formula_i,
        candidate_structure_molecular_formula
      ),
      candidate_structure_exact_mass = tidytable::coalesce(
        candidate_structure_exact_mass_s,
        candidate_structure_exact_mass_i,
        candidate_structure_exact_mass
      ),
      candidate_structure_xlogp = tidytable::coalesce(
        candidate_structure_xlogp_s,
        candidate_structure_xlogp_i,
        candidate_structure_xlogp
      )
    ) |>
    tidytable::select(
      -candidate_structure_molecular_formula_s, -candidate_structure_molecular_formula_i, -candidate_structure_exact_mass_s, -candidate_structure_exact_mass_i, -candidate_structure_xlogp_s, -candidate_structure_xlogp_i
    ) |>
    tidytable::left_join(nam_i) |>
    tidytable::left_join(nam_s) |>
    tidytable::mutate(
      candidate_structure_name = tidytable::coalesce(
        candidate_structure_name_s,
        candidate_structure_name_i,
        candidate_structure_name
      )
    ) |>
    tidytable::select(-candidate_structure_name_s, -candidate_structure_name_i) |>
    tidytable::left_join(tax_npc) |>
    tidytable::mutate(
      candidate_structure_tax_npc_01pat = tidytable::coalesce(
        candidate_structure_tax_npc_01pat_s,
        candidate_structure_tax_npc_01pat
      ),
      candidate_structure_tax_npc_02sup = tidytable::coalesce(
        candidate_structure_tax_npc_02sup_s,
        candidate_structure_tax_npc_02sup
      ),
      candidate_structure_tax_npc_03cla = tidytable::coalesce(
        candidate_structure_tax_npc_03cla_s,
        candidate_structure_tax_npc_03cla
      ),
    ) |>
    tidytable::select(
      -candidate_structure_tax_npc_01pat_s, -candidate_structure_tax_npc_02sup_s, -candidate_structure_tax_npc_03cla_s
    ) |>
    tidytable::left_join(tax_cla) |>
    tidytable::mutate(
      candidate_structure_tax_cla_chemontid = tidytable::coalesce(
        candidate_structure_tax_cla_chemontid_i,
        candidate_structure_tax_cla_chemontid
      ),
      candidate_structure_tax_cla_01kin = tidytable::coalesce(
        candidate_structure_tax_cla_01kin_i,
        candidate_structure_tax_cla_01kin
      ),
      candidate_structure_tax_cla_02sup = tidytable::coalesce(
        candidate_structure_tax_cla_02sup_i,
        candidate_structure_tax_cla_02sup
      ),
      candidate_structure_tax_cla_03cla = tidytable::coalesce(
        candidate_structure_tax_cla_03cla_i,
        candidate_structure_tax_cla_03cla
      ),
      candidate_structure_tax_cla_04dirpar = tidytable::coalesce(
        candidate_structure_tax_cla_04dirpar_i,
        candidate_structure_tax_cla_04dirpar
      )
    ) |>
    tidytable::select(
      -candidate_structure_tax_cla_chemontid_i, -candidate_structure_tax_cla_01kin_i, -candidate_structure_tax_cla_02sup_i, -candidate_structure_tax_cla_03cla_i, -candidate_structure_tax_cla_04dirpar_i
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = function(x) {
        tidytable::na_if(x, "")
      }
    ))
  rm(
    met_i,
    met_s,
    nam_i,
    nam_s,
    stereo_i,
    stereo_s,
    tax_cla,
    tax_npc
  )

  ## ISSUE see #19
  # if (quickmode == FALSE){...}

  return(table_final)
}
