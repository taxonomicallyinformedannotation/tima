import::from(tidytable, across, .into = environment())
import::from(tidytable, coalesce, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, group_by, .into = environment())
import::from(tidytable, left_join, .into = environment())
import::from(tidytable, na_if, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Complement metadata of structures
#'
#' @description This function complement structural metadata
#'
#' @importFrom tidytable across
#' @importFrom tidytable coalesce
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#' @importFrom tidytable fread
#' @importFrom tidytable group_by
#' @importFrom tidytable left_join
#' @importFrom tidytable na_if
#' @importFrom tidytable select
#' @importFrom tidytable where
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
complement_metadata_structures <- function(df,
                                           str_stereo = get("str_stereo", envir = parent.frame()),
                                           str_met = get("str_met", envir = parent.frame()),
                                           str_nam = get("str_nam", envir = parent.frame()),
                                           str_tax_cla = get("str_tax_cla", envir = parent.frame()),
                                           str_tax_npc = get("str_tax_npc", envir = parent.frame())) {
  log_debug("Trying to look for already computed metadata")
  stereo <- fread(str_stereo,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  stereo_s <- stereo |>
    select(
      candidate_structure_inchikey_no_stereo_s = structure_inchikey_no_stereo,
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo
    ) |>
    filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)

  stereo_i <- stereo |>
    select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_smiles_no_stereo_i = structure_smiles_no_stereo
    ) |>
    filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  met_2d <- fread(str_met,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    left_join(stereo) |>
    distinct(
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo,
      structure_exact_mass,
      structure_xlogp,
      structure_molecular_formula
    ) |>
    filter(!is.na(structure_inchikey_no_stereo) |
      !is.na(structure_smiles_no_stereo)) |>
    ## Avoid small discrepancies
    distinct(structure_inchikey_no_stereo, .keep_all = TRUE) |>
    distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  nam_2d <- fread(str_nam,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    left_join(stereo) |>
    filter(!is.na(structure_inchikey_no_stereo) |
      !is.na(structure_smiles_no_stereo)) |>
    distinct(
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo,
      structure_name
    ) |>
    group_by(structure_inchikey_no_stereo, structure_smiles_no_stereo) |>
    clean_collapse(cols = c("structure_name")) |>
    ## Avoid small discrepancies
    distinct(structure_inchikey_no_stereo, .keep_all = TRUE) |>
    distinct(structure_smiles_no_stereo, .keep_all = TRUE)

  tax_cla <- fread(str_tax_cla,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    select(
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
    filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  tax_npc <- fread(str_tax_npc,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_tax_npc_01pat_s =
        structure_tax_npc_01pat,
      candidate_structure_tax_npc_02sup_s =
        structure_tax_npc_02sup,
      candidate_structure_tax_npc_03cla_s =
        structure_tax_npc_03cla,
    ) |>
    filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)

  met_i <- met_2d |>
    select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_molecular_formula_i = structure_molecular_formula,
      candidate_structure_exact_mass_i = structure_exact_mass,
      candidate_structure_xlogp_i = structure_xlogp
    ) |>
    filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  met_s <- met_2d |>
    select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_molecular_formula_s = structure_molecular_formula,
      candidate_structure_exact_mass_s = structure_exact_mass,
      candidate_structure_xlogp_s = structure_xlogp
    ) |>
    filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)
  rm(met_2d)

  nam_i <- nam_2d |>
    select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_name_i = structure_name
    ) |>
    filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    distinct(candidate_structure_inchikey_no_stereo, .keep_all = TRUE)

  nam_s <- nam_2d |>
    select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_name_s = structure_name
    ) |>
    filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)
  rm(nam_2d)

  ## Always returning preferentially internal values
  ## (smiles > inchikey > external)
  table_final <- df |>
    left_join(stereo_i) |>
    left_join(stereo_s) |>
    mutate(
      candidate_structure_smiles_no_stereo = coalesce(
        candidate_structure_smiles_no_stereo_i,
        candidate_structure_smiles_no_stereo
      ),
      candidate_structure_inchikey_no_stereo = coalesce(
        candidate_structure_inchikey_no_stereo_s,
        candidate_structure_inchikey_no_stereo
      )
    ) |>
    select(
      -candidate_structure_smiles_no_stereo_i, -candidate_structure_inchikey_no_stereo_s
    ) |>
    left_join(met_i) |>
    left_join(met_s) |>
    mutate(
      candidate_structure_molecular_formula = coalesce(
        candidate_structure_molecular_formula_s,
        candidate_structure_molecular_formula_i,
        candidate_structure_molecular_formula
      ),
      candidate_structure_exact_mass = coalesce(
        candidate_structure_exact_mass_s,
        candidate_structure_exact_mass_i,
        candidate_structure_exact_mass
      ),
      candidate_structure_xlogp = coalesce(
        candidate_structure_xlogp_s,
        candidate_structure_xlogp_i,
        candidate_structure_xlogp
      )
    ) |>
    select(
      -candidate_structure_molecular_formula_s, -candidate_structure_molecular_formula_i, -candidate_structure_exact_mass_s, -candidate_structure_exact_mass_i, -candidate_structure_xlogp_s, -candidate_structure_xlogp_i
    ) |>
    left_join(nam_i) |>
    left_join(nam_s) |>
    mutate(
      candidate_structure_name = coalesce(
        candidate_structure_name_s,
        candidate_structure_name_i,
        candidate_structure_name
      )
    ) |>
    select(-candidate_structure_name_s, -candidate_structure_name_i) |>
    left_join(tax_npc) |>
    mutate(
      candidate_structure_tax_npc_01pat = coalesce(
        candidate_structure_tax_npc_01pat_s,
        candidate_structure_tax_npc_01pat
      ),
      candidate_structure_tax_npc_02sup = coalesce(
        candidate_structure_tax_npc_02sup_s,
        candidate_structure_tax_npc_02sup
      ),
      candidate_structure_tax_npc_03cla = coalesce(
        candidate_structure_tax_npc_03cla_s,
        candidate_structure_tax_npc_03cla
      ),
    ) |>
    select(
      -candidate_structure_tax_npc_01pat_s, -candidate_structure_tax_npc_02sup_s, -candidate_structure_tax_npc_03cla_s
    ) |>
    left_join(tax_cla) |>
    mutate(
      candidate_structure_tax_cla_chemontid = coalesce(
        candidate_structure_tax_cla_chemontid_i,
        candidate_structure_tax_cla_chemontid
      ),
      candidate_structure_tax_cla_01kin = coalesce(
        candidate_structure_tax_cla_01kin_i,
        candidate_structure_tax_cla_01kin
      ),
      candidate_structure_tax_cla_02sup = coalesce(
        candidate_structure_tax_cla_02sup_i,
        candidate_structure_tax_cla_02sup
      ),
      candidate_structure_tax_cla_03cla = coalesce(
        candidate_structure_tax_cla_03cla_i,
        candidate_structure_tax_cla_03cla
      ),
      candidate_structure_tax_cla_04dirpar = coalesce(
        candidate_structure_tax_cla_04dirpar_i,
        candidate_structure_tax_cla_04dirpar
      )
    ) |>
    select(
      -candidate_structure_tax_cla_chemontid_i, -candidate_structure_tax_cla_01kin_i, -candidate_structure_tax_cla_02sup_i, -candidate_structure_tax_cla_03cla_i, -candidate_structure_tax_cla_04dirpar_i
    ) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        na_if(x, "")
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
