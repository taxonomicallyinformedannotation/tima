#' @title Complement metadata of structures
#'
#' @description This function complements structural metadata by joining
#'     stereochemistry, metadata, names, and chemical taxonomy information
#'     from reference libraries. Enriches annotation results with comprehensive
#'     structure information.
#'
#' @include clean_collapse.R
#'
#' @param df Data frame with structural metadata to be complemented
#' @param str_stereo Character string path to file with structure stereochemistry
#' @param str_met Character string path to file with structure metadata
#' @param str_nam Character string path to file with structure names
#' @param str_tax_cla Character string path to file with ClassyFire taxonomy
#' @param str_tax_npc Character string path to file with NPClassifier taxonomy
#'
#' @return Data frame with enriched structural metadata
#'
#' @examples NULL
complement_metadata_structures <- function(
  df,
  str_stereo = get("str_stereo", envir = parent.frame()),
  str_met = get("str_met", envir = parent.frame()),
  str_nam = get("str_nam", envir = parent.frame()),
  str_tax_cla = get("str_tax_cla", envir = parent.frame()),
  str_tax_npc = get("str_tax_npc", envir = parent.frame())
) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (nrow(df) == 0L) {
    logger::log_warn("Empty data frame provided")
    return(df)
  }

  # Validate file paths
  str_files <- list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )

  for (param_name in names(str_files)) {
    param_value <- str_files[[param_name]]
    if (!is.character(param_value) || length(param_value) != 1L) {
      stop(param_name, " must be a single character string")
    }
    if (!file.exists(param_value)) {
      stop(param_name, " file not found: ", param_value)
    }
  }

  # logger::log_trace(
  #  "Complementing structural metadata from reference libraries"
  #)
  logger::log_debug("Input: ", nrow(df), " rows")

  # Load stereochemistry data
  stereo <- tidytable::fread(
    str_stereo,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  stereo_s <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer_s = structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)

  stereo_i <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo_i = structure_smiles_no_stereo
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )
  # logger::log_trace("Stereo loaded")

  met_2d <- tidytable::fread(
    str_met,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(stereo) |>
    tidytable::distinct(
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo,
      structure_exact_mass,
      structure_xlogp,
      structure_molecular_formula
    ) |>
    tidytable::filter(
      !is.na(structure_inchikey_connectivity_layer) |
        !is.na(structure_smiles_no_stereo)
    ) |>
    ## Avoid small discrepancies
    tidytable::distinct(
      structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)
  # logger::log_trace("Metadata loaded")

  nam_2d <- tidytable::fread(
    str_nam,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::left_join(stereo) |>
    tidytable::filter(
      !is.na(structure_inchikey_connectivity_layer) |
        !is.na(structure_smiles_no_stereo)
    ) |>
    tidytable::distinct(
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo,
      structure_name
    ) |>
    tidytable::group_by(
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo
    ) |>
    clean_collapse(cols = c("structure_name")) |>
    ## Avoid small discrepancies
    tidytable::distinct(
      structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    ) |>
    tidytable::distinct(structure_smiles_no_stereo, .keep_all = TRUE)
  # logger::log_trace("Names loaded")

  tax_cla <- tidytable::fread(
    str_tax_cla,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_tax_cla_chemontid_i = structure_tax_cla_chemontid,
      candidate_structure_tax_cla_01kin_i = structure_tax_cla_01kin,
      candidate_structure_tax_cla_02sup_i = structure_tax_cla_02sup,
      candidate_structure_tax_cla_03cla_i = structure_tax_cla_03cla,
      candidate_structure_tax_cla_04dirpar_i = structure_tax_cla_04dirpar
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )
  # logger::log_trace("Classyfire done")

  tax_npc <- tidytable::fread(
    str_tax_npc,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_tax_npc_01pat_s = structure_tax_npc_01pat,
      candidate_structure_tax_npc_02sup_s = structure_tax_npc_02sup,
      candidate_structure_tax_npc_03cla_s = structure_tax_npc_03cla,
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)
  # logger::log_trace("NPClassifier done")

  met_i <- met_2d |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_molecular_formula_i = structure_molecular_formula,
      candidate_structure_exact_mass_i = structure_exact_mass,
      candidate_structure_xlogp_i = structure_xlogp
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )

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
  # logger::log_trace("Metadata done")

  nam_i <- nam_2d |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_name_i = structure_name
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )

  nam_s <- nam_2d |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_name_s = structure_name
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(candidate_structure_smiles_no_stereo, .keep_all = TRUE)
  rm(nam_2d)
  # logger::log_trace("Names done")

  # Inject placeholder columns early if missing in input df to satisfy downstream selects
  placeholder_candidate_cols <- c(
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp",
    "candidate_structure_name",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar"
  )
  for (col in placeholder_candidate_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  placeholder_structure_cols <- c(
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_name",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo"
  )
  for (col in placeholder_structure_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

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
      candidate_structure_inchikey_connectivity_layer = tidytable::coalesce(
        candidate_structure_inchikey_connectivity_layer_s,
        candidate_structure_inchikey_connectivity_layer
      )
    ) |>
    tidytable::select(
      -candidate_structure_smiles_no_stereo_i,
      -candidate_structure_inchikey_connectivity_layer_s
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
      -candidate_structure_molecular_formula_s,
      -candidate_structure_molecular_formula_i,
      -candidate_structure_exact_mass_s,
      -candidate_structure_exact_mass_i,
      -candidate_structure_xlogp_s,
      -candidate_structure_xlogp_i
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
    tidytable::select(
      -candidate_structure_name_s,
      -candidate_structure_name_i
    ) |>
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
      -candidate_structure_tax_npc_01pat_s,
      -candidate_structure_tax_npc_02sup_s,
      -candidate_structure_tax_npc_03cla_s
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
      -candidate_structure_tax_cla_chemontid_i,
      -candidate_structure_tax_cla_01kin_i,
      -candidate_structure_tax_cla_02sup_i,
      -candidate_structure_tax_cla_03cla_i,
      -candidate_structure_tax_cla_04dirpar_i
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

  return(table_final)
}
