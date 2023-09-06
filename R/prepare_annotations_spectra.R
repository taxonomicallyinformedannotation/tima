#' @title Prepare annotations MS2
#'
#' @description This function prepares the spectral matches
#'    obtained previously to make them compatible
#'
#' @include select_annotations_columns.R
#'
#' @param input Input file
#' @param output Output file
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_annotations_spectra <-
  function(input = params$files$annotations$raw$spectral,
           output = params$files$annotations$prepared$structural,
           str_stereo = params$files$libraries$sop$merged$structures$stereo,
           str_met = params$files$libraries$sop$merged$structures$metadata,
           str_nam = params$files$libraries$sop$merged$structures$names,
           str_tax_cla =
             params$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc =
             params$files$libraries$sop$merged$structures$taxonomies$npc,
           parameters = params) {
    stopifnot(
      "Input file(s) do(es) not exist" =
        rep(TRUE, length(input)) ==
          lapply(X = input, FUN = file.exists)
    )
    log_debug(x = "Loading and formatting spectral matches")
    table <-
      lapply(
        X = input,
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::bind_rows() |>
      tidytable::filter(!is.na(feature_id)) |>
      tidytable::distinct(
        feature_id,
        candidate_library,
        candidate_spectrum_entropy,
        candidate_structure_error_mz,
        candidate_structure_name,
        candidate_structure_inchikey_no_stereo,
        candidate_structure_smiles_no_stereo,
        candidate_structure_molecular_formula,
        candidate_structure_exact_mass,
        candidate_structure_xlogp,
        candidate_score_similarity,
        candidate_count_similarity_peaks_matched
      ) |>
      ## Add new columns
      tidytable::mutate(
        candidate_structure_exact_mass =
          as.numeric(candidate_structure_exact_mass),
        candidate_structure_tax_npc_01pat = NA_character_,
        candidate_structure_tax_npc_02sup = NA_character_,
        candidate_structure_tax_npc_03cla = NA_character_,
        candidate_structure_tax_cla_chemontid = NA_character_,
        candidate_structure_tax_cla_01kin = NA_character_,
        candidate_structure_tax_cla_02sup = NA_character_,
        candidate_structure_tax_cla_03cla = NA_character_,
        candidate_structure_tax_cla_04dirpar = NA_character_,
      ) |>
      select_annotations_columns()

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_spectra")
    export_output(x = table, file = output[[1]])

    return(output[[1]])
  }
