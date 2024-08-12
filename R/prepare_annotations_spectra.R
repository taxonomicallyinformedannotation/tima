import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, select, .into = environment())

#' @title Prepare annotations MS2
#'
#' @description This function prepares the spectral matches
#'    obtained previously to make them compatible
#'
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#' @importFrom tidytable fread
#' @importFrom tidytable mutate
#' @importFrom tidytable select
#'
#' @include get_params.R
#' @include select_annotations_columns.R
#'
#' @param input Input file
#' @param output Output file
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#'
#' @return The path to the prepared spectral annotations
#'
#' @export
#'
#' @examples NULL
prepare_annotations_spectra <-
  function(input = get_params(step = "prepare_annotations_spectra")$files$annotations$raw$spectral$spectral,
           output = get_params(step = "prepare_annotations_spectra")$files$annotations$prepared$structural$spectral,
           str_stereo = get_params(step = "prepare_annotations_spectra")$files$libraries$sop$merged$structures$stereo,
           str_met = get_params(step = "prepare_annotations_spectra")$files$libraries$sop$merged$structures$metadata,
           str_nam = get_params(step = "prepare_annotations_spectra")$files$libraries$sop$merged$structures$names,
           str_tax_cla = get_params(step = "prepare_annotations_spectra")$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = get_params(step = "prepare_annotations_spectra")$files$libraries$sop$merged$structures$taxonomies$npc) {
    stopifnot("Input file(s) do(es) not exist" = all(lapply(X = input, FUN = file.exists) |> unlist()))
    log_debug(x = "Loading and formatting spectral matches")
    table <-
      lapply(
        X = input,
        FUN = fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      bind_rows() |>
      filter(!is.na(feature_id)) |>
      distinct(
        feature_id,
        candidate_adduct,
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
      mutate(
        candidate_structure_exact_mass = as.numeric(candidate_structure_exact_mass),
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

    try(expr = {
      export_params(
        parameters = get_params(step = "prepare_annotations_spectra"),
        step = "prepare_annotations_spectra"
      )
    })
    export_output(x = table, file = output[[1]])
    rm(table)

    return(output[[1]])
  }
