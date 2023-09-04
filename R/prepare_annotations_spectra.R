#' @title Prepare annotations MS2
#'
#' @description This function prepares the spectral matches
#'    obtained previously to make them compatible
#'
#' @include export_output.R
#' @include export_params.R
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
           output = params$files$annotations$prepared,
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
    params <<- parameters
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
        error_mz,
        structure_name,
        structure_inchikey_no_stereo,
        structure_smiles_no_stereo,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        score_input = score,
        count_peaks_matched
      ) |>
      ## Add new columns
      tidytable::mutate(
        library = "ISDB",
        ## TODO library_type = "TODO",
        structure_exact_mass = as.numeric(structure_exact_mass),
        structure_tax_npc_01pat = NA_character_,
        structure_tax_npc_02sup = NA_character_,
        structure_tax_npc_03cla = NA_character_,
        structure_tax_cla_chemontid = NA_character_,
        structure_tax_cla_01kin = NA_character_,
        structure_tax_cla_02sup = NA_character_,
        structure_tax_cla_03cla = NA_character_,
        structure_tax_cla_04dirpar = NA_character_,
        ## mirror sirius
        count_peaks_explained = NA
      ) |>
      select_annotations_columns(
        str_stereo = str_stereo,
        str_met = str_met,
        str_nam = str_nam,
        str_tax_cla = str_tax_cla,
        str_tax_npc = str_tax_npc
      )

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_spectra")
    export_output(x = table, file = output[[1]])

    return(output[[1]])
  }
