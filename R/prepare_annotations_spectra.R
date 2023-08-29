utils::globalVariables(
  c(
    "count_peaks_explained",
    "count_peaks_matched",
    "error_mz",
    "feature_id",
    "params",
    "score",
    "score_input",
    # "score_input_normalized",
    "structure_exact_mass",
    "structure_inchikey_2D",
    "structure_molecular_formula",
    "structure_name",
    "structure_smiles_2D",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_xlogp"
  )
)

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
#' @param str_2d_3d File containing 2D and 3D structures
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
           str_2d_3d = params$files$libraries$sop$merged$structures$dd_ddd,
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
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        score_input = score,
        count_peaks_matched
      ) |>
      ## Add new columns
      tidytable::mutate(
        library = "ISDB",
        structure_exact_mass = as.numeric(structure_exact_mass),
        structure_taxonomy_npclassifier_01pathway = NA_character_,
        structure_taxonomy_npclassifier_02superclass = NA_character_,
        structure_taxonomy_npclassifier_03class = NA_character_,
        structure_taxonomy_classyfire_chemontid = NA_character_,
        structure_taxonomy_classyfire_01kingdom = NA_character_,
        structure_taxonomy_classyfire_02superclass = NA_character_,
        structure_taxonomy_classyfire_03class = NA_character_,
        structure_taxonomy_classyfire_04directparent = NA_character_,
        ## mirror sirius
        count_peaks_explained = NA
      ) |>
      select_annotations_columns(
        str_2d_3d = str_2d_3d,
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
