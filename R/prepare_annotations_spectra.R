utils::globalVariables(
  c(
    "count_peaks_explained",
    "count_peaks_matched",
    "error_mz",
    "error_rt",
    "feature_id",
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
#' @description This function prepares the spectral matches obtained previously to make them compatible
#'
#' @include complement_metadata_structures.R
#' @include export_output.R
#' @include export_params.R
#' @include round_reals.R
#'
#' @param input Input file
#' @param output Output file
#' @param str_2D_3D File containing 2D and 3D structures
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
           str_2D_3D = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             dd_ddd,
           str_met = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             metadata,
           str_nam = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             names,
           str_tax_cla = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             taxonomies$
             cla,
           str_tax_npc = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             taxonomies$
             npc,
           parameters = params) {
    # Check if input file(s) exists
    stopifnot(
      "Input file(s) do(es) not exist" =
        rep(TRUE, length(input)) ==
          lapply(X = input, file.exists)
    )
    params <<- parameters
    log_debug(x = "Loading and formatting spectral matches")
    # Read input file and select specific columns
    table <-
      lapply(
        X = input,
        FUN = tidytable::fread,
        na.strings = c("", "NA")
      ) |>
      tidytable::bind_rows() |>
      tidytable::filter(!is.na(feature_id)) |>
      tidytable::distinct(
        feature_id,
        error_mz,
        error_rt,
        structure_name,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        score_input = score,
        count_peaks_matched
      ) |>
      # Add new columns
      tidyft::mutate(
        library = "ISDB",
        # score_input_normalized = bestNormalize::bestNormalize(
        #   x = score_input,
        #   standardize = FALSE,
        #   allow_orderNorm = FALSE,
        #   allow_lambert_s = TRUE,
        #   allow_lambert_h = TRUE
        # )$x.t,
        structure_exact_mass = as.numeric(structure_exact_mass),
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
        structure_taxonomy_classyfire_chemontid = NA,
        structure_taxonomy_classyfire_01kingdom = NA,
        structure_taxonomy_classyfire_02superclass = NA,
        structure_taxonomy_classyfire_03class = NA,
        structure_taxonomy_classyfire_04directparent = NA,
        ## mirror sirius
        count_peaks_explained = NA
      ) |>
      # tidytable::rowwise() |>
      # tidyft::mutate(structure_inchikey = paste0(structure_inchikey_2D, "-UHFFFAOYSA-N")) |>
      # tidytable::ungroup() |>
      tidytable::select(
        feature_id,
        error_mz,
        error_rt,
        structure_name,
        # structure_inchikey,
        structure_inchikey_2D,
        # structure_smiles,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        library,
        score_input,
        # score_input_normalized,
        count_peaks_matched,
        count_peaks_explained,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class,
        ## TODO until better
        structure_taxonomy_classyfire_chemontid,
        structure_taxonomy_classyfire_01kingdom,
        structure_taxonomy_classyfire_02superclass,
        structure_taxonomy_classyfire_03class,
        structure_taxonomy_classyfire_04directparent
      ) |>
      tidyft::mutate(tidytable::across(tidytable::everything(), as.character)) |>
      tidyft::mutate(tidytable::across(tidytable::everything(), .fns = function(x) {
        tidytable::na_if(x, "N/A")
      })) |>
      tidyft::mutate(tidytable::across(tidytable::everything(), .fns = function(x) {
        tidytable::na_if(x, "null")
      })) |>
      tidyft::mutate(tidytable::across(tidytable::everything(), .fns = function(x) {
        tidytable::na_if(x, "")
      })) |>
      round_reals() |>
      tidyft::mutate(tidytable::across(tidytable::where(is.numeric), as.character)) |>
      complement_metadata_structures(
        str_2D_3D = str_2D_3D,
        str_met = str_met,
        str_nam = str_nam,
        str_tax_cla = str_tax_cla,
        str_tax_npc = str_tax_npc
      )

    log_debug(x = "Exporting ...")
    # Call export_params and export_output functions
    export_params(step = "prepare_annotations_spectra")
    export_output(x = table, file = output[[1]])

    return(output[[1]])
  }
