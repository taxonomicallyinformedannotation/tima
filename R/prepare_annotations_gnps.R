#' @title Prepare annotations GNPS
#'
#' @description This function prepares GNPS obtained annotations
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
#' @param parameters Parameters
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_annotations_gnps <-
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
    if (length(input) == 0) {
      input <- "w1llN3v3r3v3r3x1st"
    }
    if (rep(TRUE, length(input)) == lapply(X = input, FUN = file.exists)) {
      params <<- parameters
      log_debug("Loading and formatting GNPS results")
      ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
      table <- lapply(
        X = input,
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
        tidytable::bind_rows() |>
        tidytable::mutate(
          candidate_structure_error_mz = as.numeric(MZErrorPPM) *
            1E-6 *
            as.numeric(Precursor_MZ)
        ) |>
        tidytable::select(tidytable::any_of(c(
          "feature_id" = "#Scan#",
          ## TODO check which one is correct
          # "candidate_structure_error_mz" = "MassDiff",
          "candidate_library" = "LibraryName",
          ## TODO "candidate_library_type" = "TODO",
          "candidate_structure_name" = "Compound_Name",
          "candidate_score_similarity" = "MQScore",
          "candidate_count_similarity_peaks_matched" = "SharedPeaks",
          "candidate_structure_inchi" = "INCHI",
          "candidate_structure_inchikey" = "InChIKey",
          "candidate_structure_inchikey_no_stereo" = "InChIKey-Planar",
          "candidate_structure_tax_npc_01pat" = "npclassifier_pathway",
          "candidate_structure_tax_npc_02sup" = "npclassifier_superclass",
          "candidate_structure_tax_npc_03cla" = "npclassifier_class",
          "candidate_structure_exact_mass" = "ExactMass",
          ## Only partially present
          "candidate_structure_tax_cla_02sup" = "superclass",
          "candidate_structure_tax_cla_03cla" = "class",
          "candidate_structure_tax_cla_04dirpar" = "subclass"
        ))) |>
        tidytable::mutate(
          candidate_structure_smiles_no_stereo = NA,
          candidate_structure_molecular_formula = candidate_structure_inchi |>
            ## really dirty
            gsub(pattern = ".*\\/C", replacement = "C") |>
            gsub(pattern = "\\/.*", replacement = ""),
          candidate_structure_xlogp = NA,
          ## Only partially present
          candidate_structure_tax_cla_chemontid = NA,
          candidate_structure_tax_cla_01kin = NA
        ) |>
        select_annotations_columns()
    } else {
      log_debug("No GNPS annotations found, returning an empty file instead")
      table <- fake_annotations_columns()
    }
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_gnps")
    export_output(x = table, file = output[[1]])
    return(output[[1]])
  }
