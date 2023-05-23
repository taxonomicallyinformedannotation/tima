utils::globalVariables(
  c(
    "#Scan#",
    "Compound_Name",
    "count_peaks_explained",
    "count_peaks_matched",
    "error_mz",
    "error_rt",
    "ExactMass",
    "feature_id",
    "InChIKey",
    "InChIKey-Planar",
    "MQScore",
    "MZErrorPPM",
    "npclassifier_class",
    "npclassifier_pathway",
    "npclassifier_superclass",
    "Precursor_MZ",
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

#' @title Prepare annotations GNPS
#'
#' @description This function prepares GNPS obtained annotations for further use
#'
#' @param input Input file
#' @param output Output file
#' @param str_2D_3D File containing 2D and 3D structures
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
           output = params$files$annotations$prepared,
           str_2D_3D = params$files$libraries$sop$merged$structures$dd_ddd,
           str_met = params$files$libraries$sop$merged$structures$metadata,
           str_nam = params$files$libraries$sop$merged$structures$names,
           str_tax_cla = params$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = params$files$libraries$sop$merged$structures$taxonomies$npc,
           parameters = params) {
    if (length(input) == 0) {
      input <- "w1llN3v3r3v3r3x1st"
    }
    if (rep(TRUE, length(input)) == lapply(X = input, file.exists)) {
      params <<- parameters
      log_debug("Loading and formatting GNPS results")
      ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
      table <- lapply(
        X = input,
        FUN = tidytable::fread
      ) |>
        tidytable::bind_rows() |>
        tidytable::mutate(
          error_mz = as.numeric(MZErrorPPM) *
            1E-6 *
            as.numeric(Precursor_MZ),
          error_rt = NA
        ) |>
        tidytable::select(
          feature_id = `#Scan#`,
          error_mz = MassDiff,
          error_rt,
          library = LibraryName,
          structure_name = Compound_Name,
          # structure_smiles = Smiles,
          score_input = MQScore,
          count_peaks_matched = SharedPeaks,
          # smiles_2D, ## Not available for now
          structure_inchi = INCHI,
          structure_inchikey = InChIKey,
          structure_inchikey_2D = `InChIKey-Planar`,
          structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
          structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
          structure_taxonomy_npclassifier_03class = npclassifier_class,
          structure_exact_mass = ExactMass,
          ## Only partially present
          structure_taxonomy_classyfire_02superclass = superclass,
          structure_taxonomy_classyfire_03class = class,
          structure_taxonomy_classyfire_04directparent = subclass
        ) |>
        tidytable::mutate(
          error_rt = NA,
          structure_smiles_2D = NA,
          structure_molecular_formula = structure_inchi |>
            ## really dirty
            gsub(pattern = ".*\\/C", replacement = "C") |>
            gsub(pattern = "\\/.*", replacement = ""),
          structure_xlogp = NA,
          # score_input_normalized = bestNormalize::bestNormalize(
          #   x = score_input,
          #   standardize = FALSE,
          #   allow_orderNorm = FALSE,
          #   allow_lambert_s = TRUE,
          #   allow_lambert_h = TRUE
          # )$x.t,
          ## Only partially present
          structure_taxonomy_classyfire_chemontid = NA,
          structure_taxonomy_classyfire_01kingdom = NA,
          ## mirror sirius
          count_peaks_explained = NA
        ) |>
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
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          structure_taxonomy_classyfire_chemontid,
          structure_taxonomy_classyfire_01kingdom,
          structure_taxonomy_classyfire_02superclass,
          structure_taxonomy_classyfire_03class,
          structure_taxonomy_classyfire_04directparent
        ) |>
        tidytable::mutate(tidytable::across(tidytable::everything(), as.character)) |>
        tidytable::mutate(tidytable::across(tidytable::everything(), tidytable::na_if, "N/A")) |>
        tidytable::mutate(tidytable::across(tidytable::everything(), tidytable::na_if, "null")) |>
        round_reals() |>
        tidytable::mutate(tidytable::across(tidytable::where(is.numeric), as.character)) |>
        complement_metadata_structures(
          str_2D_3D = str_2D_3D,
          str_met = str_met,
          str_nam = str_nam,
          str_tax_cla = str_tax_cla,
          str_tax_npc = str_tax_npc
        )
    } else {
      log_debug("No GNPS annotations found, returning an empty file instead")
      table <- data.frame(
        feature_id = NA,
        error_mz = NA,
        error_rt = NA,
        structure_name = NA,
        # structure_inchikey = NA,
        structure_inchikey_2D = NA,
        # structure_smiles = NA,
        structure_smiles_2D = NA,
        structure_molecular_formula = NA,
        structure_exact_mass = NA,
        structure_xlogp = NA,
        library = NA,
        score_input = NA,
        # score_input_normalized = NA,
        count_peaks_matched = NA,
        count_peaks_explained = NA,
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
        structure_taxonomy_classyfire_chemontid = NA,
        structure_taxonomy_classyfire_01kingdom = NA,
        structure_taxonomy_classyfire_02superclass = NA,
        structure_taxonomy_classyfire_03class = NA,
        structure_taxonomy_classyfire_04directparent = NA
      )
    }
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_gnps")
    export_output(x = table, file = output[[1]])
    return(output[[1]])
  }
