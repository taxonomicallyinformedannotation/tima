#' @title Prepare annotations GNPS
#'
#' @description This function prepares GNPS obtained annotations for further use
#'
#' @param output Input file
#' @param output Output file
#' @param str_2D_3D File containing 2D and 3D structures
#' @param str_met File containing structures metadata
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param parameters Parameters
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr bind_rows mutate mutate_all na_if select
#' @importFrom readr read_tsv
#'
#' @examples NULL
prepare_annotations_gnps <-
  function(input = params$files$annotations$raw$spectral,
           output = params$files$annotations$pretreated,
           str_2D_3D = paths$data$interim$libraries$sop$merged$structures$dd_ddd,
           str_met = paths$data$interim$libraries$sop$merged$structures$metadata,
           str_nam = paths$data$interim$libraries$sop$merged$structures$names,
           str_tax_cla = paths$data$interim$libraries$sop$merged$structures$taxonomies$classyfire,
           str_tax_npc = paths$data$interim$libraries$sop$merged$structures$taxonomies$npc,
           parameters = params) {
    if (rep(TRUE, length(input)) ==
      lapply(X = input, file.exists)) {
      params <<- parameters
      log_debug("Loading and formatting GNPS results")
      ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
      table <- lapply(
        X = input,
        FUN = readr::read_tsv,
        col_types = readr::cols(.default = "c")
      ) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
          mz_error = as.numeric(MZErrorPPM) * 1E-6 * as.numeric(Precursor_MZ),
          rt_error = NA
        ) |>
        dplyr::select(
          feature_id = `#Scan#`,
          mz_error,
          rt_error,
          structure_name = Compound_Name,
          # structure_smiles = Smiles,
          score_input = MQScore,
          # smiles_2D, ## Not available for now
          structure_inchikey = InChIKey,
          structure_inchikey_2D = `InChIKey-Planar`,
          structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
          structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
          structure_taxonomy_npclassifier_03class = npclassifier_class,
          # molecular_formula, ## Not available for now
          structure_exact_mass = ExactMass
        ) |>
        dplyr::mutate(
          library = "GNPS",
          structure_smiles_2D = NA,
          structure_molecular_formula = NA,
          structure_xlogp = NA,
          ## Only partially present
          structure_taxonomy_classyfire_chemontid = NA,
          structure_taxonomy_classyfire_01kingdom = NA,
          structure_taxonomy_classyfire_02superclass = NA,
          structure_taxonomy_classyfire_03class = NA,
          structure_taxonomy_classyfire_04directparent = NA
        ) |>
        dplyr::select(
          feature_id,
          mz_error,
          rt_error,
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
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          structure_taxonomy_classyfire_chemontid,
          structure_taxonomy_classyfire_01kingdom,
          structure_taxonomy_classyfire_02superclass,
          structure_taxonomy_classyfire_03class,
          structure_taxonomy_classyfire_04directparent
        ) |>
        dplyr::mutate_all(as.character) |>
        dplyr::mutate_all(dplyr::na_if, "N/A") |>
        dplyr::mutate_all(dplyr::na_if, "null") |>
        round_reals() |>
        complement_structures_metadata(
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
        mz_error = NA,
        rt_error = NA,
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
