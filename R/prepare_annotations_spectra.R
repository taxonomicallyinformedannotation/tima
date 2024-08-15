#' @title Prepare annotations MS2
#'
#' @description This function prepares the spectral matches
#'    obtained previously to make them compatible
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
#' @examples
#' \donttest{
#' tima:::copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' data_interim <- "data/interim/"
#' dir <- paste0(github, repo)
#' input <- get_params(step = "prepare_annotations_spectra")$files$annotations$raw$spectral$spectral |>
#'   gsub(
#'     pattern = ".tsv.gz",
#'     replacement = "_pos.tsv",
#'     fixed = TRUE
#'   )
#' get_file(url = paste0(dir, input), export = input)
#' dir <- paste0(dir, data_interim)
#' prepare_annotations_spectra(
#'   input = input,
#'   str_stereo = paste0(dir, "libraries/sop/merged/structures/stereo.tsv"),
#'   str_met = paste0(dir, "libraries/sop/merged/structures/metadata.tsv"),
#'   str_nam = paste0(dir, "libraries/sop/merged/structures/names.tsv"),
#'   str_tax_cla = paste0(dir, "libraries/sop/merged/structures/taxonomies/classyfire.tsv"),
#'   str_tax_npc = paste0(dir, "libraries/sop/merged/structures/taxonomies/npc.tsv")
#' )
#' unlink("data", recursive = TRUE)
#' }
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
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::bind_rows() |>
      tidytable::filter(!is.na(feature_id)) |>
      tidytable::distinct(
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
      tidytable::mutate(
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

    export_params(
      parameters = get_params(step = "prepare_annotations_spectra"),
      step = "prepare_annotations_spectra"
    )
    export_output(x = table, file = output[[1]])
    rm(table)

    return(output[[1]])
  }
