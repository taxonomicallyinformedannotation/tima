#' @title Prepare libraries of spectra
#'
#' @description This function prepares spectra
#'    to be used for spectral matching
#'
#' @include export_spectra_rds.R
#' @include extract_spectra.R
#' @include get_default_paths.R
#' @include get_params.R
#' @include harmonize_spectra.R
#' @include import_spectra.R
#'
#' @param input File containing spectra
#' @param polarity MS polarity
#' @param nam_lib Metadata to identify the library
#' @param col_ad Name of the adduct in mgf
#' @param col_ce Name of the collision energy in mgf
#' @param col_ci Name of the compound id in mgf
#' @param col_em Name of the exact mass in mgf
#' @param col_in Name of the InChI in mgf
#' @param col_io Name of the InChI without stereo in mgf
#' @param col_ik Name of the InChIKey in mgf
#' @param col_il Name of the InChIKey without stereo in mgf
#' @param col_mf Name of the molecular formula in mgf
#' @param col_na Name of the name in mgf
#' @param col_po Name of the polarity in mgf
#' @param col_sm Name of the SMILES in mgf
#' @param col_sn Name of the SMILES without stereo in mgf
#' @param col_si Name of the spectrum id in mgf
#' @param col_sp Name of the SPLASH in mgf
#' @param col_sy Name of the synonyms in mgf
#' @param col_xl Name of the xlogp in mgf
#'
#' @return The path to the prepared spectral library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_spectra()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_spectra <-
  function(input = get_params(step = "prepare_libraries_spectra")$files$libraries$spectral$raw,
           nam_lib = get_params(step = "prepare_libraries_spectra")$names$libraries,
           col_ad = get_params(step = "prepare_libraries_spectra")$names$mgf$adduct,
           col_ce = get_params(step = "prepare_libraries_spectra")$names$mgf$collision_energy,
           col_ci = get_params(step = "prepare_libraries_spectra")$names$mgf$compound_id,
           col_em = get_params(step = "prepare_libraries_spectra")$names$mgf$exact_mass,
           col_in = get_params(step = "prepare_libraries_spectra")$names$mgf$inchi,
           col_io = get_params(step = "prepare_libraries_spectra")$names$mgf$inchi_no_stereo,
           col_ik = get_params(step = "prepare_libraries_spectra")$names$mgf$inchikey,
           col_il = get_params(step = "prepare_libraries_spectra")$names$mgf$inchikey_no_stereo,
           col_mf = get_params(step = "prepare_libraries_spectra")$names$mgf$molecular_formula,
           col_na = get_params(step = "prepare_libraries_spectra")$names$mgf$name,
           col_po = get_params(step = "prepare_libraries_spectra")$names$mgf$polarity,
           col_sm = get_params(step = "prepare_libraries_spectra")$names$mgf$smiles,
           col_sn = get_params(step = "prepare_libraries_spectra")$names$mgf$smiles_no_stereo,
           col_si = get_params(step = "prepare_libraries_spectra")$names$mgf$spectrum_id,
           col_sp = get_params(step = "prepare_libraries_spectra")$names$mgf$splash,
           col_sy = get_params(step = "prepare_libraries_spectra")$names$mgf$synonyms,
           col_xl = get_params(step = "prepare_libraries_spectra")$names$mgf$xlogp) {
    output_pos <- file.path(
      get_default_paths()$data$interim$libraries$spectra$exp$path,
      paste0(nam_lib, "_pos.rds")
    )
    output_neg <- file.path(
      get_default_paths()$data$interim$libraries$spectra$exp$path,
      paste0(nam_lib, "_neg.rds")
    )
    output_sop <- file.path(
      get_default_paths()$data$interim$libraries$sop$path,
      paste0("spectral_prepared.tsv.gz")
    )
    if (!all(purrr::map(.x = list(output_neg, output_pos), .f = file.exists) |> unlist())) {
      if (is.null(input)) {
        input <- "fileDoesNotExist"
      }
      if (file.exists(input)) {
        log_debug("Importing")
        spectra <- purrr::map(.x = input, .f = import_spectra)

        log_debug("Extracting")
        spectra_extracted <- purrr::map(.x = spectra, .f = extract_spectra)
        rm(spectra)

        log_debug("Harmonizing ...")
        log_debug("... pos")
        spectra_harmonized_pos <- purrr::map(
          .x = spectra_extracted,
          .f = harmonize_spectra,
          mode = "pos",
          metad = nam_lib,
          col_ad = col_ad,
          col_ce = col_ce,
          col_ci = col_ci,
          col_em = col_em,
          col_in = col_in,
          col_io = col_io,
          col_ik = col_ik,
          col_il = col_il,
          col_mf = col_mf,
          col_na = col_na,
          col_po = col_po,
          col_sm = col_sm,
          col_sn = col_sn,
          col_si = col_si,
          col_sp = col_sp,
          col_sy = col_sy,
          col_xl = col_xl
        ) |>
          ## TODO report the issue as otherwise precursorMz is lost
          purrr::map(
            .f = function(x) {
              x <- x |> tidytable::rename(precursor_mz = precursorMz)
            }
          )
        log_debug("... neg")
        spectra_harmonized_neg <- purrr::map(
          .x = spectra_extracted,
          .f = tima:::harmonize_spectra,
          mode = "neg",
          metad = nam_lib,
          col_ad = col_ad,
          col_ce = col_ce,
          col_ci = col_ci,
          col_em = col_em,
          col_in = col_in,
          col_io = col_io,
          col_ik = col_ik,
          col_il = col_il,
          col_mf = col_mf,
          col_na = col_na,
          col_po = col_po,
          col_sm = col_sm,
          col_sn = col_sn,
          col_si = col_si,
          col_sp = col_sp,
          col_sy = col_sy,
          col_xl = col_xl
        ) |>
          ## TODO report the issue as otherwise precursorMz is lost
          purrr::map(
            .f = function(x) {
              x <- x |> tidytable::rename(precursor_mz = precursorMz)
            }
          )
        rm(spectra_extracted)

        log_debug("Extracting structures for the SOP library.")
        sop <- tidytable::bind_rows(
          spectra_harmonized_pos |>
            tidytable::bind_rows(),
          spectra_harmonized_neg |>
            tidytable::bind_rows()
        ) |>
          tidytable::distinct(
            structure_inchikey = inchikey,
            structure_smiles = smiles,
            structure_smiles_no_stereo = smiles_no_stereo
          ) |>
          tidytable::mutate(
            structure_inchikey_no_stereo = structure_inchikey |>
              gsub(pattern = "-.*", replacement = ""),
            organism_name = NA_character_
          )
      } else {
        log_debug("Your input file does not exist, returning empty lib instead.")
        spectra_harmonized_pos <- list(
          tidytable::tidytable(
            "compound_id" = "fake_compound",
            "adduct" = NA_character_,
            "collision_energy" = NA_character_,
            "exactmass" = NA_real_,
            "formula" = NA_character_,
            "inchi" = NA_character_,
            "inchi_no_stereo" = NA_character_,
            "inchikey" = NA_character_,
            "inchikey_no_stereo" = NA_character_,
            "name" = NA_character_,
            "precursorMz" = 0,
            "precursorCharge" = NA_integer_,
            "smiles" = NA_character_,
            "smiles_no_stereo" = NA_character_,
            "spectrum_id" = NA_integer_,
            "splash" = NA_character_,
            "synonyms" = NA_character_,
            "xlogp" = NA_character_,
            "rtime" = NA_real_,
            "mz" = c(1, 2, 3) |> list(),
            "intensity" = c(1, 2, 3) |> list(),
            "library" = NA_character_,
            "precursor_mz" = 0
          )
        )
        spectra_harmonized_neg <- spectra_harmonized_pos
        sop <- tidytable::tidytable(
          "structure_inchikey" = NA_character_,
          "structure_smiles" = NA_character_,
          "structure_smiles_no_stereo" = NA_character_,
          "structure_inchikey_no_stereo" = NA_character_,
          "organism_name" = NA_character_
        )
      }
      log_debug("Exporting")
      tima:::export_output(sop, file = output_sop)
      mapply(tima:::export_spectra_rds, output_pos, spectra_harmonized_pos)
      mapply(tima:::export_spectra_rds, output_neg, spectra_harmonized_neg)
      rm(spectra_harmonized_pos, spectra_harmonized_neg)
    } else {
      log_debug("Library already exists")
    }

    tima:::export_params(
      parameters = get_params(step = "prepare_libraries_spectra"),
      step = "prepare_libraries_spectra"
    )

    return(c(
      "pos" = output_pos,
      "neg" = output_neg,
      "sop" = output_sop
    ))
  }
