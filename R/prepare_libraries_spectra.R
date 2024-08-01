import::from(stringi, stri_sub, .into = environment())
import::from(tidytable, bind_cols, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, tidytable, .into = environment())

#' @title Prepare libraries of spectra
#'
#' @description This function prepares spectra
#'    to be used for spectral matching
#'
#' @importFrom stringi stri_sub
#' @importFrom tidytable bind_cols
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable mutate
#' @importFrom tidytable tidytable
#'
#' @include export_spectra_2.R
#' @include extract_spectra.R
#' @include get_params.R
#' @include harmonize_spectra.R
#' @include import_spectra.R
#' @include parse_yaml_paths.R
#' @include sanitize_spectra.R
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
#' @return NULL
#'
#' @export
#'
#' @examples NULL
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
      parse_yaml_paths()$data$interim$libraries$spectra$exp$path,
      paste0(nam_lib, "_pos.rds")
    )
    output_neg <- file.path(
      parse_yaml_paths()$data$interim$libraries$spectra$exp$path,
      paste0(nam_lib, "_neg.rds")
    )
    output_sop <- file.path(
      parse_yaml_paths()$data$interim$libraries$sop$path,
      paste0("spectral_prepared.tsv.gz")
    )
    if (!all(lapply(X = list(output_neg, output_pos), FUN = file.exists) |> unlist())) {
      if (is.null(input)) {
        input <- "fileDoesNotExist"
      }
      if (file.exists(input)) {
        log_debug("Importing")
        spectra <- lapply(X = input, FUN = import_spectra)

        log_debug("Sanitizing")
        spectra_sanitized <- lapply(X = spectra, FUN = sanitize_spectra)
        rm(spectra)

        log_debug("Extracting")
        spectra_extracted <- lapply(X = spectra_sanitized, FUN = extract_spectra)
        rm(spectra_sanitized)

        log_debug("Harmonizing ...")
        log_debug("... pos")
        spectra_harmonized_pos <- lapply(
          X = spectra_extracted,
          FUN = harmonize_spectra,
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
          lapply(
            FUN = function(x) {
              x <- x |> rename(precursor_mz = precursorMz)
            }
          )
        log_debug("... neg")
        spectra_harmonized_neg <- lapply(
          X = spectra_extracted,
          FUN = harmonize_spectra,
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
          lapply(
            FUN = function(x) {
              x <- x |> rename(precursor_mz = precursorMz)
            }
          )
        rm(spectra_extracted)

        log_debug("Extracting structures for the SOP library.")
        sop <- bind_rows(
          spectra_harmonized_pos |>
            bind_rows(),
          spectra_harmonized_neg |>
            bind_rows()
        ) |>
          distinct(
            structure_inchikey = inchikey,
            structure_smiles = smiles,
            structure_smiles_no_stereo = smiles_no_stereo
          ) |>
          mutate(
            structure_inchikey_no_stereo = structure_inchikey |>
              gsub(pattern = "-.*", replacement = ""),
            organism_name = NA_character_
          )
      } else {
        log_debug("Your input file does not exist, returning empty lib instead.")
        spectra_harmonized_pos <- list(
          tidytable(
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
        sop <- tidytable(
          "structure_inchikey" = NA_character_,
          "structure_smiles" = NA_character_,
          "structure_smiles_no_stereo" = NA_character_,
          "structure_inchikey_no_stereo" = NA_character_,
          "organism_name" = NA_character_
        )
      }
      log_debug("Exporting")
      export_output(sop, file = output_sop)
      mapply(
        export_spectra_2,
        output_pos,
        spectra_harmonized_pos,
        nam_lib
      )
      mapply(
        export_spectra_2,
        output_neg,
        spectra_harmonized_neg,
        nam_lib
      )
      rm(spectra_harmonized_pos, spectra_harmonized_neg)
    } else {
      log_debug("Library already exists")
    }
    export_params(
      parameters = get_params(step = "prepare_libraries_spectra"),
      step = "prepare_libraries_spectra"
    )

    return(c(
      "pos" = output_pos,
      "neg" = output_neg,
      "sop" = output_sop
    ))
  }
