#' @title Prepare libraries of structure organism pairs HMDB
#'
#' @description This function prepares the HMDB structure-organism pairs
#'
#' @include fake_sop_columns.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Input file
#' @param output Output file
#'
#' @return The path to the prepared structure-organism pairs library HMDB
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_hmdb()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_hmdb <-
  function(input = get_params(step = "prepare_libraries_sop_hmdb")$files$libraries$sop$raw$hmdb,
           output = get_params(step = "prepare_libraries_sop_hmdb")$files$libraries$sop$prepared$hmdb) {
    if (file.exists(input)) {
      log_debug("Unzipping HMDB...")
      hmdb_prepared <- tryCatch(
        expr = {
          utils::unzip(zipfile = input, exdir = dirname(input))
          hmdb_structures <- input |>
            gsub(
              pattern = ".zip",
              replacement = ".sdf",
              fixed = TRUE
            )
          log_debug(x = "Loading HMDB...")
          sdf_data <- readLines(con = hmdb_structures, warn = FALSE)

          find_fixed_pattern_line_in_file <- function(file, pattern) {
            return(file |>
              stringi::stri_detect_fixed(pattern = pattern) |>
              which())
          }

          return_next_line <- function(x, file) {
            file[x + 1]
          }

          patterns <- list(
            "id" = "> <DATABASE_ID>",
            "smiles" = "> <SMILES>",
            ## Not needed
            # "inchi" = "> <INCHI_IDENTIFIER>",
            "inchikey" = "> <INCHI_KEY>",
            "formula" = "> <FORMULA>",
            "mass" = "> <EXACT_MASS>",
            "logp" = "> <JCHEM_LOGP>",
            "name" = "> <GENERIC_NAME>"
          )

          hmdb_list <- patterns |>
            purrr::map(.f = find_fixed_pattern_line_in_file, file = sdf_data) |>
            purrr::map(.f = return_next_line, file = sdf_data)

          # Function to align a vector to the reference (id)
          align_column <- function(ref, col) {
            aligned <- rep(NA, length(ref))
            j <- 1

            for (i in seq_along(ref)) {
              if (j <= length(col) && !is.na(col[j])) {
                aligned[i] <- col[j]
                j <- j + 1
              }
            }
            return(aligned)
          }

          hmdb_df <- data.frame(
            id = hmdb_list$id,
            smiles = hmdb_list$smiles,
            inchikey = hmdb_list$inchikey,
            formula = hmdb_list$formula,
            mass = align_column(ref = hmdb_list$id, hmdb_list$mass),
            logp = align_column(ref = hmdb_list$id, hmdb_list$logp),
            name = hmdb_list$name
          )

          log_debug(x = "Formatting HMDB...")
          hmdb_prepared <- hmdb_df |>
            tidytable::mutate(tidytable::across(.cols = tidyselect::everything(), .fns = tidytable::na_if, "")) |>
            tidytable::filter(!is.na(inchikey)) |>
            tidytable::mutate(
              structure_inchikey_no_stereo = stringi::stri_sub(
                str = inchikey,
                from = 1,
                to = 14
              ),
              ## ISSUE see #19
              structure_smiles_no_stereo = NA_character_,
            ) |>
            tidytable::select(
              structure_name = name,
              structure_inchikey = inchikey,
              structure_smiles = smiles,
              structure_inchikey_no_stereo,
              structure_smiles_no_stereo,
              structure_molecular_formula = formula,
              structure_exact_mass = mass,
              structure_xlogp = logp
            ) |>
            tidytable::mutate(
              structure_taxonomy_npclassifier_01pathway = NA_character_,
              structure_taxonomy_npclassifier_02superclass = NA_character_,
              structure_taxonomy_npclassifier_03class = NA_character_,
              structure_taxonomy_classyfire_chemontid = NA_character_,
              structure_taxonomy_classyfire_01kingdom = NA_character_,
              structure_taxonomy_classyfire_02superclass = NA_character_,
              structure_taxonomy_classyfire_03class = NA_character_,
              structure_taxonomy_classyfire_04directparent = NA_character_,
            ) |>
            tidytable::mutate(
              organism_name = "Homo sapiens",
              organism_taxonomy_ottid = 770315,
              organism_taxonomy_01domain = "Eukaryota",
              organism_taxonomy_02kingdom = "Metazoa",
              organism_taxonomy_03phylum = "Chordata",
              organism_taxonomy_04class = "Mammalia",
              organism_taxonomy_05order = "Primates",
              organism_taxonomy_06family = "Hominidae",
              organism_taxonomy_07tribe = NA_character_,
              organism_taxonomy_08genus = "Homo",
              organism_taxonomy_09species = "Homo sapiens",
              organism_taxonomy_10varietas = NA_character_,
              reference_doi = NA_character_
            ) |>
            round_reals() |>
            tidytable::distinct()

          log_debug("Deleting unzipped file...")
          file.remove(hmdb_structures)
          hmdb_prepared
        },
        error = function(e) {
          log_debug("Something went wrong, see original error message:")
          log_debug(e)
          hmdb_prepared <- fake_sop_columns()
        }
      )
    } else {
      log_debug("Sorry, HMDB not found, returning an empty file instead")
      hmdb_prepared <- fake_sop_columns()
    }
    log_debug(x = "Exporting ...")
    export_output(x = hmdb_prepared, file = output)
    return(output)
  }
