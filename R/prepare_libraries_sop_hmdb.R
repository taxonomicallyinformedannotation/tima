#' @title Prepare libraries of structure organism pairs HMDB
#'
#' @description This function prepares HMDB (Human Metabolome Database)
#'     structure-organism pairs by parsing SDF files, extracting metadata,
#'     and formatting for TIMA annotation workflows. Handles human metabolite
#'     data with structures and biofluid/tissue associations.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Character string path to HMDB SDF zip file
#' @param output Character string path for prepared HMDB library output
#'
#' @return Character string path to prepared HMDB structure-organism pairs
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
prepare_libraries_sop_hmdb <- function(
  input = get_params(
    step = "prepare_libraries_sop_hmdb"
  )$files$libraries$sop$raw$hmdb,
  output = get_params(
    step = "prepare_libraries_sop_hmdb"
  )$files$libraries$sop$prepared$hmdb
) {
  # Validate inputs
  if (!is.character(input) || length(input) != 1L) {
    stop("input must be a single character string")
  }

  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  logger::log_info("Preparing HMDB structure-organism pairs")

  if (!file.exists(output) || file.size(output) < 100000) {
    if (!file.exists(input)) {
      logger::log_warn("HMDB file not found: {input}")
      logger::log_info("Creating empty HMDB library")
      hmdb_prepared <- fake_sop_columns()
    } else {
      logger::log_debug("Processing HMDB from: {input}")
      # logger::log_trace("Unzipping HMDB")
      hmdb_prepared <- tryCatch(
        expr = {
          utils::unzip(
            zipfile = input,
            exdir = dirname(input)
          )
          hmdb_structures <- input |>
            gsub(
              pattern = ".zip",
              replacement = ".sdf",
              fixed = TRUE
            )
          # logger::log_trace("Loading HMDB")
          sdf_data <- readLines(con = hmdb_structures, warn = FALSE)

          find_fixed_pattern_line_in_file <- function(file, pattern) {
            return(
              file |>
                stringi::stri_detect_fixed(pattern = pattern) |>
                which()
            )
          }

          return_next_line <- function(x, file) {
            file[x + 1]
          }

          patterns <- list(
            "id" = "> <DATABASE_ID>",
            "smiles" = "> <SMILES>",
            "inchikey" = "> <INCHI_KEY>",
            "formula" = "> <FORMULA>",
            "mass" = "> <EXACT_MASS>",
            "logp" = "> <JCHEM_LOGP>",
            "name" = "> <GENERIC_NAME>"
          )

          hmdb_list <- patterns |>
            purrr::map(
              .f = find_fixed_pattern_line_in_file,
              file = sdf_data
            )

          # Function to realign vectors with missing values
          realign_vectors <- function(vec1, vec2) {
            result <- numeric(length(vec1))
            result[] <- NA
            i <- 1
            j <- 1
            while (i <= length(vec1) && j <= length(vec2)) {
              if (i < length(vec1) && vec2[j] > vec1[i + 1]) {
                i <- i + 1
              } else {
                result[i] <- vec2[j]
                i <- i + 1
                j <- j + 1
              }
            }

            return(result)
          }
          hmdb_list$mass <- realign_vectors(hmdb_list$formula, hmdb_list$mass)
          hmdb_list$logp <- realign_vectors(hmdb_list$formula, hmdb_list$logp)

          hmdb_list <- hmdb_list |>
            purrr::map(.f = return_next_line, file = sdf_data)

          hmdb_df <- data.frame(
            id = hmdb_list$id,
            smiles = hmdb_list$smiles,
            inchikey = hmdb_list$inchikey,
            formula = hmdb_list$formula,
            mass = hmdb_list$mass,
            logp = hmdb_list$logp,
            name = hmdb_list$name
          )

          # logger::log_trace("Formatting HMDB")
          hmdb_prepared <- hmdb_df |>
            tidytable::mutate(tidytable::across(
              .cols = tidyselect::everything(),
              .fns = tidytable::na_if,
              ""
            )) |>
            tidytable::filter(!is.na(inchikey)) |>
            tidytable::mutate(
              structure_inchikey_2D = stringi::stri_sub(
                str = inchikey,
                from = 1,
                to = 14
              ),
              structure_smiles_2D = NA_character_,
            ) |>
            tidytable::select(
              structure_name = name,
              structure_inchikey = inchikey,
              structure_smiles = smiles,
              structure_inchikey_2D,
              structure_smiles_2D,
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
            select_sop_columns() |>
            round_reals() |>
            tidytable::distinct()

          # logger::log_trace("Deleting unzipped file")
          file.remove(hmdb_structures)
          hmdb_prepared
        },
        error = function(e) {
          logger::log_error(
            "Something went wrong with HMDB processing: ",
            conditionMessage(e)
          )
          fake_sop_columns()
        }
      )
    }

    export_output(x = hmdb_prepared, file = output)
  } else {
    # logger::log_trace("HMDB library already exists and is valid")
  }

  return(output)
}
