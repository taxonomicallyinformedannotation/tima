#' @title Prepare libraries of structure organism pairs ECMDB
#'
#' @description This function prepares ECMDB (E. coli Metabolome Database)
#'     structure-organism pairs by parsing JSON data, extracting metabolite
#'     information, and formatting for TIMA workflows. Handles E. coli
#'     metabolite data with structures.
#'
#' @include fake_sop_columns.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Character string path to ECMDB JSON zip file
#' @param output Character string path for prepared ECMDB library output
#'
#' @return Character string path to prepared ECMDB structure-organism pairs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_ecmdb()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_ecmdb <- function(
  input = get_params(
    step = "prepare_libraries_sop_ecmdb"
  )$files$libraries$sop$raw$ecmdb,
  output = get_params(
    step = "prepare_libraries_sop_ecmdb"
  )$files$libraries$sop$prepared$ecmdb
) {
  # Validate inputs
  if (!is.character(input) || length(input) != 1L) {
    stop("input must be a single character string")
  }

  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  logger::log_info("Preparing ECMDB structure-organism pairs")

  if (!file.exists(output) || file.size(output) < 100000) {
    if (!file.exists(input)) {
      logger::log_warn("ECMDB file not found: ", input)
      logger::log_info("Creating empty ECMDB library")
      ecmdb_prepared <- fake_sop_columns()
    } else {
      logger::log_debug("Processing ECMDB from: {input}")
      logger::log_trace("Loading ECMDB resources")

      file <- gsub(
        pattern = ".zip",
        replacement = "",
        x = input,
        fixed = TRUE
      ) |>
        gsub(
          pattern = ".*/",
          replacement = "",
          perl = TRUE
        )

      ecmdb <- jsonlite::stream_in(
        con = unz(description = input, filename = file),
        verbose = FALSE
      ) |>
        data.frame() |>
        tidytable::as_tidytable()

      logger::log_trace("Formatting ECMDB")
      ecmdb_prepared <- ecmdb |>
        tidytable::mutate(
          structure_inchikey_2D = stringi::stri_sub(
            str = moldb_inchikey,
            from = 1,
            to = 14
          ),
          structure_smiles_2D = NA_character_
        ) |>
        tidytable::rename(
          structure_name = name,
          structure_inchikey = moldb_inchikey,
          structure_smiles = moldb_smiles,
          structure_molecular_formula = moldb_formula,
          structure_exact_mass = moldb_mono_mass,
          structure_xlogp = moldb_logp
        ) |>
        tidytable::mutate(
          structure_taxonomy_npclassifier_01pathway = NA_character_,
          structure_taxonomy_npclassifier_02superclass = NA_character_,
          structure_taxonomy_npclassifier_03class = NA_character_,
          structure_taxonomy_classyfire_chemontid = NA_character_,
          structure_taxonomy_classyfire_01kingdom = NA_character_,
          structure_taxonomy_classyfire_02superclass = NA_character_,
          structure_taxonomy_classyfire_03class = NA_character_,
          structure_taxonomy_classyfire_04directparent = NA_character_
        ) |>
        tidytable::mutate(
          organism_name = "Escherichia coli",
          organism_taxonomy_ottid = 474506,
          organism_taxonomy_01domain = "Bacteria",
          organism_taxonomy_02kingdom = NA_character_,
          organism_taxonomy_03phylum = "Proteobacteria",
          organism_taxonomy_04class = "Gammaproteobacteria",
          organism_taxonomy_05order = "Enterobacteriales",
          organism_taxonomy_06family = "Enterobacteriaceae",
          organism_taxonomy_07tribe = NA_character_,
          organism_taxonomy_08genus = NA_character_,
          organism_taxonomy_09species = "Escherichia coli",
          organism_taxonomy_10varietas = NA_character_
        ) |>
        tidytable::mutate(reference_doi = NA) |>
        select_sop_columns() |>
        round_reals() |>
        tidytable::distinct()
      rm(ecmdb)
    }

    export_params(
      parameters = get_params(step = "prepare_libraries_sop_ecmdb"),
      step = "prepare_libraries_sop_ecmdb"
    )
    export_output(x = ecmdb_prepared, file = output)
    rm(ecmdb_prepared)
  } else {
    logger::log_trace("ECMDB library already exists and is valid")
  }

  return(output)
}
