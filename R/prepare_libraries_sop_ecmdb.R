#' @title Prepare libraries of structure organism pairs ECMDB
#'
#' @description This function prepares ECMDB (E. coli Metabolome Database)
#'     structure-organism pairs by parsing JSON data, extracting metabolite
#'     information, and formatting for TIMA workflows. Handles E. coli
#'     metabolite data with structures.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#' @include logs_utils.R
#' @include export_params.R
#' @include export_output.R
#'
#' @param input [character] Path to ECMDB JSON zip file
#' @param output [character] Path for prepared ECMDB library
#'     output
#'
#' @return Character string path to prepared ECMDB structure-organism pairs
#'
#' @family preparation
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
  .assert_single_char(input, "input")
  .assert_single_char(output, "output")

  log_info("Preparing ECMDB structure-organism pairs")

  if (!file.exists(output) || file.size(output) < 100000) {
    if (!file.exists(input)) {
      log_warn("ECMDB file not found: %s", input)
      log_info("Creating empty ECMDB library")
      ecmdb_prepared <- fake_sop_columns()
    } else {
      log_debug("Processing ECMDB from: %s", input)

      file <- basename(input) |>
        gsub(
          pattern = ".zip",
          replacement = "",
          fixed = TRUE
        )

      ecmdb <- jsonlite::stream_in(
        con = unz(description = input, filename = file),
        verbose = FALSE
      ) |>
        tidytable::as_tidytable()

      core <- tidytable::tidytable(
        structure_name = ecmdb$name,
        structure_inchikey = ecmdb$moldb_inchikey,
        structure_smiles = ecmdb$moldb_smiles,
        structure_molecular_formula = ecmdb$moldb_formula,
        structure_exact_mass = ecmdb$moldb_mono_mass,
        structure_xlogp = ecmdb$moldb_logp
      ) |>
        round_reals() |>
        tidytable::distinct()
      rm(ecmdb)

      ecmdb_prepared <- .build_sop_table_from_core(
        core = core,
        organism_name = "Escherichia coli",
        organism_taxonomy = list(
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
        ),
        reference_doi = NA
      )
      rm(core)
    }

    export_params(
      parameters = get_params(step = "prepare_libraries_sop_ecmdb"),
      step = "prepare_libraries_sop_ecmdb"
    )
    export_output(x = ecmdb_prepared, file = output)
  }

  output
}
