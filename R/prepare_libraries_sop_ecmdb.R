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
  if (!is.character(input) || length(input) != 1L) {
    cli::cli_abort(
      "input must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Preparing ECMDB structure-organism pairs")

  if (!file.exists(output) || file.size(output) < 100000) {
    if (!file.exists(input)) {
      log_warn("ECMDB file not found: %s", input)
      log_info("Creating empty ECMDB library")
      ecmdb_prepared <- fake_sop_columns()
    } else {
      log_debug("Processing ECMDB from: %s", input)

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

      n_c <- nrow(core)
      na_chr <- rep.int(NA_character_, n_c)
      ecmdb_prepared <- tidytable::tidytable(
        structure_name = core$structure_name,
        structure_inchikey = core$structure_inchikey,
        structure_smiles = core$structure_smiles,
        structure_inchikey_2D = na_chr,
        structure_smiles_2D = na_chr,
        structure_molecular_formula = core$structure_molecular_formula,
        structure_exact_mass = core$structure_exact_mass,
        structure_xlogp = core$structure_xlogp,
        structure_taxonomy_npclassifier_01pathway = na_chr,
        structure_taxonomy_npclassifier_02superclass = na_chr,
        structure_taxonomy_npclassifier_03class = na_chr,
        structure_taxonomy_classyfire_chemontid = na_chr,
        structure_taxonomy_classyfire_01kingdom = na_chr,
        structure_taxonomy_classyfire_02superclass = na_chr,
        structure_taxonomy_classyfire_03class = na_chr,
        structure_taxonomy_classyfire_04directparent = na_chr,
        organism_name = rep.int("Escherichia coli", n_c),
        organism_taxonomy_ottid = rep.int(474506, n_c),
        organism_taxonomy_01domain = rep.int("Bacteria", n_c),
        organism_taxonomy_02kingdom = na_chr,
        organism_taxonomy_03phylum = rep.int("Proteobacteria", n_c),
        organism_taxonomy_04class = rep.int("Gammaproteobacteria", n_c),
        organism_taxonomy_05order = rep.int("Enterobacteriales", n_c),
        organism_taxonomy_06family = rep.int("Enterobacteriaceae", n_c),
        organism_taxonomy_07tribe = na_chr,
        organism_taxonomy_08genus = na_chr,
        organism_taxonomy_09species = rep.int("Escherichia coli", n_c),
        organism_taxonomy_10varietas = na_chr,
        reference_doi = rep.int(NA, n_c)
      ) |>
        select_sop_columns()
      rm(core)
    }

    export_params(
      parameters = get_params(step = "prepare_libraries_sop_ecmdb"),
      step = "prepare_libraries_sop_ecmdb"
    )
    export_output(x = ecmdb_prepared, file = output)
    rm(ecmdb_prepared)
  } else {
    return(output)
  }

  output
}
