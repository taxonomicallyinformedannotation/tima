#' @title Prepare libraries of structure organism pairs PubChem Lite
#'
#' @description This function prepares the PubChem Lite CCSbase export for
#'     exposomics as a xenobiotic structure-organism pairs library.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include safe_fread.R
#' @include select_sop_columns.R
#' @include logs_utils.R
#'
#' @param input [character] Character string path to PubChem Lite CSV file
#' @param output [character] Character string path for prepared SOP output
#'
#' @return Character string path to prepared SOP file
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_pubchemlite()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_pubchemlite <- function(
  input = get_params(
    step = "prepare_libraries_sop_pubchemlite"
  )$files$libraries$sop$raw$pubchemlite,
  output = get_params(
    step = "prepare_libraries_sop_pubchemlite"
  )$files$libraries$sop$prepared$pubchemlite
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

  ctx <- log_operation("prepare_libraries_sop_pubchemlite", input = input)

  if (!file.exists(output) || file.size(output) < 100000) {
    if (!file.exists(input)) {
      log_warn("PubChem Lite file not found: %s", input)
      pubchemlite_prepared <- fake_sop_columns()
    } else {
      log_debug("Processing PubChem Lite from: %s", input)

      pubchemlite <- safe_fread(
        file = input,
        file_type = "PubChem Lite structure-organism pairs",
        na.strings = c("", "NA"),
        colClasses = "character"
      )

      if (nrow(pubchemlite) == 0L) {
        pubchemlite_prepared <- fake_sop_columns()
      } else {
        pubchemlite_prepared <- pubchemlite |>
          tidytable::mutate(tidytable::across(
            .cols = tidyselect::everything(),
            .fns = tidytable::na_if,
            ""
          )) |>
          tidytable::mutate(
            structure_inchikey_2D = NA_character_,
            structure_smiles_2D = NA_character_
          ) |>
          tidytable::rename(
            structure_name = CompoundName,
            structure_inchikey = InChIKey,
            structure_smiles = SMILES,
            structure_molecular_formula = MolecularFormula,
            structure_exact_mass = MonoisotopicMass,
            structure_xlogp = XLogP
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
            structure_tag = "xenobiotic",
            organism_name = "xenobiotic",
            organism_taxonomy_ottid = "93302", # complicated placeholder, different from Biota
            organism_taxonomy_01domain = NA_character_,
            organism_taxonomy_02kingdom = NA_character_,
            organism_taxonomy_03phylum = NA_character_,
            organism_taxonomy_04class = NA_character_,
            organism_taxonomy_05order = NA_character_,
            organism_taxonomy_06family = NA_character_,
            organism_taxonomy_07tribe = NA_character_,
            organism_taxonomy_08genus = NA_character_,
            organism_taxonomy_09species = NA_character_,
            organism_taxonomy_10varietas = NA_character_,
            reference_doi = NA_character_
          ) |>
          select_sop_columns() |>
          round_reals() |>
          tidytable::distinct()
      }
    }

    export_output(x = pubchemlite_prepared, file = output)
    log_complete(ctx, n_pairs = nrow(pubchemlite_prepared))
  } else {
    log_debug("PubChem Lite library already exists and is valid")
    log_complete(ctx, cached = TRUE)
    output
  }

  output
}
