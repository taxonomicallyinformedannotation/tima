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
#' @include export_output.R
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
        clean_col <- function(col) {
          if (is.null(col)) {
            return(rep(NA_character_, nrow(pubchemlite)))
          }
          col[!nzchar(col)] <- NA_character_
          col
        }
        pubchemlite_core <- tidytable::tidytable(
          structure_name = clean_col(pubchemlite[["CompoundName"]]),
          structure_inchikey = clean_col(pubchemlite[["InChIKey"]]),
          structure_smiles = clean_col(pubchemlite[["SMILES"]]),
          structure_molecular_formula = clean_col(
            pubchemlite[["MolecularFormula"]]
          ),
          structure_exact_mass = clean_col(pubchemlite[["MonoisotopicMass"]]),
          structure_xlogp = clean_col(pubchemlite[["XLogP"]])
        ) |>
          round_reals() |>
          tidytable::distinct()
        rm(pubchemlite)

        n_out <- nrow(pubchemlite_core)
        na_col <- rep(NA_character_, n_out)
        pubchemlite_prepared <- tidytable::tidytable(
          structure_name = pubchemlite_core$structure_name,
          structure_inchikey = pubchemlite_core$structure_inchikey,
          structure_smiles = pubchemlite_core$structure_smiles,
          structure_inchikey_2D = na_col,
          structure_smiles_2D = na_col,
          structure_molecular_formula = pubchemlite_core$structure_molecular_formula,
          structure_exact_mass = pubchemlite_core$structure_exact_mass,
          structure_xlogp = pubchemlite_core$structure_xlogp,
          structure_taxonomy_npclassifier_01pathway = na_col,
          structure_taxonomy_npclassifier_02superclass = na_col,
          structure_taxonomy_npclassifier_03class = na_col,
          structure_taxonomy_classyfire_chemontid = na_col,
          structure_taxonomy_classyfire_01kingdom = na_col,
          structure_taxonomy_classyfire_02superclass = na_col,
          structure_taxonomy_classyfire_03class = na_col,
          structure_taxonomy_classyfire_04directparent = na_col,
          structure_tag = rep("xenobiotic", n_out),
          organism_name = rep("xenobiotic", n_out),
          organism_taxonomy_ottid = rep("93302", n_out), # complicated placeholder, different from Biota
          organism_taxonomy_01domain = na_col,
          organism_taxonomy_02kingdom = na_col,
          organism_taxonomy_03phylum = na_col,
          organism_taxonomy_04class = na_col,
          organism_taxonomy_05order = na_col,
          organism_taxonomy_06family = na_col,
          organism_taxonomy_07tribe = na_col,
          organism_taxonomy_08genus = na_col,
          organism_taxonomy_09species = na_col,
          organism_taxonomy_10varietas = na_col,
          reference_doi = na_col
        ) |>
          select_sop_columns()
      }
    }

    export_output(x = pubchemlite_prepared, file = output)
    n_pairs <- nrow(pubchemlite_prepared)
    rm(pubchemlite_prepared)
    log_complete(ctx, n_pairs = n_pairs)
  } else {
    log_debug("PubChem Lite library already exists and is valid")
    log_complete(ctx, cached = TRUE)
    output
  }

  output
}
