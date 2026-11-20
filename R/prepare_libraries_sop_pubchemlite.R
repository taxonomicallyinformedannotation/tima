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
#' @param input [character] Path to PubChem Lite CSV file
#' @param output [character] Path for prepared SOP output
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
  .assert_single_char(input, "input")
  .assert_single_char(output, "output")

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

        pubchemlite_prepared <- .build_sop_table_from_core(
          core = pubchemlite_core,
          organism_name = "xenobiotic",
          organism_taxonomy = list(
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
            organism_taxonomy_10varietas = NA_character_
          ),
          structure_tag = "xenobiotic"
        )
      }
    }

    export_output(x = pubchemlite_prepared, file = output)
    n_pairs <- nrow(pubchemlite_prepared)
    rm(pubchemlite_prepared)
    log_complete(ctx, n_pairs = n_pairs)
  } else {
    log_debug("PubChem Lite library already exists and is valid")
    log_complete(ctx, cached = TRUE)
  }

  output
}
