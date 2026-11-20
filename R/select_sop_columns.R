#' @title Select structure-organism pair columns
#'
#' @description Standardize SOP columns: structures, taxonomy, organism fields.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param df [data.frame] Data frame containing structure-organism pair data.
#'
#' @return Data frame with standardized SOP columns.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Assuming 'sop_data' has the expected column structure
#' standardized <- select_sop_columns(sop_data)
#' }
select_sop_columns <- function(df) {
  validate_dataframe(df, param_name = "df", min_rows = 0)

  if (nrow(df) == 0L) {
    log_warn("Empty dataframe in select_sop_columns")
    return(df)
  }

  head_map <- c(
    structure_name = "structure_name",
    structure_inchikey = "structure_inchikey",
    structure_smiles = "structure_smiles",
    structure_inchikey_connectivity_layer = "structure_inchikey_2D",
    structure_smiles_no_stereo = "structure_smiles_2D",
    structure_molecular_formula = "structure_molecular_formula",
    structure_exact_mass = "structure_exact_mass",
    structure_xlogp = "structure_xlogp"
  )
  tail_map <- c(
    structure_tax_npc_01pat = "structure_taxonomy_npclassifier_01pathway",
    structure_tax_npc_02sup = "structure_taxonomy_npclassifier_02superclass",
    structure_tax_npc_03cla = "structure_taxonomy_npclassifier_03class",
    structure_tax_cla_chemontid = "structure_taxonomy_classyfire_chemontid",
    structure_tax_cla_01kin = "structure_taxonomy_classyfire_01kingdom",
    structure_tax_cla_02sup = "structure_taxonomy_classyfire_02superclass",
    structure_tax_cla_03cla = "structure_taxonomy_classyfire_03class",
    structure_tax_cla_04dirpar = "structure_taxonomy_classyfire_04directparent",
    organism_name = "organism_name",
    organism_taxonomy_ottid = "organism_taxonomy_ottid",
    organism_taxonomy_01domain = "organism_taxonomy_01domain",
    organism_taxonomy_02kingdom = "organism_taxonomy_02kingdom",
    organism_taxonomy_03phylum = "organism_taxonomy_03phylum",
    organism_taxonomy_04class = "organism_taxonomy_04class",
    organism_taxonomy_05order = "organism_taxonomy_05order",
    organism_taxonomy_06family = "organism_taxonomy_06family",
    organism_taxonomy_07tribe = "organism_taxonomy_07tribe",
    organism_taxonomy_08genus = "organism_taxonomy_08genus",
    organism_taxonomy_09species = "organism_taxonomy_09species",
    organism_taxonomy_10varietas = "organism_taxonomy_10varietas",
    reference_doi = "reference_doi"
  )
  nms <- names(df)
  col_map <- if ("structure_tag" %in% nms) {
    c(head_map, c(structure_tag = "structure_tag"), tail_map)
  } else {
    c(head_map, tail_map)
  }
  missing_cols <- setdiff(unname(col_map), nms)
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      paste0(
        "Column(s) ",
        paste(missing_cols, collapse = ", "),
        " don't exist."
      )
    )
  }
  out_list <- as.list(df)[unname(col_map)]
  names(out_list) <- names(col_map)
  out_list[["structure_tax_cla_chemontid"]] <- normalize_chemontid(
    out_list[["structure_tax_cla_chemontid"]]
  )
  tidytable::as_tidytable(out_list)
}

#' Expand a 6-column structure core table into the standardized SOP schema
#' @noRd
.build_sop_table_from_core <- function(
  core,
  organism_name,
  organism_taxonomy,
  structure_tag = NULL,
  reference_doi = NA_character_
) {
  n_c <- nrow(core)
  na_chr <- rep.int(NA_character_, n_c)
  out_list <- list(
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
    structure_taxonomy_classyfire_04directparent = na_chr
  )
  if (!is.null(structure_tag)) {
    out_list[["structure_tag"]] <- rep.int(structure_tag, n_c)
  }
  out_list[["organism_name"]] <- rep.int(organism_name, n_c)
  out_list[["reference_doi"]] <- rep.int(reference_doi, n_c)
  out_list <- c(out_list, lapply(organism_taxonomy, rep.int, times = n_c))
  select_sop_columns(tidytable::as_tidytable(out_list))
}
