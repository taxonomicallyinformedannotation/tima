#' @title Split structure-organism pairs table
#'
#' @description Splits a concatenated structure-organism pairs (SOP) table into
#'     separate normalized tables for structures, organisms, and their
#'     relationships. Processes SMILES strings and creates standardized
#'     reference tables.
#'
#' @include clean_collapse.R
#' @include process_smiles.R
#' @include validators.R
#' @include logging_helpers.R
#'
#' @param table Data frame containing combined structure-organism pair data with
#'     columns for structures (SMILES, InChI, names), organisms, and references
#' @param cache Path to cache file for previously processed SMILES, or NULL to
#'     skip caching
#'
#' @return List of normalized data frames:
#'   \item{table_keys}{Structure-organism pairs with reference DOIs}
#'   \item{table_structures_stereo}{Structure stereochemistry information}
#'   \item{table_organisms}{Organism taxonomy information}
#'   \item{table_structural}{Processed and standardized structure data}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Split concatenated library
#' result <- split_tables_sop(
#'   table = combined_sop_data,
#'   cache = "data/processed_smiles.tsv"
#' )
#'
#' # Access individual tables
#' structures <- result$table_structural
#' organisms <- result$table_organisms
#' }
split_tables_sop <- function(table, cache) {
  # Input Validation ----
  validate_dataframe(table, param_name = "table")

  # Early exit for empty input
  if (nrow(table) == 0L) {
    logger::log_warn("Empty table provided")
    return(create_empty_sop_tables())
  }

  logger::log_info("Splitting SOP library into standardized components")
  logger::log_debug("Input: {nrow(table)} rows")

  table <- table |>
    tidytable::mutate(
      structure_smiles_initial = tidytable::coalesce(
        structure_smiles,
        structure_smiles_no_stereo
      )
    )
  table_structural_initial <- table |>
    tidytable::select(
      tidyselect::contains(match = "structure")
    ) |>
    tidytable::distinct()

  table_organisms <- table |>
    tidytable::select(
      tidyselect::contains(match = "organism")
    ) |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::distinct()

  # logger::log_trace("Sanitizing structures")
  table_structural_standardized <- table_structural_initial |>
    process_smiles(cache = cache)

  table_structural <- table_structural_initial |>
    tidytable::select(
      structure_smiles_initial,
      structure_name,
      tidyselect::contains(match = "_tax")
    ) |>
    tidytable::distinct() |>
    tidytable::inner_join(y = table_structural_standardized) |>
    tidytable::distinct()
  rm(table_structural_initial, table_structural_standardized)

  table <- table |>
    tidytable::select(
      structure_smiles_initial,
      tidyselect::contains(match = "organism"),
      tidyselect::contains(match = "reference")
    ) |>
    tidytable::distinct() |>
    tidytable::inner_join(y = table_structural) |>
    tidytable::distinct()

  table_keys <- table |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_smiles_no_stereo)) |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles_no_stereo,
      organism_name,
      reference_doi
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey, organism_name) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidytable::filter(!is.na(reference_doi) | n == 1) |>
    tidytable::select(-n)
  rm(table)
  log_with_count("Referenced structure-organism pairs", n = nrow(table_keys))

  table_structures_stereo <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(
      !is.na(structure_smiles) |
        !is.na(structure_smiles_no_stereo)
    ) |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo
    ) |>
    tidytable::distinct()

  # Calculate structure statistics
  n_stereoisomers <- nrow(
    table_structures_stereo |>
      tidytable::filter(
        !grepl(INCHI_NO_STEREO_PATTERN, structure_inchikey, fixed = TRUE)
      ) |>
      tidytable::distinct(structure_inchikey)
  )
  n_no_stereo <- nrow(
    table_structures_stereo |>
      tidytable::filter(grepl(
        INCHI_NO_STEREO_PATTERN,
        structure_inchikey,
        fixed = TRUE
      )) |>
      tidytable::distinct(structure_inchikey)
  )
  n_constitutional <- nrow(
    table_structures_stereo |>
      tidytable::distinct(structure_inchikey_connectivity_layer)
  )

  logger::log_info(
    "Structures: {format_count(n_stereoisomers)} stereoisomers, ",
    "{format_count(n_no_stereo)} without stereochemistry, ",
    "{format_count(n_constitutional)} constitutional isomers"
  )

  table_structures_metadata <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_molecular_formula)) |>
    tidytable::filter(!is.na(structure_exact_mass)) |>
    # tidytable::filter(!is.na(structure_xlogp)) |>
    tidytable::select(
      structure_inchikey,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp
    ) |>
    tidytable::distinct()

  table_structures_names <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_name)) |>
    tidytable::select(structure_inchikey, structure_name) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey) |>
    clean_collapse(cols = c("structure_name"))

  table_structures_taxonomy_npc <- table_structural |>
    tidytable::filter(!is.na(structure_smiles_no_stereo)) |>
    tidytable::filter(
      !is.na(structure_tax_npc_01pat) |
        !is.na(structure_tax_npc_02sup) |
        !is.na(structure_tax_npc_03cla)
    ) |>
    tidytable::select(
      structure_smiles_no_stereo,
      structure_tax_npc_01pat,
      structure_tax_npc_02sup,
      structure_tax_npc_03cla
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_smiles_no_stereo) |>
    clean_collapse(
      cols = c(
        "structure_tax_npc_01pat",
        "structure_tax_npc_02sup",
        "structure_tax_npc_03cla"
      )
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::replace_na(.x = .x, replace = "notClassified")
    ))

  table_structures_taxonomy_cla <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::filter(!is.na(structure_tax_cla_chemontid)) |>
    tidytable::select(
      structure_inchikey_connectivity_layer,
      structure_tax_cla_chemontid,
      structure_tax_cla_01kin,
      structure_tax_cla_02sup,
      structure_tax_cla_03cla,
      structure_tax_cla_04dirpar
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey_connectivity_layer) |>
    clean_collapse(
      cols = c(
        "structure_tax_cla_chemontid",
        "structure_tax_cla_01kin",
        "structure_tax_cla_02sup",
        "structure_tax_cla_03cla",
        "structure_tax_cla_04dirpar"
      )
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::replace_na(.x = .x, replace = "notClassified")
    ))

  table_organisms_names <- table_organisms |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::select(organism_name) |>
    tidytable::distinct()

  log_with_count("Unique organisms", n = nrow(table_organisms_names))

  table_org_tax_ott <- table_organisms |>
    tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
    tidytable::distinct() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::replace_na(.x = .x, replace = "notClassified")
    ))
  rm(table_organisms)

  tables <-
    list(
      "key" = table_keys,
      # "org_nam" = table_organisms_names,
      "org_tax_ott" = table_org_tax_ott,
      "str_stereo" = table_structures_stereo,
      "str_met" = table_structures_metadata,
      "str_nam" = table_structures_names,
      "str_tax_cla" = table_structures_taxonomy_cla,
      "str_tax_npc" = table_structures_taxonomy_npc
    )

  rm(
    table_keys,
    # table_organisms_names,
    table_org_tax_ott,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_names,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc
  )
  return(tables)
}

# Helper Functions ----

#' Create empty SOP tables structure
#' @keywords internal
create_empty_sop_tables <- function() {
  list(
    table_keys = tidytable::tidytable(),
    table_structures_stereo = tidytable::tidytable(),
    table_organisms = tidytable::tidytable(),
    table_structural = tidytable::tidytable()
  )
}
