#' @title Split structure-organism pairs table
#'
#' @description Splits a concatenated structure-organism pairs (SOP) table into
#'     separate normalized tables for structures, organisms, and their
#'     relationships. Processes SMILES strings and creates standardized
#'     reference tables.
#'
#' @include columns_utils.R
#' @include process_smiles.R
#' @include logs_utils.R
#' @include validations_utils.R
#'
#' @param table [data.frame] Data frame containing combined structure-organism pair data with
#'     columns for structures (SMILES, InChI, names), organisms, and references
#' @param cache [character] Path to cache file for previously processed SMILES, or NULL to
#'     skip caching
#'
#' @return List of normalized data frames:
#'   \item{key}{Structure-organism pairs with reference DOIs}
#'   \item{org_tax_ott}{Organism taxonomy information}
#'   \item{str_can}{Canonicalization mapping (smiles_initial -> smiles)}
#'   \item{str_stereo}{Structure stereochemistry with name, tag, xlogp}
#'   \item{str_met}{Stereo-invariant physical properties (formula, mass)}
#'   \item{str_tax_cla}{ClassyFire taxonomy keyed by inchikey}
#'   \item{str_tax_npc}{NPClassifier taxonomy keyed by smiles (with stereo)}
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
#' structures <- result$str_stereo
#' organisms <- result$org_tax_ott
#' }
split_tables_sop <- function(table, cache) {
  # Input Validation ----
  validate_dataframe(table, param_name = "table")

  # Early exit for empty input
  if (nrow(table) == 0L) {
    log_warn("Empty table provided")
    return(create_empty_sop_tables())
  }

  log_info("Splitting SOP library into standardized components")
  log_debug("Input: %d rows", nrow(table))

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

  # log_trace("Sanitizing structures")
  table_structural_standardized <- table_structural_initial |>
    process_smiles(cache = cache)

  table_structural <- table_structural_initial |>
    tidytable::select(
      structure_smiles_initial,
      structure_name,
      tidyselect::contains(match = "_tag"),
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

  # str_can: Canonicalization mapping (smiles_initial -> smiles with stereo)
  table_structures_canonical <- table_structural |>
    tidytable::filter(!is.na(structure_smiles_initial)) |>
    tidytable::filter(!is.na(structure_smiles)) |>
    tidytable::select(
      structure_smiles_initial,
      structure_smiles
    ) |>
    tidytable::distinct(structure_smiles_initial, .keep_all = TRUE)

  # str_stereo: keyed by full inchikey, includes name, tag, xlogp
  table_structures_stereo <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(
      !is.na(structure_smiles) |
        !is.na(structure_smiles_no_stereo)
    ) |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::mutate(
      structure_inchikey_no_stereo = tidytable::if_else(
        !is.na(structure_inchikey) & nchar(structure_inchikey) >= 27L,
        paste0(
          stringi::stri_sub(str = structure_inchikey, from = 1L, to = 14L),
          "-",
          stringi::stri_sub(str = structure_inchikey, from = -1L, to = -1L)
        ),
        NA_character_
      )
    )

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

  log_info(
    "Structures: %s stereoisomers, %s without stereochemistry, %s constitutional isomers",
    format_count(n_stereoisomers),
    format_count(n_no_stereo),
    format_count(n_constitutional)
  )

  # Collapse name, tag, xlogp per inchikey
  # Ensure structure_xlogp column exists before selecting/summarizing
  # (avoids expensive per-group pick(everything()) check)
  if (!"structure_xlogp" %in% names(table_structures_stereo)) {
    table_structures_stereo$structure_xlogp <- NA_character_
  }
  table_structures_stereo <- table_structures_stereo |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_connectivity_layer,
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo,
      structure_xlogp,
      structure_name,
      structure_tag
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_connectivity_layer,
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo
    ) |>
    tidytable::summarize(
      structure_xlogp = .resolve_numeric_or_na(structure_xlogp),
      structure_name = .collapse_harmonized_names(structure_name),
      structure_tag = .collapse_unique_non_empty(structure_tag)
    ) |>
    tidytable::ungroup()

  # str_met: Stereo-invariant properties keyed by inchikey_no_stereo
  ## formula and exact_mass are deterministically derived from SMILES via
  ## process_smiles() and are stereo-invariant (same connectivity + protonation
  ## = same formula = same mass).
  table_structures_metadata <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_molecular_formula)) |>
    tidytable::filter(!is.na(structure_exact_mass)) |>
    tidytable::mutate(
      structure_inchikey_no_stereo = tidytable::if_else(
        !is.na(structure_inchikey) & nchar(structure_inchikey) >= 27L,
        paste0(
          stringi::stri_sub(str = structure_inchikey, from = 1L, to = 14L),
          "-",
          stringi::stri_sub(str = structure_inchikey, from = -1L, to = -1L)
        ),
        NA_character_
      )
    ) |>
    tidytable::select(
      structure_inchikey_no_stereo,
      structure_molecular_formula,
      structure_exact_mass
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey_no_stereo) |>
    tidytable::summarize(
      structure_molecular_formula = .resolve_single_or_na(
        structure_molecular_formula
      ),
      structure_exact_mass = .resolve_numeric_or_na(structure_exact_mass)
    ) |>
    tidytable::ungroup()

  # str_tax_npc: keyed by canonical SMILES with stereo
  table_structures_taxonomy_npc <- table_structural |>
    tidytable::filter(!is.na(structure_smiles)) |>
    tidytable::filter(
      !is.na(structure_tax_npc_01pat) |
        !is.na(structure_tax_npc_02sup) |
        !is.na(structure_tax_npc_03cla)
    ) |>
    tidytable::select(
      structure_smiles,
      structure_tax_npc_01pat,
      structure_tax_npc_02sup,
      structure_tax_npc_03cla
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_smiles) |>
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

  # str_tax_cla: keyed by full inchikey
  table_structures_taxonomy_cla <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_tax_cla_chemontid)) |>
    tidytable::select(
      structure_inchikey,
      structure_tax_cla_chemontid,
      structure_tax_cla_01kin,
      structure_tax_cla_02sup,
      structure_tax_cla_03cla,
      structure_tax_cla_04dirpar
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey) |>
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
      "str_can" = table_structures_canonical,
      "str_stereo" = table_structures_stereo,
      "str_met" = table_structures_metadata,
      "str_tax_cla" = table_structures_taxonomy_cla,
      "str_tax_npc" = table_structures_taxonomy_npc
    )

  rm(
    table_keys,
    # table_organisms_names,
    table_org_tax_ott,
    table_structures_canonical,
    table_structures_stereo,
    table_structures_metadata,
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
    "key" = tidytable::tidytable(),
    "org_tax_ott" = tidytable::tidytable(),
    "str_can" = tidytable::tidytable(),
    "str_stereo" = tidytable::tidytable(),
    "str_met" = tidytable::tidytable(),
    "str_tax_cla" = tidytable::tidytable(),
    "str_tax_npc" = tidytable::tidytable()
  )
}
