#' @title Complement structural metadata
#'
#' @description Complements structural metadata by joining stereochemistry,
#'     metadata, and chemical taxonomy from reference libraries.
#'     Enriches annotation results with comprehensive structure information.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#' @include safe_fread.R
#'
#' @param df Data frame with structural metadata to complement
#' @param str_stereo Path to structure stereochemistry file (includes name, tag,
#'     xlogp)
#' @param str_met Path to structure metadata file (formula, mass)
#' @param str_tax_cla Path to ClassyFire taxonomy file (keyed by inchikey)
#' @param str_tax_npc Path to NPClassifier taxonomy file (keyed by smiles with
#'     stereo)
#'
#' @return Data frame with enriched structural metadata
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' enriched <- complement_metadata_structures(
#'   df = annotations,
#'   str_stereo = "data/str_stereo.tsv",
#'   str_met = "data/str_metadata.tsv",
#'   str_tax_cla = "data/str_tax_classyfire.tsv",
#'   str_tax_npc = "data/str_tax_npclassifier.tsv"
#' )
#' }
# Process-level cache: keyed on (str_stereo, str_met, str_tax_cla, str_tax_npc).
# Avoids re-reading the large reference tables on every annotator call.
.complement_metadata_ref_cache <- local({
  .env <- new.env(hash = TRUE, parent = emptyenv())

  load_refs <- function(str_stereo, str_met, str_tax_cla, str_tax_npc) {
    cache_key <- paste(str_stereo, str_met, str_tax_cla, str_tax_npc, sep = "|")
    cached <- get0(cache_key, envir = .env)
    if (!is.null(cached)) {
      return(cached)
    }

    stereo_cols <- .schema_stereo_cols()
    stereo_full <- .safe_fread_selected(
      file = str_stereo,
      file_type = "stereochemistry data",
      selected_cols = stereo_cols
    ) |>
      .ensure_columns(cols = stereo_cols)

    if (
      all(is.na(stereo_full$structure_inchikey_no_stereo)) &&
        nrow(stereo_full) > 0L
    ) {
      stereo_full <- stereo_full |>
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
    }

    met_cols <- .schema_metadata_cols()
    met_full <- .safe_fread_selected(
      file = str_met,
      file_type = "structure metadata",
      selected_cols = met_cols
    ) |>
      .ensure_columns(cols = met_cols)

    tax_cla_full <- safe_fread(
      file = str_tax_cla,
      file_type = "ClassyFire taxonomy",
      na.strings = c("", "NA"),
      colClasses = "character",
      select = .schema_classyfire_cols()
    ) |>
      .ensure_columns(cols = .schema_classyfire_cols()) |>
      tidytable::mutate(
        structure_tax_cla_chemontid = normalize_chemontid(
          structure_tax_cla_chemontid
        )
      )

    tax_npc_full <- safe_fread(
      file = str_tax_npc,
      file_type = "NPClassifier taxonomy",
      na.strings = c("", "NA"),
      colClasses = "character",
      select = .schema_npc_cols()
    ) |>
      .ensure_columns(cols = .schema_npc_cols())

    result <- list(
      stereo_full = stereo_full,
      met_full = met_full,
      tax_cla_full = tax_cla_full,
      tax_npc_full = tax_npc_full
    )
    assign(cache_key, result, envir = .env)
    result
  }

  list(load_refs = load_refs)
})

complement_metadata_structures <- function(
  df,
  str_stereo,
  str_met,
  str_tax_cla,
  str_tax_npc
) {
  ctx <- log_operation("complement_metadata", n_input = nrow(df))

  # Input Validation ----
  validate_dataframe(df, param_name = "df")

  # Early exit for empty input
  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided")
    return(df)
  }

  # Validate all file paths exist
  validate_file_existence(list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  log_debug("Complementing metadata for %d rows", nrow(df))

  keys <- .extract_structure_keys(df)
  if (length(keys$inchikey) == 0L && length(keys$smiles) == 0L) {
    log_debug("No structure keys found in input; returning input unchanged")
    return(.ensure_structure_placeholders(df))
  }

  # Load (or retrieve from cache) all four reference tables once per session.
  refs <- .complement_metadata_ref_cache$load_refs(
    str_stereo,
    str_met,
    str_tax_cla,
    str_tax_npc
  )

  # Filter stereo to keys present in this annotation batch.
  stereo <- refs$stereo_full |>
    tidytable::filter(
      structure_inchikey_connectivity_layer %in%
        keys$inchikey |
        structure_smiles_no_stereo %in% keys$smiles
    ) |>
    tidytable::distinct()
  log_debug("Stereo loaded: %d rows after filtering", nrow(stereo))

  if (
    nrow(stereo) > 0L &&
      (all(is.na(stereo$structure_inchikey_connectivity_layer)) ||
        all(is.na(stereo$structure_smiles_no_stereo)))
  ) {
    cli::cli_abort(
      paste(
        "stereochemistry data must contain",
        "structure_inchikey_connectivity_layer and structure_smiles_no_stereo"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  stereo_k <- stereo |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_connectivity_layer,
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo
    ) |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::distinct(structure_inchikey, .keep_all = TRUE)

  keys$inchikey <- unique(c(
    keys$inchikey,
    stats::na.omit(stereo$structure_inchikey_connectivity_layer)
  ))
  keys$smiles <- unique(c(
    keys$smiles,
    stats::na.omit(stereo$structure_smiles_no_stereo)
  ))
  keys$inchikey_full <- unique(stats::na.omit(stereo$structure_inchikey))
  keys$inchikey_no_stereo <- unique(stats::na.omit(
    stereo$structure_inchikey_no_stereo
  ))
  keys$smiles_stereo <- unique(stats::na.omit(stereo$structure_smiles))

  stereo_s <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer_s = structure_inchikey_connectivity_layer,
      candidate_structure_inchikey_no_stereo_s = structure_inchikey_no_stereo,
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  # Fallback: stereo_i_conn for cases where inchikey_no_stereo is unavailable
  # (e.g., SIRIUS annotations). Keyed by connectivity_layer only.
  stereo_i_conn <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo_ic = structure_smiles_no_stereo,
      candidate_structure_inchikey_no_stereo_ic = structure_inchikey_no_stereo
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )

  # Load metadata (formula + mass only, keyed by inchikey_no_stereo)
  met_cols <- .schema_metadata_cols()
  if (
    !"structure_inchikey_no_stereo" %in%
      attr(refs$met_full, "selected_cols")
  ) {
    log_warn(
      paste(
        "structure metadata has no structure keys;",
        "skipping key-based metadata enrichment"
      )
    )
  }

  met_2d <- refs$met_full |>
    .ensure_columns(cols = met_cols) |>
    tidytable::filter(
      structure_inchikey_no_stereo %in%
        keys$inchikey_no_stereo
    ) |>
    tidytable::distinct(
      structure_inchikey_no_stereo,
      structure_exact_mass,
      structure_molecular_formula
    ) |>
    tidytable::filter(!is.na(structure_inchikey_no_stereo))

  # Build name/tag/xlogp lookup from stereo (keyed by inchikey_no_stereo,
  # collapsing across stereoisomers).
  # Uses singleton/multi-row split: singletons (~99% of groups) pass through
  # directly; only multi-row groups need R-level collapse.
  nam_base <- stereo |>
    tidytable::filter(!is.na(structure_inchikey_no_stereo)) |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      structure_name,
      structure_tag,
      structure_xlogp
    ) |>
    tidytable::mutate(
      .n_grp = .N,
      .by = candidate_structure_inchikey_no_stereo
    )

  # Singletons: pass through directly (most groups)
  singletons_nam <- nam_base |>
    tidytable::filter(.n_grp == 1L) |>
    tidytable::mutate(
      .lk_name = tidytable::if_else(
        is.na(structure_name) | trimws(structure_name) == "",
        NA_character_,
        trimws(structure_name)
      ),
      .lk_tag = tidytable::if_else(
        is.na(structure_tag) | trimws(structure_tag) == "",
        NA_character_,
        trimws(structure_tag)
      ),
      .lk_xlogp = tidytable::if_else(
        is.na(structure_xlogp) | trimws(structure_xlogp) == "",
        NA_character_,
        structure_xlogp
      )
    ) |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo,
      .lk_name,
      .lk_tag,
      .lk_xlogp
    )

  # Multi-row groups: apply R-level collapse
  multi_nam <- nam_base |>
    tidytable::filter(.n_grp > 1L) |>
    tidytable::select(-.n_grp)

  if (nrow(multi_nam) > 0L) {
    multi_nam <- multi_nam |>
      tidytable::summarize(
        .lk_name = .collapse_harmonized_names(structure_name),
        .lk_tag = .collapse_unique_non_empty(structure_tag),
        .lk_xlogp = .resolve_numeric_or_na(structure_xlogp),
        .by = candidate_structure_inchikey_no_stereo
      )
    nam_lookup <- tidytable::bind_rows(singletons_nam, multi_nam)
  } else {
    nam_lookup <- singletons_nam
  }
  rm(nam_base, singletons_nam, multi_nam)
  log_debug("Names/tag/xlogp lookup: %d unique keys", nrow(nam_lookup))

  # ClassyFire taxonomy — keyed by full inchikey (filter to batch keys)
  tax_cla <- refs$tax_cla_full |>
    tidytable::select(
      candidate_structure_inchikey = structure_inchikey,
      candidate_structure_tax_cla_chemontid_i = structure_tax_cla_chemontid,
      candidate_structure_tax_cla_01kin_i = structure_tax_cla_01kin,
      candidate_structure_tax_cla_02sup_i = structure_tax_cla_02sup,
      candidate_structure_tax_cla_03cla_i = structure_tax_cla_03cla,
      candidate_structure_tax_cla_04dirpar_i = structure_tax_cla_04dirpar
    ) |>
    tidytable::filter(
      candidate_structure_inchikey %in% keys$inchikey_full
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey,
      .keep_all = TRUE
    )
  log_debug("ClassyFire taxonomy: %d rows", nrow(tax_cla))

  # NPClassifier taxonomy — keyed by canonical SMILES with stereo
  tax_npc <- refs$tax_npc_full |>
    tidytable::select(
      candidate_structure_smiles = structure_smiles,
      candidate_structure_tax_npc_01pat_s = structure_tax_npc_01pat,
      candidate_structure_tax_npc_02sup_s = structure_tax_npc_02sup,
      candidate_structure_tax_npc_03cla_s = structure_tax_npc_03cla
    ) |>
    tidytable::filter(
      candidate_structure_smiles %in% keys$smiles_stereo
    ) |>
    tidytable::distinct(
      candidate_structure_smiles,
      .keep_all = TRUE
    )
  log_debug("NPClassifier taxonomy: %d rows", nrow(tax_npc))

  # Build metadata lookup keyed by inchikey_no_stereo.
  # Formula and mass are deterministic from SMILES, so distinct() suffices.
  met_lookup <- met_2d |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      .lk_molecular_formula = structure_molecular_formula,
      .lk_exact_mass = structure_exact_mass
    ) |>
    tidytable::filter(!is.na(candidate_structure_inchikey_no_stereo)) |>
    tidytable::distinct(
      candidate_structure_inchikey_no_stereo,
      .keep_all = TRUE
    )
  rm(met_2d)
  log_debug("Metadata lookup: %d unique keys", nrow(met_lookup))

  # Inject placeholder columns early if missing in input df.
  df <- .ensure_structure_placeholders(df)
  log_debug("Starting key resolution joins on %d rows", nrow(df))

  # Derive candidate_structure_inchikey_no_stereo in df.
  # All structural identifiers (SMILES, InChIKey variants) are strictly
  # taken from the stereo reference file when available, ensuring
  # consistency with process_smiles() output.
  df <- .resolve_structure_identifiers(
    df = df,
    stereo_i_conn = stereo_i_conn,
    stereo_s = stereo_s
  )

  # Build the bridge from annotations (which have inchikey_no_stereo or
  # connectivity_layer) to the full inchikey needed for tax_cla and
  # the stereo smiles needed for tax_npc.
  # stereo_k provides: inchikey ->
  #   (smiles, connectivity_layer, inchikey_no_stereo,
  #    smiles_no_stereo)

  # Build unique key combinations for enrichment lookup.
  # Use only (inchikey_no_stereo, connectivity_layer) — NOT smiles_no_stereo.
  # Including smiles_no_stereo would fragment the lookup when tautomers
  # produce different SMILES for the same InChIKey (e.g., RWQNBRDOKXIBIV
  # glutamine amide vs enol form), leading to missed enrichments.
  structure_keys <- .build_structure_keys(df)
  log_debug("Structure keys: %d unique combinations", nrow(structure_keys))

  stereo_bridge <- .build_stereo_bridge(stereo_k)

  key_lookup <- .build_structure_key_lookup(
    structure_keys = structure_keys,
    met_lookup = met_lookup,
    nam_lookup = nam_lookup,
    stereo_bridge = stereo_bridge,
    tax_cla = tax_cla,
    tax_npc = tax_npc
  )

  log_debug("Key lookup: %d rows; starting final join", nrow(key_lookup))
  table_final <- .apply_structure_enrichment(df = df, key_lookup = key_lookup)
  rm(
    structure_keys,
    met_lookup,
    nam_lookup,
    key_lookup,
    stereo_k,
    stereo_i_conn,
    stereo_s,
    stereo_bridge,
    tax_cla,
    tax_npc
  )

  log_complete(ctx, n_enriched = nrow(table_final))

  table_final
}

.extract_structure_keys <- function(df) {
  inchikey_cols <- c(
    "candidate_structure_inchikey_connectivity_layer",
    "structure_inchikey_connectivity_layer"
  )
  smiles_cols <- c(
    "candidate_structure_smiles_no_stereo",
    "structure_smiles_no_stereo"
  )

  present_inchikey_cols <- intersect(inchikey_cols, names(df))
  present_smiles_cols <- intersect(smiles_cols, names(df))

  inchikey <- if (length(present_inchikey_cols) == 0L) {
    character()
  } else {
    unique(stats::na.omit(unlist(
      tidytable::select(df, tidyselect::all_of(present_inchikey_cols)),
      use.names = FALSE
    )))
  }

  smiles <- if (length(present_smiles_cols) == 0L) {
    character()
  } else {
    unique(stats::na.omit(unlist(
      tidytable::select(df, tidyselect::all_of(present_smiles_cols)),
      use.names = FALSE
    )))
  }

  list(inchikey = inchikey, smiles = smiles)
}

.resolve_structure_identifiers <- function(df, stereo_i_conn, stereo_s) {
  if (
    "candidate_structure_inchikey_connectivity_layer" %in%
      names(df) &&
      nrow(stereo_i_conn) > 0L
  ) {
    df <- df |>
      tidytable::left_join(
        y = stereo_i_conn,
        by = "candidate_structure_inchikey_connectivity_layer"
      ) |>
      tidytable::mutate(
        candidate_structure_smiles_no_stereo = tidytable::coalesce(
          candidate_structure_smiles_no_stereo_ic,
          candidate_structure_smiles_no_stereo
        ),
        candidate_structure_inchikey_no_stereo = tidytable::coalesce(
          candidate_structure_inchikey_no_stereo_ic,
          candidate_structure_inchikey_no_stereo
        )
      ) |>
      tidytable::select(
        -candidate_structure_smiles_no_stereo_ic,
        -candidate_structure_inchikey_no_stereo_ic
      )
  }

  if (
    "candidate_structure_smiles_no_stereo" %in% names(df) && nrow(stereo_s) > 0L
  ) {
    df <- df |>
      tidytable::left_join(
        y = stereo_s,
        by = "candidate_structure_smiles_no_stereo"
      ) |>
      tidytable::mutate(
        candidate_structure_inchikey_connectivity_layer = tidytable::coalesce(
          candidate_structure_inchikey_connectivity_layer_s,
          candidate_structure_inchikey_connectivity_layer
        ),
        candidate_structure_inchikey_no_stereo = tidytable::coalesce(
          candidate_structure_inchikey_no_stereo_s,
          candidate_structure_inchikey_no_stereo
        )
      ) |>
      tidytable::select(
        -candidate_structure_inchikey_connectivity_layer_s,
        -candidate_structure_inchikey_no_stereo_s
      )
  }

  df
}

.build_structure_keys <- function(df) {
  df |>
    tidytable::distinct(
      candidate_structure_inchikey_no_stereo,
      candidate_structure_inchikey_connectivity_layer
    )
}

.build_stereo_bridge <- function(stereo_k) {
  stereo_k |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      .bridge_inchikey = structure_inchikey,
      .bridge_smiles = structure_smiles
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )
}

.build_structure_key_lookup <- function(
  structure_keys,
  met_lookup,
  nam_lookup,
  stereo_bridge,
  tax_cla,
  tax_npc
) {
  structure_keys |>
    tidytable::left_join(
      y = met_lookup,
      by = "candidate_structure_inchikey_no_stereo"
    ) |>
    tidytable::left_join(
      y = nam_lookup,
      by = "candidate_structure_inchikey_no_stereo"
    ) |>
    tidytable::left_join(
      y = stereo_bridge,
      by = "candidate_structure_inchikey_connectivity_layer"
    ) |>
    tidytable::left_join(
      y = tax_cla,
      by = c(".bridge_inchikey" = "candidate_structure_inchikey")
    ) |>
    tidytable::left_join(
      y = tax_npc,
      by = c(".bridge_smiles" = "candidate_structure_smiles")
    ) |>
    tidytable::select(-.bridge_inchikey, -.bridge_smiles) |>
    tidytable::rename(
      .enr_candidate_structure_molecular_formula = .lk_molecular_formula,
      .enr_candidate_structure_exact_mass = .lk_exact_mass,
      .enr_candidate_structure_tag = .lk_tag,
      .enr_candidate_structure_name = .lk_name,
      .enr_candidate_structure_xlogp = .lk_xlogp,
      .enr_candidate_structure_tax_npc_01pat = candidate_structure_tax_npc_01pat_s,
      .enr_candidate_structure_tax_npc_02sup = candidate_structure_tax_npc_02sup_s,
      .enr_candidate_structure_tax_npc_03cla = candidate_structure_tax_npc_03cla_s,
      .enr_candidate_structure_tax_cla_chemontid = candidate_structure_tax_cla_chemontid_i,
      .enr_candidate_structure_tax_cla_01kin = candidate_structure_tax_cla_01kin_i,
      .enr_candidate_structure_tax_cla_02sup = candidate_structure_tax_cla_02sup_i,
      .enr_candidate_structure_tax_cla_03cla = candidate_structure_tax_cla_03cla_i,
      .enr_candidate_structure_tax_cla_04dirpar = candidate_structure_tax_cla_04dirpar_i
    )
}

.apply_structure_enrichment <- function(df, key_lookup) {
  df |>
    tidytable::left_join(
      y = key_lookup,
      by = c(
        "candidate_structure_inchikey_no_stereo",
        "candidate_structure_inchikey_connectivity_layer"
      )
    ) |>
    tidytable::mutate(
      candidate_structure_molecular_formula = .pick_enrichment(
        .enr_candidate_structure_molecular_formula,
        candidate_structure_molecular_formula
      ),
      candidate_structure_exact_mass = .pick_enrichment(
        .enr_candidate_structure_exact_mass,
        candidate_structure_exact_mass
      ),
      candidate_structure_xlogp = .pick_enrichment(
        .enr_candidate_structure_xlogp,
        candidate_structure_xlogp
      ),
      candidate_structure_tag = .pick_enrichment(
        .enr_candidate_structure_tag,
        candidate_structure_tag
      ),
      candidate_structure_name = .pick_enrichment(
        .enr_candidate_structure_name,
        candidate_structure_name
      ),
      candidate_structure_tax_npc_01pat = .pick_enrichment(
        .enr_candidate_structure_tax_npc_01pat,
        candidate_structure_tax_npc_01pat
      ),
      candidate_structure_tax_npc_02sup = .pick_enrichment(
        .enr_candidate_structure_tax_npc_02sup,
        candidate_structure_tax_npc_02sup
      ),
      candidate_structure_tax_npc_03cla = .pick_enrichment(
        .enr_candidate_structure_tax_npc_03cla,
        candidate_structure_tax_npc_03cla
      ),
      candidate_structure_tax_cla_chemontid = .pick_enrichment(
        .enr_candidate_structure_tax_cla_chemontid,
        candidate_structure_tax_cla_chemontid
      ),
      candidate_structure_tax_cla_01kin = .pick_enrichment(
        .enr_candidate_structure_tax_cla_01kin,
        candidate_structure_tax_cla_01kin
      ),
      candidate_structure_tax_cla_02sup = .pick_enrichment(
        .enr_candidate_structure_tax_cla_02sup,
        candidate_structure_tax_cla_02sup
      ),
      candidate_structure_tax_cla_03cla = .pick_enrichment(
        .enr_candidate_structure_tax_cla_03cla,
        candidate_structure_tax_cla_03cla
      ),
      candidate_structure_tax_cla_04dirpar = .pick_enrichment(
        .enr_candidate_structure_tax_cla_04dirpar,
        candidate_structure_tax_cla_04dirpar
      )
    ) |>
    tidytable::select(-tidyselect::starts_with(".enr_"))
}

.ensure_structure_placeholders <- function(df) {
  placeholder_candidate_cols <- .schema_candidate_structure_placeholders()
  placeholder_structure_cols <- .schema_structure_placeholders()

  needed <- setdiff(
    c(placeholder_candidate_cols, placeholder_structure_cols),
    names(df)
  )
  if (length(needed) == 0L) {
    return(df)
  }

  df |>
    tidytable::mutate(
      !!!stats::setNames(
        rep(list(NA_character_), length(needed)),
        needed
      )
    )
}

.schema_stereo_cols <- function() {
  c(
    "structure_inchikey",
    "structure_smiles",
    "structure_inchikey_connectivity_layer",
    "structure_inchikey_no_stereo",
    "structure_smiles_no_stereo",
    "structure_xlogp",
    "structure_name",
    "structure_tag"
  )
}

.schema_metadata_cols <- function() {
  c(
    "structure_inchikey_no_stereo",
    "structure_exact_mass",
    "structure_molecular_formula"
  )
}

.schema_classyfire_cols <- function() {
  c(
    "structure_inchikey",
    "structure_tax_cla_chemontid",
    "structure_tax_cla_01kin",
    "structure_tax_cla_02sup",
    "structure_tax_cla_03cla",
    "structure_tax_cla_04dirpar"
  )
}

.schema_npc_cols <- function() {
  c(
    "structure_smiles",
    "structure_tax_npc_01pat",
    "structure_tax_npc_02sup",
    "structure_tax_npc_03cla"
  )
}

.schema_candidate_structure_placeholders <- function() {
  c(
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp",
    "candidate_structure_tag",
    "candidate_structure_name",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar"
  )
}

.schema_structure_placeholders <- function() {
  c(
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_tag",
    "structure_name",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo"
  )
}

.safe_fread_selected <- function(file, file_type, selected_cols) {
  header <- names(tidytable::fread(
    file = file,
    nrows = 0L,
    showProgress = FALSE
  ))
  selected <- intersect(selected_cols, header)

  if (length(selected) == 0L) {
    out <- tidytable::tidytable()
    attr(out, "selected_cols") <- character()
    return(out)
  }

  out <- safe_fread(
    file = file,
    file_type = file_type,
    na.strings = c("", "NA"),
    colClasses = "character",
    select = selected
  )
  attr(out, "selected_cols") <- selected
  out
}

.ensure_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) == 0L) {
    return(df)
  }

  df |>
    tidytable::mutate(
      !!!stats::setNames(
        rep(list(NA_character_), length(missing_cols)),
        missing_cols
      )
    )
}


.resolve_numeric_or_na <- function(x, tolerance = 1e-8) {
  nums <- stats::na.omit(as.numeric(x))
  nums <- nums[is.finite(nums)]

  if (length(nums) == 0L) {
    return(NA_character_)
  }

  if ((max(nums) - min(nums)) <= tolerance) {
    return(as.character(nums[[1L]]))
  }

  # Conflicting numeric values for same key: avoid arbitrary assignment.
  NA_character_
}

.collapse_unique_non_empty <- function(x, sep = " $ ") {
  vals <- trimws(as.character(x))
  vals[vals == ""] <- NA_character_
  vals <- vals[!is.na(vals)]
  vals <- unique(vals)

  if (length(vals) == 0L) {
    return(NA_character_)
  }

  paste(vals, collapse = sep)
}

.collapse_harmonized_names <- function(x, sep = " $ ") {
  vals <- trimws(as.character(x))
  vals[vals == ""] <- NA_character_
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0L) {
    return(NA_character_)
  }

  keys <- tolower(vals)
  vals <- vals[!duplicated(keys)]

  paste(vals, collapse = sep)
}


.normalize_enrichment_text <- function(x) {
  vals <- as.character(x)
  trimmed <- trimws(vals)
  low <- tolower(trimmed)
  invalid <- is.na(vals) |
    trimmed == "" |
    low %in% c("na", "n/a", "null", "notclassified", "empty")
  vals[invalid] <- NA_character_
  vals
}

.pick_enrichment <- function(enriched, existing) {
  tidytable::coalesce(
    .normalize_enrichment_text(enriched),
    .normalize_enrichment_text(existing)
  )
}
