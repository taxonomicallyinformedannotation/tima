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
#' @param str_stereo Path to structure stereochemistry file (includes name, tag, xlogp)
#' @param str_met Path to structure metadata file (formula, mass)
#' @param str_tax_cla Path to ClassyFire taxonomy file (keyed by inchikey)
#' @param str_tax_npc Path to NPClassifier taxonomy file (keyed by smiles with stereo)
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

  # Load stereochemistry data (now includes name, tag, xlogp)
  stereo_cols <- c(
    "structure_inchikey",
    "structure_smiles",
    "structure_inchikey_connectivity_layer",
    "structure_inchikey_no_stereo",
    "structure_smiles_no_stereo",
    "structure_xlogp",
    "structure_name",
    "structure_tag"
  )
  stereo <- .safe_fread_selected(
    file = str_stereo,
    file_type = "stereochemistry data",
    selected_cols = stereo_cols
  ) |>
    .ensure_columns(cols = stereo_cols) |>
    tidytable::filter(
      structure_inchikey_connectivity_layer %in%
        keys$inchikey |
        structure_smiles_no_stereo %in% keys$smiles
    ) |>
    tidytable::distinct()
  log_debug("Stereo loaded: %d rows after filtering", nrow(stereo))

  # Derive structure_inchikey_no_stereo from full inchikey if not in file
  if (all(is.na(stereo$structure_inchikey_no_stereo)) && nrow(stereo) > 0L) {
    stereo <- stereo |>
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

  # stereo_i: keyed by inchikey_no_stereo (connectivity + protonation).
  # This is the correct granularity because different protonation states
  # (same connectivity layer, different last InChIKey letter) are chemically
  # distinct and have different canonical SMILES.
  stereo_i <- stereo |>
    tidytable::filter(!is.na(structure_inchikey_no_stereo)) |>
    tidytable::select(
      candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
      candidate_structure_inchikey_connectivity_layer_from_stereo = structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo_i = structure_smiles_no_stereo
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_no_stereo,
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
  # log_trace("Stereo loaded")

  # Load metadata (formula + mass only, keyed by inchikey_no_stereo)
  met_cols <- c(
    "structure_inchikey_no_stereo",
    "structure_exact_mass",
    "structure_molecular_formula"
  )
  met_2d <- .safe_fread_selected(
    file = str_met,
    file_type = "structure metadata",
    selected_cols = met_cols
  )
  if (
    !"structure_inchikey_no_stereo" %in%
      attr(met_2d, "selected_cols")
  ) {
    log_warn(
      "structure metadata has no structure keys; skipping key-based metadata enrichment"
    )
  }

  met_2d <- met_2d |>
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
  # log_trace("Metadata loaded")

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

  # ClassyFire taxonomy — keyed by full inchikey
  tax_cla <- safe_fread(
    file = str_tax_cla,
    file_type = "ClassyFire taxonomy",
    na.strings = c("", "NA"),
    colClasses = "character",
    select = c(
      "structure_inchikey",
      "structure_tax_cla_chemontid",
      "structure_tax_cla_01kin",
      "structure_tax_cla_02sup",
      "structure_tax_cla_03cla",
      "structure_tax_cla_04dirpar"
    )
  ) |>
    tidytable::mutate(
      structure_tax_cla_chemontid = normalize_chemontid(
        structure_tax_cla_chemontid
      )
    ) |>
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
  tax_npc <- safe_fread(
    file = str_tax_npc,
    file_type = "NPClassifier taxonomy",
    na.strings = c("", "NA"),
    colClasses = "character",
    select = c(
      "structure_smiles",
      "structure_tax_npc_01pat",
      "structure_tax_npc_02sup",
      "structure_tax_npc_03cla"
    )
  ) |>
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
        # Always use canonical SMILES from stereo reference
        candidate_structure_smiles_no_stereo = tidytable::coalesce(
          candidate_structure_smiles_no_stereo_ic,
          candidate_structure_smiles_no_stereo
        ),
        # Always use inchikey_no_stereo from stereo reference
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

  # Also try SMILES-based lookup for rows still missing inchikey_no_stereo
  if (
    "candidate_structure_smiles_no_stereo" %in% names(df) && nrow(stereo_s) > 0L
  ) {
    df <- df |>
      tidytable::left_join(
        y = stereo_s,
        by = "candidate_structure_smiles_no_stereo"
      ) |>
      tidytable::mutate(
        # Prefer reference InChIKey connectivity layer over source
        candidate_structure_inchikey_connectivity_layer = tidytable::coalesce(
          candidate_structure_inchikey_connectivity_layer_s,
          candidate_structure_inchikey_connectivity_layer
        ),
        # Prefer reference inchikey_no_stereo over source
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

  # Build the bridge from annotations (which have inchikey_no_stereo or
  # connectivity_layer) to the full inchikey needed for tax_cla and
  # the stereo smiles needed for tax_npc.
  # stereo_k provides: inchikey -> (smiles, connectivity_layer, inchikey_no_stereo, smiles_no_stereo)

  structure_keys <- df |>
    tidytable::distinct(
      candidate_structure_inchikey_no_stereo,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo
    )
  log_debug("Structure keys: %d unique combinations", nrow(structure_keys))

  # Join met_lookup by inchikey_no_stereo
  key_lookup <- structure_keys |>
    tidytable::left_join(
      y = met_lookup,
      by = "candidate_structure_inchikey_no_stereo"
    ) |>
    tidytable::left_join(
      y = nam_lookup,
      by = "candidate_structure_inchikey_no_stereo"
    )

  # For taxonomy joins, we need to bridge through stereo_k to get
  # full inchikey and stereo smiles from the annotation's coarser keys
  # Build a bridge: smiles_no_stereo -> (inchikey, smiles with stereo)
  # Use first match per smiles_no_stereo to avoid fan-out
  stereo_bridge <- stereo_k |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      .bridge_inchikey = structure_inchikey,
      .bridge_smiles = structure_smiles
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  key_lookup <- key_lookup |>
    tidytable::left_join(
      y = stereo_bridge,
      by = "candidate_structure_smiles_no_stereo"
    ) |>
    # Join ClassyFire by full inchikey (via bridge)
    tidytable::left_join(
      y = tax_cla,
      by = c(".bridge_inchikey" = "candidate_structure_inchikey")
    ) |>
    # Join NPClassifier by stereo smiles (via bridge)
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

  log_debug("Key lookup: %d rows; starting final join", nrow(key_lookup))
  table_final <- df |>
    tidytable::left_join(
      y = key_lookup,
      by = c(
        "candidate_structure_inchikey_no_stereo",
        "candidate_structure_inchikey_connectivity_layer",
        "candidate_structure_smiles_no_stereo"
      )
    ) |>
    tidytable::mutate(
      ## Computable properties: prefer enriched value from library reference
      ## files, fall back to values computed by recompute_structure_fields_from_smiles.
      candidate_structure_molecular_formula = .pick_enrichment(
        .enr_candidate_structure_molecular_formula,
        candidate_structure_molecular_formula
      ),
      candidate_structure_exact_mass = .pick_enrichment(
        .enr_candidate_structure_exact_mass,
        candidate_structure_exact_mass
      ),
      ## xlogp: enriched from str_stereo (stereo-sensitive, collapsed per inchikey_no_stereo)
      candidate_structure_xlogp = .pick_enrichment(
        .enr_candidate_structure_xlogp,
        candidate_structure_xlogp
      ),
      ## Non-computable properties: coalesce with existing
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
  rm(
    structure_keys,
    met_lookup,
    nam_lookup,
    key_lookup,
    stereo_k,
    stereo_i,
    stereo_i_conn,
    stereo_s,
    stereo_bridge,
    tax_cla,
    tax_npc
  )

  log_complete(ctx, n_enriched = nrow(table_final))

  return(table_final)
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

.ensure_structure_placeholders <- function(df) {
  placeholder_candidate_cols <- c(
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

  placeholder_structure_cols <- c(
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_tag",
    "structure_name",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo"
  )

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

.filter_rows_by_structure_keys <- function(df, keys) {
  has_inchikey <- "structure_inchikey_connectivity_layer" %in% names(df)
  has_smiles <- "structure_smiles_no_stereo" %in% names(df)

  if (!has_inchikey && !has_smiles) {
    return(df[0L, , drop = FALSE])
  }

  keep <- rep(FALSE, nrow(df))
  if (has_inchikey) {
    keep <- keep | df$structure_inchikey_connectivity_layer %in% keys$inchikey
  }
  if (has_smiles) {
    keep <- keep | df$structure_smiles_no_stereo %in% keys$smiles
  }

  df[keep, , drop = FALSE]
}

.resolve_single_or_na <- function(x) {
  vals <- unique(trimws(stats::na.omit(as.character(x))))
  vals <- vals[nzchar(vals)]

  if (length(vals) == 0L) {
    return(NA_character_)
  }
  if (length(vals) == 1L) {
    return(vals[[1L]])
  }

  # Conflicting values for same key: avoid returning arbitrary metadata.
  NA_character_
}

.resolve_numeric_or_na <- function(x, tolerance = 1e-8) {
  nums <- as.numeric(stats::na.omit(as.character(x)))
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

.resolve_xlogp <- function(x, tolerance = 1e-6) {
  nums <- as.numeric(stats::na.omit(as.character(x)))
  nums <- nums[is.finite(nums)]

  if (length(nums) == 0L) {
    return(NA_character_)
  }

  if ((max(nums) - min(nums)) <= tolerance) {
    return(as.character(nums[[1L]]))
  }

  # Conservative default for conflicting xlogp values.
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

.first_non_empty <- function(x) {
  vals <- trimws(as.character(x))
  vals[vals == ""] <- NA_character_
  idx <- which(!is.na(vals))

  if (length(idx) == 0L) {
    return(NA_character_)
  }

  vals[idx[1L]]
}

.prefer_library_value <- function(existing, primary, secondary, tertiary) {
  # Library lookups always take precedence when present.
  lib_value <- .first_non_empty(c(primary, secondary, tertiary))
  if (!is.na(lib_value)) {
    return(lib_value)
  }

  # Fall back to existing candidate value only if lookups are empty.
  .first_non_empty(existing)
}

.normalize_structure_name <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  x_chr
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
