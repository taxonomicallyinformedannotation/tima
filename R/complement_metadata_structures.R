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

  met_lookup <- refs$met_lookup_full |>
    tidytable::filter(
      candidate_structure_inchikey_no_stereo %in% keys$inchikey_no_stereo
    )

  # Name/tag/xlogp collapse is precomputed once in the reference cache.
  nam_lookup <- refs$nam_lookup_full |>
    tidytable::filter(
      candidate_structure_inchikey_no_stereo %in% keys$inchikey_no_stereo
    )
  log_debug("Names/tag/xlogp lookup: %d unique keys", nrow(nam_lookup))

  # ClassyFire taxonomy — keyed by full inchikey (filter to batch keys)
  tax_cla <- refs$tax_cla_lookup_full |>
    tidytable::filter(
      candidate_structure_inchikey %in% keys$inchikey_full
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey,
      .keep_all = TRUE
    )
  log_debug("ClassyFire taxonomy: %d rows", nrow(tax_cla))

  # NPClassifier taxonomy — keyed by canonical SMILES with stereo
  tax_npc <- refs$tax_npc_lookup_full |>
    tidytable::filter(
      candidate_structure_smiles %in% keys$smiles_stereo
    ) |>
    tidytable::distinct(
      candidate_structure_smiles,
      .keep_all = TRUE
    )
  log_debug("NPClassifier taxonomy: %d rows", nrow(tax_npc))

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

  key_lookup <- .build_structure_key_lookup_fast(
    structure_keys = structure_keys,
    met_lookup = met_lookup,
    nam_lookup = nam_lookup,
    stereo_bridge = stereo_bridge,
    tax_cla = tax_cla,
    tax_npc = tax_npc
  )

  log_debug("Key lookup: %d rows; starting final join", nrow(key_lookup))
  table_final <- .apply_structure_enrichment_fast(
    df = df,
    key_lookup = key_lookup
  )
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

.complement_metadata_ref_cache <- local({
  .env <- new.env(hash = TRUE, parent = emptyenv())

  load_refs <- function(str_stereo, str_met, str_tax_cla, str_tax_npc) {
    # Include file modification time so cache invalidates when any reference file is replaced.
    mtime_key <- paste(
      vapply(
        c(
          str_stereo[[1L]],
          str_met[[1L]],
          str_tax_cla[[1L]],
          str_tax_npc[[1L]]
        ),
        function(f) {
          mt <- tryCatch(
            as.character(file.info(f)$mtime),
            error = function(.e) "?"
          )
          mt %||% "?"
        },
        character(1L)
      ),
      collapse = "|"
    )
    cache_key <- paste(
      str_stereo,
      str_met,
      str_tax_cla,
      str_tax_npc,
      mtime_key,
      sep = "|"
    )
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

    # Precompute expensive per-reference lookups once and memoize them.
    met_lookup_full <- met_full |>
      .ensure_columns(cols = met_cols) |>
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

    nam_lookup_full <- .build_name_lookup_from_stereo(stereo_full)

    tax_cla_lookup_full <- tax_cla_full |>
      tidytable::select(
        candidate_structure_inchikey = structure_inchikey,
        candidate_structure_tax_cla_chemontid_i = structure_tax_cla_chemontid,
        candidate_structure_tax_cla_01kin_i = structure_tax_cla_01kin,
        candidate_structure_tax_cla_02sup_i = structure_tax_cla_02sup,
        candidate_structure_tax_cla_03cla_i = structure_tax_cla_03cla,
        candidate_structure_tax_cla_04dirpar_i = structure_tax_cla_04dirpar
      ) |>
      tidytable::distinct(candidate_structure_inchikey, .keep_all = TRUE)

    tax_npc_lookup_full <- tax_npc_full |>
      tidytable::select(
        candidate_structure_smiles = structure_smiles,
        candidate_structure_tax_npc_01pat_s = structure_tax_npc_01pat,
        candidate_structure_tax_npc_02sup_s = structure_tax_npc_02sup,
        candidate_structure_tax_npc_03cla_s = structure_tax_npc_03cla
      ) |>
      tidytable::distinct(candidate_structure_smiles, .keep_all = TRUE)

    # Validate stereo completeness at load time before caching.
    if (
      nrow(stereo_full) > 0L &&
        (!"structure_smiles_no_stereo" %in% colnames(stereo_full) ||
          all(is.na(stereo_full$structure_smiles_no_stereo)))
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

    result <- list(
      stereo_full = stereo_full,
      met_full = met_full,
      tax_cla_full = tax_cla_full,
      tax_npc_full = tax_npc_full,
      met_lookup_full = met_lookup_full,
      nam_lookup_full = nam_lookup_full,
      tax_cla_lookup_full = tax_cla_lookup_full,
      tax_npc_lookup_full = tax_npc_lookup_full
    )
    assign(cache_key, result, envir = .env)
    result
  }

  list(load_refs = load_refs)
})

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
  if (nrow(df) == 0L) {
    return(tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = character(0),
      candidate_structure_inchikey_connectivity_layer = character(0)
    ))
  }

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

.make_structure_lookup_key <- function(no_stereo, connectivity_layer) {
  no_stereo <- as.character(no_stereo)
  connectivity_layer <- as.character(connectivity_layer)
  key_sep <- "\u001f"

  no_stereo[
    is.na(no_stereo) | !nzchar(no_stereo) | no_stereo %in% c("NA", "null")
  ] <- NA_character_
  connectivity_layer[
    is.na(connectivity_layer) |
      !nzchar(connectivity_layer) |
      connectivity_layer %in% c("NA", "null")
  ] <- NA_character_

  ifelse(
    is.na(no_stereo) | is.na(connectivity_layer),
    NA_character_,
    paste0(no_stereo, key_sep, connectivity_layer)
  )
}

.build_structure_key_lookup_fast <- function(
  structure_keys,
  met_lookup,
  nam_lookup,
  stereo_bridge,
  tax_cla,
  tax_npc
) {
  if (nrow(structure_keys) == 0L) {
    return(tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = character(0),
      candidate_structure_inchikey_connectivity_layer = character(0),
      .structure_key = character(0)
    ))
  }

  lookup_tbl <- structure_keys |>
    tidytable::mutate(
      .structure_key = .make_structure_lookup_key(
        candidate_structure_inchikey_no_stereo,
        candidate_structure_inchikey_connectivity_layer
      )
    )

  if (nrow(met_lookup) > 0L) {
    met_map_formula <- stats::setNames(
      met_lookup$.lk_molecular_formula,
      met_lookup$candidate_structure_inchikey_no_stereo
    )
    met_map_mass <- stats::setNames(
      met_lookup$.lk_exact_mass,
      met_lookup$candidate_structure_inchikey_no_stereo
    )
    lookup_tbl$.enr_candidate_structure_molecular_formula <- met_map_formula[
      match(
        structure_keys$candidate_structure_inchikey_no_stereo,
        names(met_map_formula)
      )
    ]
    lookup_tbl$.enr_candidate_structure_exact_mass <- met_map_mass[
      match(
        structure_keys$candidate_structure_inchikey_no_stereo,
        names(met_map_mass)
      )
    ]
  } else {
    lookup_tbl$.enr_candidate_structure_molecular_formula <- NA_character_
    lookup_tbl$.enr_candidate_structure_exact_mass <- NA_character_
  }

  if (nrow(nam_lookup) > 0L) {
    nam_map_name <- stats::setNames(
      nam_lookup$.lk_name,
      nam_lookup$candidate_structure_inchikey_no_stereo
    )
    nam_map_tag <- stats::setNames(
      nam_lookup$.lk_tag,
      nam_lookup$candidate_structure_inchikey_no_stereo
    )
    nam_map_xlogp <- stats::setNames(
      nam_lookup$.lk_xlogp,
      nam_lookup$candidate_structure_inchikey_no_stereo
    )
    lookup_tbl$.enr_candidate_structure_tag <- nam_map_tag[
      match(
        structure_keys$candidate_structure_inchikey_no_stereo,
        names(nam_map_tag)
      )
    ]
    lookup_tbl$.enr_candidate_structure_name <- nam_map_name[
      match(
        structure_keys$candidate_structure_inchikey_no_stereo,
        names(nam_map_name)
      )
    ]
    lookup_tbl$.enr_candidate_structure_xlogp <- nam_map_xlogp[
      match(
        structure_keys$candidate_structure_inchikey_no_stereo,
        names(nam_map_xlogp)
      )
    ]
  } else {
    lookup_tbl$.enr_candidate_structure_tag <- NA_character_
    lookup_tbl$.enr_candidate_structure_name <- NA_character_
    lookup_tbl$.enr_candidate_structure_xlogp <- NA_character_
  }

  if (nrow(stereo_bridge) > 0L) {
    bridge_inchikey_map <- stats::setNames(
      stereo_bridge$.bridge_inchikey,
      stereo_bridge$candidate_structure_inchikey_connectivity_layer
    )
    bridge_smiles_map <- stats::setNames(
      stereo_bridge$.bridge_smiles,
      stereo_bridge$candidate_structure_inchikey_connectivity_layer
    )
    lookup_tbl$.bridge_inchikey <- bridge_inchikey_map[
      match(
        structure_keys$candidate_structure_inchikey_connectivity_layer,
        names(bridge_inchikey_map)
      )
    ]
    lookup_tbl$.bridge_smiles <- bridge_smiles_map[
      match(
        structure_keys$candidate_structure_inchikey_connectivity_layer,
        names(bridge_smiles_map)
      )
    ]
  } else {
    lookup_tbl$.bridge_inchikey <- NA_character_
    lookup_tbl$.bridge_smiles <- NA_character_
  }

  if (
    nrow(tax_cla) > 0L && "candidate_structure_inchikey" %in% names(tax_cla)
  ) {
    tax_cla_map_chemontid <- stats::setNames(
      tax_cla$candidate_structure_tax_cla_chemontid_i,
      tax_cla$candidate_structure_inchikey
    )
    tax_cla_map_01kin <- stats::setNames(
      tax_cla$candidate_structure_tax_cla_01kin_i,
      tax_cla$candidate_structure_inchikey
    )
    tax_cla_map_02sup <- stats::setNames(
      tax_cla$candidate_structure_tax_cla_02sup_i,
      tax_cla$candidate_structure_inchikey
    )
    tax_cla_map_03cla <- stats::setNames(
      tax_cla$candidate_structure_tax_cla_03cla_i,
      tax_cla$candidate_structure_inchikey
    )
    tax_cla_map_04dirpar <- stats::setNames(
      tax_cla$candidate_structure_tax_cla_04dirpar_i,
      tax_cla$candidate_structure_inchikey
    )
    lookup_tbl$.enr_candidate_structure_tax_cla_chemontid <- tax_cla_map_chemontid[
      match(lookup_tbl$.bridge_inchikey, names(tax_cla_map_chemontid))
    ]
    lookup_tbl$.enr_candidate_structure_tax_cla_01kin <- tax_cla_map_01kin[
      match(lookup_tbl$.bridge_inchikey, names(tax_cla_map_01kin))
    ]
    lookup_tbl$.enr_candidate_structure_tax_cla_02sup <- tax_cla_map_02sup[
      match(lookup_tbl$.bridge_inchikey, names(tax_cla_map_02sup))
    ]
    lookup_tbl$.enr_candidate_structure_tax_cla_03cla <- tax_cla_map_03cla[
      match(lookup_tbl$.bridge_inchikey, names(tax_cla_map_03cla))
    ]
    lookup_tbl$.enr_candidate_structure_tax_cla_04dirpar <- tax_cla_map_04dirpar[
      match(lookup_tbl$.bridge_inchikey, names(tax_cla_map_04dirpar))
    ]
  } else {
    lookup_tbl$.enr_candidate_structure_tax_cla_chemontid <- NA_character_
    lookup_tbl$.enr_candidate_structure_tax_cla_01kin <- NA_character_
    lookup_tbl$.enr_candidate_structure_tax_cla_02sup <- NA_character_
    lookup_tbl$.enr_candidate_structure_tax_cla_03cla <- NA_character_
    lookup_tbl$.enr_candidate_structure_tax_cla_04dirpar <- NA_character_
  }

  if (nrow(tax_npc) > 0L && "candidate_structure_smiles" %in% names(tax_npc)) {
    tax_npc_map_01pat <- stats::setNames(
      tax_npc$candidate_structure_tax_npc_01pat_s,
      tax_npc$candidate_structure_smiles
    )
    tax_npc_map_02sup <- stats::setNames(
      tax_npc$candidate_structure_tax_npc_02sup_s,
      tax_npc$candidate_structure_smiles
    )
    tax_npc_map_03cla <- stats::setNames(
      tax_npc$candidate_structure_tax_npc_03cla_s,
      tax_npc$candidate_structure_smiles
    )
    lookup_tbl$.enr_candidate_structure_tax_npc_01pat <- tax_npc_map_01pat[
      match(lookup_tbl$.bridge_smiles, names(tax_npc_map_01pat))
    ]
    lookup_tbl$.enr_candidate_structure_tax_npc_02sup <- tax_npc_map_02sup[
      match(lookup_tbl$.bridge_smiles, names(tax_npc_map_02sup))
    ]
    lookup_tbl$.enr_candidate_structure_tax_npc_03cla <- tax_npc_map_03cla[
      match(lookup_tbl$.bridge_smiles, names(tax_npc_map_03cla))
    ]
  } else {
    lookup_tbl$.enr_candidate_structure_tax_npc_01pat <- NA_character_
    lookup_tbl$.enr_candidate_structure_tax_npc_02sup <- NA_character_
    lookup_tbl$.enr_candidate_structure_tax_npc_03cla <- NA_character_
  }

  lookup_tbl
}

.apply_structure_enrichment_fast <- function(df, key_lookup) {
  if (nrow(df) == 0L) {
    return(df)
  }

  row_keys <- .make_structure_lookup_key(
    df$candidate_structure_inchikey_no_stereo,
    df$candidate_structure_inchikey_connectivity_layer
  )
  idx <- match(row_keys, key_lookup$.structure_key)

  out <- df
  enrichment_map <- list(
    candidate_structure_molecular_formula = c(
      ".enr_candidate_structure_molecular_formula",
      "candidate_structure_molecular_formula"
    ),
    candidate_structure_exact_mass = c(
      ".enr_candidate_structure_exact_mass",
      "candidate_structure_exact_mass"
    ),
    candidate_structure_xlogp = c(
      ".enr_candidate_structure_xlogp",
      "candidate_structure_xlogp"
    ),
    candidate_structure_tag = c(
      ".enr_candidate_structure_tag",
      "candidate_structure_tag"
    ),
    candidate_structure_name = c(
      ".enr_candidate_structure_name",
      "candidate_structure_name"
    ),
    candidate_structure_tax_npc_01pat = c(
      ".enr_candidate_structure_tax_npc_01pat",
      "candidate_structure_tax_npc_01pat"
    ),
    candidate_structure_tax_npc_02sup = c(
      ".enr_candidate_structure_tax_npc_02sup",
      "candidate_structure_tax_npc_02sup"
    ),
    candidate_structure_tax_npc_03cla = c(
      ".enr_candidate_structure_tax_npc_03cla",
      "candidate_structure_tax_npc_03cla"
    ),
    candidate_structure_tax_cla_chemontid = c(
      ".enr_candidate_structure_tax_cla_chemontid",
      "candidate_structure_tax_cla_chemontid"
    ),
    candidate_structure_tax_cla_01kin = c(
      ".enr_candidate_structure_tax_cla_01kin",
      "candidate_structure_tax_cla_01kin"
    ),
    candidate_structure_tax_cla_02sup = c(
      ".enr_candidate_structure_tax_cla_02sup",
      "candidate_structure_tax_cla_02sup"
    ),
    candidate_structure_tax_cla_03cla = c(
      ".enr_candidate_structure_tax_cla_03cla",
      "candidate_structure_tax_cla_03cla"
    ),
    candidate_structure_tax_cla_04dirpar = c(
      ".enr_candidate_structure_tax_cla_04dirpar",
      "candidate_structure_tax_cla_04dirpar"
    )
  )

  for (target_col in names(enrichment_map)) {
    enrich_col <- enrichment_map[[target_col]][[1L]]
    existing_col <- enrichment_map[[target_col]][[2L]]
    out[[target_col]] <- .pick_enrichment(
      key_lookup[[enrich_col]][idx],
      out[[existing_col]]
    )
  }

  out
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

.build_name_lookup_from_stereo <- function(stereo) {
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

  multi_nam <- nam_base |>
    tidytable::filter(.n_grp > 1L) |>
    tidytable::select(-.n_grp)

  out <- if (nrow(multi_nam) > 0L) {
    multi_nam <- multi_nam |>
      tidytable::summarize(
        .lk_name = .collapse_harmonized_names(structure_name),
        .lk_tag = .collapse_unique_non_empty(structure_tag),
        .lk_xlogp = .resolve_numeric_or_na(structure_xlogp),
        .by = candidate_structure_inchikey_no_stereo
      )
    tidytable::bind_rows(singletons_nam, multi_nam)
  } else {
    singletons_nam
  }

  rm(nam_base, singletons_nam, multi_nam)
  out
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
