#' @title Complement structural metadata
#'
#' @description Complements structural metadata by joining stereochemistry,
#'     metadata, names, and chemical taxonomy from reference libraries.
#'     Enriches annotation results with comprehensive structure information.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#' @include safe_fread.R
#'
#' @param df Data frame with structural metadata to complement
#' @param str_stereo Path to structure stereochemistry file
#' @param str_met Path to structure metadata file
#' @param str_nam Path to structure names file
#' @param str_tax_cla Path to ClassyFire taxonomy file
#' @param str_tax_npc Path to NPClassifier taxonomy file
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
#'   str_nam = "data/str_names.tsv",
#'   str_tax_cla = "data/str_tax_classyfire.tsv",
#'   str_tax_npc = "data/str_tax_npclassifier.tsv"
#' )
#' }
complement_metadata_structures <- function(
  df,
  str_stereo,
  str_met,
  str_nam,
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
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  log_debug("Complementing metadata for %d rows", nrow(df))

  keys <- .extract_structure_keys(df)
  if (length(keys$inchikey) == 0L && length(keys$smiles) == 0L) {
    log_debug("No structure keys found in input; returning input unchanged")
    return(.ensure_structure_placeholders(df))
  }

  # Load stereochemistry data
  stereo_cols <- c(
    "structure_inchikey",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo"
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
    tidytable::distinct(
      structure_inchikey,
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo
    )

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
      structure_inchikey_connectivity_layer,
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

  stereo_s <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer_s = structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo
    ) |>
    tidytable::filter(!is.na(candidate_structure_smiles_no_stereo)) |>
    tidytable::distinct(
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  stereo_i <- stereo |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo_i = structure_smiles_no_stereo
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )
  # log_trace("Stereo loaded")

  met_cols <- c(
    "structure_inchikey",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_tag",
    "structure_molecular_formula"
  )
  met_2d <- .safe_fread_selected(
    file = str_met,
    file_type = "structure metadata",
    selected_cols = met_cols
  )
  if (
    !any(
      c(
        "structure_inchikey",
        "structure_inchikey_connectivity_layer",
        "structure_smiles_no_stereo"
      ) %in%
        attr(met_2d, "selected_cols")
    )
  ) {
    log_warn(
      "structure metadata has no structure keys; skipping key-based metadata enrichment"
    )
  }

  met_2d <- met_2d |>
    .ensure_columns(cols = met_cols) |>
    tidytable::filter(
      structure_inchikey %in%
        keys$inchikey_full |
        structure_inchikey_connectivity_layer %in% keys$inchikey |
        structure_smiles_no_stereo %in% keys$smiles
    ) |>
    # Join stereo key map first to recover connectivity/smiles from full inchikey.
    tidytable::left_join(
      y = stereo_k,
      by = "structure_inchikey",
      suffix = c("", "_stereo")
    ) |>
    .ensure_columns(
      cols = c(
        "structure_inchikey_connectivity_layer_stereo",
        "structure_smiles_no_stereo_stereo"
      )
    ) |>
    tidytable::mutate(
      structure_inchikey_connectivity_layer = tidytable::coalesce(
        as.character(structure_inchikey_connectivity_layer),
        as.character(structure_inchikey_connectivity_layer_stereo)
      ),
      structure_smiles_no_stereo = tidytable::coalesce(
        as.character(structure_smiles_no_stereo),
        as.character(structure_smiles_no_stereo_stereo)
      )
    ) |>
    tidytable::select(
      -structure_inchikey_connectivity_layer_stereo,
      -structure_smiles_no_stereo_stereo
    ) |>
    .filter_rows_by_structure_keys(keys = keys) |>
    tidytable::distinct(
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo,
      structure_exact_mass,
      structure_xlogp,
      structure_tag,
      structure_molecular_formula
    ) |>
    tidytable::filter(
      !is.na(structure_inchikey_connectivity_layer) |
        !is.na(structure_smiles_no_stereo)
    )
  # log_trace("Metadata loaded")

  nam_cols <- c(
    "structure_inchikey",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo",
    "structure_name"
  )
  nam_2d <- .safe_fread_selected(
    file = str_nam,
    file_type = "structure names",
    selected_cols = nam_cols
  )
  if (
    !any(
      c(
        "structure_inchikey",
        "structure_inchikey_connectivity_layer",
        "structure_smiles_no_stereo"
      ) %in%
        attr(nam_2d, "selected_cols")
    )
  ) {
    log_warn(
      "structure names has no structure keys; skipping key-based name enrichment"
    )
  }

  nam_2d <- nam_2d |>
    .ensure_columns(cols = nam_cols) |>
    tidytable::filter(
      structure_inchikey %in%
        keys$inchikey_full |
        structure_inchikey_connectivity_layer %in% keys$inchikey |
        structure_smiles_no_stereo %in% keys$smiles
    ) |>
    # Join stereo key map first to recover connectivity/smiles from full inchikey.
    tidytable::left_join(
      y = stereo_k,
      by = "structure_inchikey",
      suffix = c("", "_stereo")
    ) |>
    .ensure_columns(
      cols = c(
        "structure_inchikey_connectivity_layer_stereo",
        "structure_smiles_no_stereo_stereo"
      )
    ) |>
    tidytable::mutate(
      structure_inchikey_connectivity_layer = tidytable::coalesce(
        as.character(structure_inchikey_connectivity_layer),
        as.character(structure_inchikey_connectivity_layer_stereo)
      ),
      structure_smiles_no_stereo = tidytable::coalesce(
        as.character(structure_smiles_no_stereo),
        as.character(structure_smiles_no_stereo_stereo)
      )
    ) |>
    tidytable::select(
      -structure_inchikey_connectivity_layer_stereo,
      -structure_smiles_no_stereo_stereo
    ) |>
    tidytable::mutate(
      structure_name = .normalize_structure_name(structure_name)
    ) |>
    .filter_rows_by_structure_keys(keys = keys) |>
    tidytable::distinct(
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo,
      structure_name
    )
  # log_trace("Names loaded")

  tax_cla <- safe_fread(
    file = str_tax_cla,
    file_type = "ClassyFire taxonomy",
    na.strings = c("", "NA"),
    colClasses = "character",
    select = c(
      "structure_inchikey_connectivity_layer",
      "structure_tax_cla_chemontid",
      "structure_tax_cla_01kin",
      "structure_tax_cla_02sup",
      "structure_tax_cla_03cla",
      "structure_tax_cla_04dirpar"
    )
  ) |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_structure_tax_cla_chemontid_i = structure_tax_cla_chemontid,
      candidate_structure_tax_cla_01kin_i = structure_tax_cla_01kin,
      candidate_structure_tax_cla_02sup_i = structure_tax_cla_02sup,
      candidate_structure_tax_cla_03cla_i = structure_tax_cla_03cla,
      candidate_structure_tax_cla_04dirpar_i = structure_tax_cla_04dirpar
    ) |>
    tidytable::filter(
      candidate_structure_inchikey_connectivity_layer %in% keys$inchikey
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      .keep_all = TRUE
    )
  # log_trace("Classyfire done")

  tax_npc <- safe_fread(
    file = str_tax_npc,
    file_type = "NPClassifier taxonomy",
    na.strings = c("", "NA"),
    colClasses = "character",
    select = c(
      "structure_smiles_no_stereo",
      "structure_tax_npc_01pat",
      "structure_tax_npc_02sup",
      "structure_tax_npc_03cla"
    )
  ) |>
    tidytable::select(
      candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
      candidate_structure_tax_npc_01pat_s = structure_tax_npc_01pat,
      candidate_structure_tax_npc_02sup_s = structure_tax_npc_02sup,
      candidate_structure_tax_npc_03cla_s = structure_tax_npc_03cla
    ) |>
    tidytable::filter(
      candidate_structure_smiles_no_stereo %in% keys$smiles
    ) |>
    tidytable::distinct(
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )
  # log_trace("NPClassifier done")

  met_lookup <- met_2d |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp,
      structure_tag
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::group_by(candidate_structure_inchikey_connectivity_layer) |>
    tidytable::summarize(
      .lk_molecular_formula = .resolve_single_or_na(
        structure_molecular_formula
      ),
      .lk_exact_mass = .resolve_numeric_or_na(structure_exact_mass),
      .lk_xlogp = .resolve_xlogp(structure_xlogp),
      .lk_tag = .collapse_unique_non_empty(structure_tag)
    ) |>
    tidytable::ungroup()
  rm(met_2d)
  # log_trace("Metadata done")

  nam_lookup <- nam_2d |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      structure_name
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::group_by(candidate_structure_inchikey_connectivity_layer) |>
    tidytable::summarize(
      .lk_name = .collapse_harmonized_names(structure_name)
    ) |>
    tidytable::ungroup()
  rm(nam_2d)
  # log_trace("Names done")

  # Inject placeholder columns early if missing in input df.
  df <- .ensure_structure_placeholders(df)

  structure_keys <- df |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo
    )

  key_lookup <- structure_keys |>
    tidytable::left_join(y = stereo_i) |>
    tidytable::left_join(y = stereo_s) |>
    tidytable::mutate(
      candidate_structure_smiles_no_stereo = tidytable::coalesce(
        candidate_structure_smiles_no_stereo_i,
        candidate_structure_smiles_no_stereo
      ),
      candidate_structure_inchikey_connectivity_layer = tidytable::coalesce(
        candidate_structure_inchikey_connectivity_layer_s,
        candidate_structure_inchikey_connectivity_layer
      )
    ) |>
    tidytable::select(
      -candidate_structure_smiles_no_stereo_i,
      -candidate_structure_inchikey_connectivity_layer_s
    ) |>
    tidytable::left_join(
      y = met_lookup,
      by = "candidate_structure_inchikey_connectivity_layer"
    ) |>
    tidytable::left_join(
      y = nam_lookup,
      by = "candidate_structure_inchikey_connectivity_layer"
    ) |>
    tidytable::left_join(y = tax_npc) |>
    tidytable::left_join(y = tax_cla) |>
    tidytable::rename(
      .enr_candidate_structure_molecular_formula = .lk_molecular_formula,
      .enr_candidate_structure_exact_mass = .lk_exact_mass,
      .enr_candidate_structure_xlogp = .lk_xlogp,
      .enr_candidate_structure_tag = .lk_tag,
      .enr_candidate_structure_name = .lk_name,
      .enr_candidate_structure_tax_npc_01pat = candidate_structure_tax_npc_01pat_s,
      .enr_candidate_structure_tax_npc_02sup = candidate_structure_tax_npc_02sup_s,
      .enr_candidate_structure_tax_npc_03cla = candidate_structure_tax_npc_03cla_s,
      .enr_candidate_structure_tax_cla_chemontid = candidate_structure_tax_cla_chemontid_i,
      .enr_candidate_structure_tax_cla_01kin = candidate_structure_tax_cla_01kin_i,
      .enr_candidate_structure_tax_cla_02sup = candidate_structure_tax_cla_02sup_i,
      .enr_candidate_structure_tax_cla_03cla = candidate_structure_tax_cla_03cla_i,
      .enr_candidate_structure_tax_cla_04dirpar = candidate_structure_tax_cla_04dirpar_i
    )

  table_final <- df |>
    tidytable::left_join(
      y = key_lookup,
      by = c(
        "candidate_structure_inchikey_connectivity_layer",
        "candidate_structure_smiles_no_stereo"
      )
    ) |>
    tidytable::mutate(
      candidate_structure_molecular_formula = .pick_enrichment(
        .enr_candidate_structure_molecular_formula,
        candidate_structure_molecular_formula
      ),
      candidate_structure_exact_mass = tidytable::coalesce(
        .enr_candidate_structure_exact_mass,
        candidate_structure_exact_mass
      ),
      candidate_structure_xlogp = tidytable::coalesce(
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
  rm(
    structure_keys,
    met_lookup,
    nam_lookup,
    key_lookup,
    stereo_k,
    stereo_i,
    stereo_s,
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
    existing
  )
}
