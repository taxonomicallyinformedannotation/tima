#' @title Prepare HMDB-like structure organism pairs library
#'
#' @description Internal helper for HMDB ecosystem databases distributed as
#'     SDF archives with HMDB-style fields.
#'
#' @include columns_utils.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input [character] Character string path to SDF zip file
#' @param output [character] Character string path for prepared SOP output
#' @param source_name [character] Character label used in logs
#' @param organism_name [character] Character organism name attached to all rows
#' @param organism_taxonomy [list] Named list containing organism taxonomy
#'     fields
#' @param tag [character] Character scalar tag propagated through SOP pipeline
#'
#' @return Character string path to prepared SOP file
#'
#' @keywords internal
prepare_libraries_sop_hmdb_like <- function(
  input,
  output,
  source_name,
  organism_name = "Homo sapiens",
  organism_taxonomy = list(
    organism_taxonomy_ottid = "770315",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Metazoa",
    organism_taxonomy_03phylum = "Chordata",
    organism_taxonomy_04class = "Mammalia",
    organism_taxonomy_05order = "Primates",
    organism_taxonomy_06family = "Hominidae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Homo",
    organism_taxonomy_09species = "Homo sapiens",
    organism_taxonomy_10varietas = NA_character_
  ),
  tag = NA_character_
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
  if (!is.character(source_name) || length(source_name) != 1L) {
    cli::cli_abort(
      "source_name must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (is.null(tag) || length(tag) == 0L) {
    tag <- NA_character_
  }
  if (!is.character(tag) || length(tag) != 1L) {
    cli::cli_abort(
      "tag must be a single character string or NA",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  ctx <- log_operation(
    "prepare_libraries_sop_hmdb_like",
    source = source_name,
    input = input,
    tag = ifelse(is.na(tag), "NA", tag)
  )

  if (!file.exists(output) || file.size(output) < 100000) {
    if (!file.exists(input)) {
      log_warn("%s file not found: %s", source_name, input)
      hmdb_like_prepared <- fake_sop_columns()
    } else {
      log_debug("Processing %s from: %s", source_name, input)

      hmdb_like_prepared <- tryCatch(
        expr = {
          hmdb_df <- parse_hmdb_like_input(input)

          if (nrow(hmdb_df) == 0L) {
            hmdb_like_prepared <- fake_sop_columns()[0, ]
          } else {
            hmdb_like_prepared <- hmdb_df |>
              tidytable::mutate(tidytable::across(
                .cols = tidyselect::everything(),
                .fns = tidytable::na_if,
                ""
              )) |>
              ## SMILES is the single source of truth. All computable
              ## fields (InChIKey, formula, mass, xlogp) are derived
              ## from SMILES via process_smiles() in split_tables_sop().
              tidytable::filter(!is.na(smiles)) |>
              tidytable::mutate(
                structure_inchikey_2D = NA_character_,
                structure_smiles_2D = NA_character_
              ) |>
              tidytable::select(
                structure_name = name,
                structure_inchikey = inchikey,
                structure_smiles = smiles,
                structure_inchikey_2D,
                structure_smiles_2D,
                structure_molecular_formula = formula,
                structure_exact_mass = mass,
                structure_xlogp = logp
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
                structure_tag = tag,
                organism_name = organism_name,
                reference_doi = NA_character_
              ) |>
              tidytable::mutate(!!!organism_taxonomy) |>
              select_sop_columns() |>
              round_reals() |>
              tidytable::distinct()

            if (nrow(hmdb_like_prepared) == 0L) {
              hmdb_like_prepared <- fake_sop_columns()[0, ]
            }
          }

          hmdb_like_prepared
        },
        error = function(e) {
          log_error(
            "Something went wrong with %s processing: %s",
            source_name,
            conditionMessage(e)
          )
          fake_sop_columns()
        }
      )
    }

    export_output(x = hmdb_like_prepared, file = output)
    log_complete(ctx, n_pairs = nrow(hmdb_like_prepared))
  } else {
    log_debug("%s library already exists and is valid", source_name)
    log_complete(ctx, cached = TRUE)
    output
  }
  output
}

parse_hmdb_like_input <- function(input) {
  if (grepl("\\.zip$", input, ignore.case = TRUE)) {
    archive_index <- archive::archive(input)
    sdf_entries <- archive_index$path[grepl(
      "\\.sdf$",
      archive_index$path,
      ignore.case = TRUE
    )]

    if (length(sdf_entries) == 0L) {
      cli::cli_abort(
        "no .sdf file found after unzip",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    parsed_tables <- lapply(sdf_entries, function(sdf_entry) {
      con <- archive::archive_read(input, file = sdf_entry)
      on.exit(close(con), add = TRUE)
      parse_hmdb_like_sdf_lines(readLines(
        con = con,
        warn = FALSE,
        encoding = "UTF-8"
      ))
    })

    return(tidytable::bind_rows(parsed_tables))
  }

  if (grepl("\\.sdf$", input, ignore.case = TRUE)) {
    return(parse_hmdb_like_sdf_file(input))
  }

  cli::cli_abort(
    "unsupported input format. expected .zip or .sdf file",
    class = c("tima_validation_error", "tima_error"),
    call = NULL
  )
}

parse_hmdb_like_sdf_file <- function(sdf_file) {
  parse_hmdb_like_sdf_lines(
    readLines(con = sdf_file, warn = FALSE, encoding = "UTF-8")
  )
}

parse_hmdb_like_sdf_lines <- function(sdf_lines) {
  if (length(sdf_lines) == 0L) {
    return(tidytable::tidytable(
      id = character(0),
      smiles = character(0),
      inchikey = character(0),
      formula = character(0),
      mass = character(0),
      logp = character(0),
      name = character(0)
    ))
  }

  field_map <- c(
    DATABASE_ID = "id",
    HMDB_ID = "id",
    SMPDB_ID = "id",
    YMDB_ID = "id",
    T3DB_ID = "id",
    MIMEDB_ID = "id",
    SMILES = "smiles",
    INCHI_KEY = "inchikey",
    INCHIKEY = "inchikey",
    FORMULA = "formula",
    EXACT_MASS = "mass",
    MOLECULAR_WEIGHT = "mass",
    JCHEM_LOGP = "logp",
    ALOGPS_LOGP = "logp",
    XLOGP3 = "logp",
    GENERIC_NAME = "name"
  )

  record_end_idx <- which(sdf_lines == "$$$$")
  n_records <- if (length(record_end_idx) == 0L) 1L else length(record_end_idx)

  out <- list(
    id = rep(NA_character_, n_records),
    smiles = rep(NA_character_, n_records),
    inchikey = rep(NA_character_, n_records),
    formula = rep(NA_character_, n_records),
    mass = rep(NA_character_, n_records),
    logp = rep(NA_character_, n_records),
    name = rep(NA_character_, n_records)
  )

  header_match <- stringi::stri_match_first_regex(
    str = sdf_lines,
    pattern = "^>\\s*<([^>]+)>"
  )
  header_idx <- which(!is.na(header_match[, 2]))

  if (length(header_idx) == 0L) {
    return(tidytable::as_tidytable(out))
  }

  header_fields <- toupper(trimws(header_match[header_idx, 2]))
  canonical_fields <- unname(field_map[header_fields])
  keep_idx <- which(!is.na(canonical_fields))

  if (length(keep_idx) == 0L) {
    return(tidytable::as_tidytable(out))
  }

  header_idx <- header_idx[keep_idx]
  canonical_fields <- canonical_fields[keep_idx]

  value_idx <- pmin(header_idx + 1L, length(sdf_lines))
  values <- trimws(sdf_lines[value_idx])
  valid_values <- nzchar(values) & values != "$$$$"

  if (!all(valid_values)) {
    header_idx <- header_idx[valid_values]
    canonical_fields <- canonical_fields[valid_values]
    values <- values[valid_values]
  }

  if (length(values) == 0L) {
    return(tidytable::as_tidytable(out))
  }

  record_ids <- if (length(record_end_idx) == 0L) {
    rep.int(1L, length(header_idx))
  } else {
    findInterval(header_idx, record_end_idx) + 1L
  }

  field_positions <- split(seq_along(canonical_fields), canonical_fields)

  invisible(lapply(field_positions, function(field_pos) {
    field_name <- canonical_fields[[field_pos[[1L]]]]
    field_records <- record_ids[field_pos]
    first_occurrence <- !duplicated(field_records)
    out[[field_name]][field_records[first_occurrence]] <<-
      values[field_pos[first_occurrence]]
    NULL
  }))

  tidytable::as_tidytable(out)
}
