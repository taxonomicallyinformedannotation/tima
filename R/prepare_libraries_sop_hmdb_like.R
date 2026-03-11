#' @title Prepare HMDB-like structure organism pairs library
#'
#' @description Internal helper for HMDB ecosystem databases distributed as
#'     SDF archives with HMDB-style fields.
#'
#' @include columns_utils.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Character string path to SDF zip file
#' @param output Character string path for prepared SOP output
#' @param source_name Character label used in logs
#' @param organism_name Character organism name attached to all rows
#' @param organism_taxonomy Named list containing organism taxonomy fields
#' @param tag Character scalar tag propagated through SOP pipeline
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
    stop("input must be a single character string")
  }
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }
  if (!is.character(source_name) || length(source_name) != 1L) {
    stop("source_name must be a single character string")
  }

  if (is.null(tag) || length(tag) == 0L) {
    tag <- NA_character_
  }
  if (!is.character(tag) || length(tag) != 1L) {
    stop("tag must be a single character string or NA")
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
              tidytable::filter(!is.na(inchikey)) |>
              tidytable::mutate(
                structure_inchikey_2D = stringi::stri_sub(
                  str = inchikey,
                  from = 1,
                  to = 14
                ),
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
                organism_name = organism_name,
                reference_doi = NA_character_,
                tag = tag
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
  }

  output
}

parse_hmdb_like_input <- function(input) {
  if (grepl("\\.zip$", input, ignore.case = TRUE)) {
    sdf_entries <- archive::archive(input) |>
      tidytable::as_tidytable() |>
      tidytable::filter(grepl("\\.sdf$", path, ignore.case = TRUE)) |>
      tidytable::pull(path)

    if (length(sdf_entries) == 0L) {
      stop("No .sdf file found after unzip")
    }

    return(purrr::map_dfr(
      .x = sdf_entries,
      .f = function(sdf_entry) {
        con <- archive::archive_read(input, file = sdf_entry)
        on.exit(close(con), add = TRUE)
        parse_hmdb_like_sdf_lines(readLines(
          con = con,
          warn = FALSE,
          encoding = "UTF-8"
        ))
      }
    ))
  }

  if (grepl("\\.sdf$", input, ignore.case = TRUE)) {
    return(parse_hmdb_like_sdf_file(input))
  }

  stop("Unsupported input format. Expected .zip or .sdf file")
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
    GENERIC_NAME = "name",
    NAME = "name",
    COMMON_NAME = "name",
    DATABASE_NAME = "name"
  )

  canonical_field <- function(field_name) {
    mapped <- unname(field_map[toupper(field_name)])
    if (length(mapped) == 0L) {
      return(NA_character_)
    }
    mapped[[1L]]
  }

  n_records <- sum(trimws(sdf_lines) == "$$$$")
  if (n_records == 0L) {
    n_records <- 1L
  }

  out <- list(
    id = rep(NA_character_, n_records),
    smiles = rep(NA_character_, n_records),
    inchikey = rep(NA_character_, n_records),
    formula = rep(NA_character_, n_records),
    mass = rep(NA_character_, n_records),
    logp = rep(NA_character_, n_records),
    name = rep(NA_character_, n_records)
  )

  rec_idx <- 1L
  current_target <- NA_character_

  for (line in sdf_lines) {
    line_trim <- trimws(line)

    if (identical(line_trim, "$$$$")) {
      rec_idx <- rec_idx + 1L
      if (rec_idx > n_records) {
        break
      }
      current_target <- NA_character_
      next
    }

    if (startsWith(line_trim, ">")) {
      field_match <- stringi::stri_match_first_regex(
        str = line_trim,
        pattern = "^>\\s*<([^>]+)>"
      )
      if (!is.na(field_match[1, 2])) {
        current_target <- canonical_field(field_match[1, 2])
      } else {
        current_target <- NA_character_
      }
      next
    }

    if (
      !is.na(current_target) &&
        nzchar(line_trim) &&
        is.na(out[[current_target]][rec_idx])
    ) {
      out[[current_target]][rec_idx] <- line_trim
      current_target <- NA_character_
    }
  }

  tidytable::tidytable(
    id = out$id,
    smiles = out$smiles,
    inchikey = out$inchikey,
    formula = out$formula,
    mass = out$mass,
    logp = out$logp,
    name = out$name
  )
}

extract_hmdb_like_sdf_fields <- function(record_lines) {
  fields <- list()
  i <- 1L
  n <- length(record_lines)

  while (i <= n) {
    field_match <- stringi::stri_match_first_regex(
      str = record_lines[[i]],
      pattern = "^>\\s*<([^>]+)>"
    )

    if (!is.na(field_match[1, 2])) {
      field_name <- toupper(trimws(field_match[1, 2]))
      i <- i + 1L
      value_lines <- character(0)

      while (
        i <= n &&
          trimws(record_lines[[i]]) != "" &&
          !grepl("^>\\s*<", record_lines[[i]]) &&
          trimws(record_lines[[i]]) != "$$$$"
      ) {
        value_lines <- c(value_lines, trimws(record_lines[[i]]))
        i <- i + 1L
      }

      field_value <- paste(value_lines, collapse = "\n")
      existing_value <- fields[[field_name]]
      if (is.null(existing_value) || !nzchar(existing_value)) {
        fields[[field_name]] <- field_value
      }
    } else {
      i <- i + 1L
    }
  }

  fields
}
