#' @title Prepare HMDB-like structure organism pairs library
#'
#' @description Internal helper for HMDB ecosystem databases distributed as
#'     SDF archives with HMDB-style fields.
#'
#' @include columns_utils.R
#' @include round_reals.R
#' @include select_sop_columns.R
#' @include logs_utils.R
#' @include export_output.R
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
            ## SMILES is the single source of truth. All computable
            ## fields (InChIKey, formula, mass, xlogp) are derived
            ## from SMILES via process_smiles() in split_tables_sop().
            sm <- hmdb_df$smiles
            keep <- !is.na(sm) & nzchar(sm)
            if (!any(keep)) {
              hmdb_like_prepared <- fake_sop_columns()[0, ]
            } else {
              clean_empty <- function(x) {
                v <- x[keep]
                empty_mask <- !is.na(v) & !nzchar(v)
                if (any(empty_mask)) {
                  v[empty_mask] <- NA_character_
                }
                v
              }
              core <- tidytable::tidytable(
                structure_name = clean_empty(hmdb_df$name),
                structure_inchikey = clean_empty(hmdb_df$inchikey),
                structure_smiles = sm[keep],
                structure_molecular_formula = clean_empty(hmdb_df$formula),
                structure_exact_mass = clean_empty(hmdb_df$mass),
                structure_xlogp = clean_empty(hmdb_df$logp)
              ) |>
                round_reals() |>
                tidytable::distinct()

              if (nrow(core) == 0L) {
                hmdb_like_prepared <- fake_sop_columns()[0, ]
              } else {
                n_c <- nrow(core)
                na_chr <- rep.int(NA_character_, n_c)
                out_list <- c(
                  list(
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
                    structure_taxonomy_classyfire_04directparent = na_chr,
                    structure_tag = rep.int(tag, n_c),
                    organism_name = rep.int(organism_name, n_c),
                    reference_doi = na_chr
                  ),
                  lapply(organism_taxonomy, rep.int, times = n_c)
                )
                hmdb_like_prepared <- tidytable::as_tidytable(out_list) |>
                  select_sop_columns()
              }
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
    n_pairs <- nrow(hmdb_like_prepared)
    rm(hmdb_like_prepared)
    log_complete(ctx, n_pairs = n_pairs)
  } else {
    log_debug("%s library already exists and is valid", source_name)
    log_complete(ctx, cached = TRUE)
    output
  }
  output
}

parse_hmdb_like_input <- function(input) {
  if (grepl("\\.zip$", input, ignore.case = TRUE)) {
    archive_index <- utils::unzip(input, list = TRUE)
    is_sdf <- grepl("\\.sdf$", archive_index$Name, ignore.case = TRUE)
    sdf_entries <- archive_index$Name[is_sdf]

    if (length(sdf_entries) == 0L) {
      cli::cli_abort(
        "no .sdf file found after unzip",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    if (length(sdf_entries) == 1L) {
      con <- unz(description = input, filename = sdf_entries[[1L]])
      on.exit(close(con), add = TRUE)
      return(parse_hmdb_like_sdf_lines(readLines(
        con = con,
        warn = FALSE,
        encoding = "UTF-8"
      )))
    }

    # For archives with many individual .sdf files (e.g., SMPDB with ~50,000
    # files), passing `files = sdf_entries` to utils::unzip() calls
    # unzLocateFile() per file and scans the central directory O(N^2) times.
    # Extracting with `files = NULL` walks the archive once in O(N) time.
    tmp_dir <- tempfile("tima_sdf_")
    dir.create(tmp_dir, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    extracted_paths <- utils::unzip(input, exdir = tmp_dir)
    if (!all(is_sdf)) {
      extracted_paths <- extracted_paths[
        grepl("\\.sdf$", extracted_paths, ignore.case = TRUE)
      ]
    }

    batch_size <- 2000L
    n_files <- length(extracted_paths)
    batches <- split(
      extracted_paths,
      ceiling(seq_len(n_files) / batch_size)
    )
    parsed_tables <- lapply(batches, function(batch_files) {
      lines_list <- lapply(batch_files, function(f) {
        lines <- readLines(con = f, warn = FALSE, encoding = "UTF-8")
        if (length(lines) > 0L && lines[[length(lines)]] != "$$$$") {
          c(lines, "$$$$")
        } else {
          lines
        }
      })
      parse_hmdb_like_sdf_lines(unlist(lines_list, use.names = FALSE))
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

  cand_idx <- which(startsWith(sdf_lines, ">"))
  if (length(cand_idx) == 0L) {
    return(tidytable::as_tidytable(out))
  }

  cand_lines <- sdf_lines[cand_idx]
  u_cand <- unique(cand_lines)
  u_match <- stri_match_first_regex(
    str = u_cand,
    pattern = "^>\\s*<([^>]+)>"
  )
  u_fields <- toupper(trimws(u_match[, 2L]))
  u_canonical <- unname(field_map[u_fields])
  u_field_id <- match(u_canonical, names(out))

  if (all(is.na(u_field_id))) {
    return(tidytable::as_tidytable(out))
  }

  cand_field_id <- u_field_id[fastmatch::fmatch(cand_lines, u_cand)]
  keep_idx <- which(!is.na(cand_field_id))

  if (length(keep_idx) == 0L) {
    return(tidytable::as_tidytable(out))
  }

  header_idx <- cand_idx[keep_idx]
  field_ids <- cand_field_id[keep_idx]

  value_idx <- pmin(header_idx + 1L, length(sdf_lines))
  values <- trimws(sdf_lines[value_idx])
  valid_values <- nzchar(values) & values != "$$$$"

  if (!all(valid_values)) {
    header_idx <- header_idx[valid_values]
    field_ids <- field_ids[valid_values]
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

  for (fid in seq_along(out)) {
    field_pos <- which(field_ids == fid)
    if (length(field_pos) > 0L) {
      field_records <- record_ids[field_pos]
      if (anyDuplicated(field_records) > 0L) {
        first_occurrence <- !duplicated(field_records)
        out[[fid]][field_records[first_occurrence]] <-
          values[field_pos[first_occurrence]]
      } else {
        out[[fid]][field_records] <- values[field_pos]
      }
    }
  }

  tidytable::as_tidytable(out)
}
