#' @title Process SMILES strings
#'
#' @description Processes SMILES using RDKit (via Python) to standardize
#'     structures, generate InChIKeys, calculate molecular properties,
#'     and extract 2D representations. Results are cached to avoid reprocessing.
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include safe_fread.R
#' @include validations_params.R
#'
#' @param df [data.frame] Data frame containing SMILES strings
#' @param smiles_colname [character] Column name containing SMILES
#'     (default: "structure_smiles_initial")
#' @param cache [character] Path to cached processed SMILES file, or NULL to
#'     skip caching
#'
#' @return Data frame with processed SMILES including InChIKey, molecular
#'     formula
#' (with isotopes shown), exact mass (with isotope contributions), 2D SMILES,
#'     xLogP, and connectivity layer
#'
#' @family chemical-classification
#' @concept EDAM:topic_2258 Cheminformatics
#' @concept EDAM:data_2301 SMILES string
#' @concept EDAM:format_1196 SMILES
#' @concept EDAM:format_1199 InChIKey
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Natural compound
#' df <- data.frame(
#'   structure_smiles_initial = "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
#' )
#' result <- process_smiles(df)
#' # Formula: C6H12O6, Mass: 180.063 Da
#'
#' # Isotope-labeled compound (4× 13C)
#' df_labeled <- data.frame(
#'   structure_smiles_initial = "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
#' )
#' result_labeled <- process_smiles(df_labeled)
#' # Formula: C2[13C]4H12O6 (isotopes shown separately)
#' # Mass: 184.077 Da (difference of ~4.013 Da from natural)
#' # SMILES preserves [13C] notation
#' # InChIKey differs from natural glucose
#' }
process_smiles <- function(
  df,
  smiles_colname = "structure_smiles_initial",
  cache = NULL
) {
  ctx <- log_operation("process_smiles", n_structures = nrow(df))

  validate_dataframe(df, param_name = "df")
  validate_character(
    smiles_colname,
    param_name = "smiles_colname",
    allow_empty = FALSE
  )

  if (!smiles_colname %in% names(df)) {
    cli::cli_abort(
      c(
        "column not found in data frame",
        "x" = smiles_colname
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Processing SMILES with RDKit")

  table_smiles <- extract_unique_smiles(df, smiles_colname)

  if (nrow(table_smiles) == 0L) {
    log_warn("No valid SMILES to process")
    return(df)
  }

  log_debug("Processing %d unique SMILES", nrow(table_smiles))

  table_processed_1 <- load_smiles_cache(cache, smiles_colname)
  extra_parts <- list()

  # Reuse cached entries when the input SMILES is already a canonical stereo
  # or 2D (no-stereo) SMILES present in the cache.
  if (
    nrow(table_processed_1) > 0L &&
      all(
        c(
          "structure_smiles",
          "structure_inchikey",
          "structure_molecular_formula",
          "structure_exact_mass",
          "structure_smiles_no_stereo",
          "structure_xlogp"
        ) %in%
          names(table_processed_1)
      )
  ) {
    unmatched <- table_smiles[[smiles_colname]][
      is.na(fastmatch::fmatch(
        table_smiles[[smiles_colname]],
        table_processed_1[[smiles_colname]]
      ))
    ]
    attr(table_processed_1[[smiles_colname]], ".match.hash") <- NULL
    if (length(unmatched) > 0L) {
      # 1. Exact match against canonical stereo SMILES
      m_can <- fastmatch::fmatch(
        unmatched,
        table_processed_1$structure_smiles
      )
      attr(table_processed_1$structure_smiles, ".match.hash") <- NULL
      hit_can <- !is.na(m_can)
      if (any(hit_can)) {
        extra_can <- table_processed_1[m_can[hit_can], ]
        extra_can[[smiles_colname]] <- unmatched[hit_can]
        extra_parts[[length(extra_parts) + 1L]] <- extra_can
        unmatched <- unmatched[!hit_can]
      }
    }
    if (length(unmatched) > 0L) {
      # 2. Exact match against standardized 2D (no-stereo) SMILES
      m_2d <- fastmatch::fmatch(
        unmatched,
        table_processed_1$structure_smiles_no_stereo
      )
      attr(table_processed_1$structure_smiles_no_stereo, ".match.hash") <- NULL
      hit_2d <- !is.na(m_2d)
      if (any(hit_2d)) {
        extra_2d <- table_processed_1[m_2d[hit_2d], ]
        extra_2d[[smiles_colname]] <- unmatched[hit_2d]
        extra_2d$structure_smiles <- extra_2d$structure_smiles_no_stereo
        ik <- extra_2d$structure_inchikey
        valid_ik <- !is.na(ik) & nchar(ik) >= 27L
        if (any(valid_ik)) {
          extra_2d$structure_inchikey[valid_ik] <- paste0(
            stri_sub(str = ik[valid_ik], from = 1L, to = 14L),
            "-UHFFFAOYSA-",
            stri_sub(str = ik[valid_ik], from = -1L, to = -1L)
          )
        }
        extra_parts[[length(extra_parts) + 1L]] <- extra_2d
        unmatched <- unmatched[!hit_2d]
      }
    }
    table_smiles_to_process <- if (length(unmatched) == 0L) {
      table_smiles[0L, ]
    } else {
      out_unm <- tidytable::tidytable(tmp__ = unmatched)
      names(out_unm) <- smiles_colname
      out_unm
    }
  } else {
    # Identify SMILES not yet in cache
    in_cache <- !is.na(fastmatch::fmatch(
      table_smiles[[smiles_colname]],
      table_processed_1[[smiles_colname]]
    ))
    attr(table_processed_1[[smiles_colname]], ".match.hash") <- NULL
    table_smiles_to_process <- table_smiles[!in_cache, ]
  }

  n_to_process <- nrow(table_smiles_to_process)

  if (n_to_process == 0L) {
    log_info("All SMILES already in cache, no processing needed")
    keep_1 <- which(
      !is.na(fastmatch::fmatch(
        table_processed_1[[smiles_colname]],
        table_smiles[[smiles_colname]]
      ))
    )
    out_cached <- if (length(extra_parts) > 0L) {
      tidytable::bind_rows(c(list(table_processed_1[keep_1, ]), extra_parts))
    } else {
      table_processed_1[keep_1, ]
    }
    sc_vec <- out_cached[[smiles_colname]]
    if (anyDuplicated(sc_vec) > 0L) {
      out_cached <- out_cached[!duplicated(sc_vec), ]
    }
    ik_c <- out_cached$structure_inchikey
    cl_c <- stri_sub(str = ik_c, from = 1L, to = 14L)
    out_cached$structure_inchikey_connectivity_layer <- cl_c
    out_cached$structure_inchikey_no_stereo <- ifelse(
      is.na(ik_c),
      NA_character_,
      paste0(cl_c, "-", stri_sub(str = ik_c, from = -1L, to = -1L))
    )
    log_complete(ctx, n_processed = nrow(out_cached))
    return(out_cached)
  }

  if (length(extra_parts) > 0L) {
    table_processed_1 <- tidytable::bind_rows(c(
      list(table_processed_1),
      extra_parts
    ))
  }

  # Load Python Processor only when new SMILES actually require RDKit.
  # This avoids repeated expensive Python startup for cached-only calls.
  load_python_smiles_processor()

  # Create temporary files for Python processing
  input_smi_file <- tempfile(fileext = ".smi")
  output_csv_file <- tempfile(fileext = ".csv.gz")

  log_info(
    "Processing %d new SMILES with RDKit",
    nrow(table_smiles_to_process)
  )

  # Write SMILES to temporary file
  tidytable::fwrite(
    x = table_smiles_to_process,
    file = input_smi_file
  )

  # Process SMILES using Python/RDKit
  tryCatch(
    {
      reticulate::py$process_smiles(input_smi_file, output_csv_file)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "rdkit smiles processing failed",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  # Read processed results
  table_processed_2 <- safe_fread(
    file = output_csv_file,
    file_type = "RDKit processed SMILES"
  ) |>
    tidytable::rename(
      !!as.name(smiles_colname) := "structure_smiles_initial"
    )

  log_info(
    "Successfully processed %d SMILES",
    nrow(table_processed_2)
  )

  # Combine cached and new results
  combined_processed <- tidytable::bind_rows(
    table_processed_1,
    table_processed_2
  )
  cp_sc <- combined_processed[[smiles_colname]]
  keep_cp <- which(!is.na(cp_sc) & !duplicated(cp_sc))
  combined_processed <- combined_processed[keep_cp, ]

  # Persist updated SMILES cache to disk when a valid cache path is provided
  if (
    !is.null(cache) &&
      is.character(cache) &&
      length(cache) == 1L &&
      nzchar(cache) &&
      dir.exists(dirname(cache))
  ) {
    tryCatch(
      {
        compress_method <- if (grepl("\\.gz$", cache, ignore.case = TRUE)) {
          "gzip"
        } else {
          "none"
        }
        tidytable::fwrite(
          x = combined_processed,
          file = cache,
          compress = compress_method,
          showProgress = FALSE
        )
        .smiles_cache_store$invalidate()
      },
      error = function(e) {
        log_debug("Could not update SMILES cache %s: %s", cache, e$message)
      }
    )
  }

  keep_final <- which(
    !is.na(fastmatch::fmatch(
      combined_processed[[smiles_colname]],
      table_smiles[[smiles_colname]]
    ))
  )
  table_final <- combined_processed[keep_final, ]
  ik_f <- table_final$structure_inchikey
  cl_f <- stri_sub(str = ik_f, from = 1L, to = 14L)
  table_final$structure_inchikey_connectivity_layer <- cl_f
  table_final$structure_inchikey_no_stereo <- ifelse(
    is.na(ik_f),
    NA_character_,
    paste0(cl_f, "-", stri_sub(str = ik_f, from = -1L, to = -1L))
  )

  log_complete(ctx, n_processed = nrow(table_final))

  table_final
}


#' Load Python SMILES processor
#' @keywords internal
load_python_smiles_processor <- function() {
  tryCatch(
    {
      reticulate::py_require(packages = c("rdkit", "chembl_structure_pipeline"))
      py_script <- system.file("python/process_smiles.py", package = "tima")
      reticulate::source_python(file = py_script)
    },
    error = function(e) {
      log_error(
        "Failed to load Python processor: %s",
        conditionMessage(e)
      )
      cli::cli_abort(
        c(
          "cannot load python smiles processor",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )
}

#' Extract unique SMILES from dataframe
#' @keywords internal
extract_unique_smiles <- function(df, smiles_colname) {
  col_vec <- df[[smiles_colname]]
  u_sm <- unique(col_vec)
  u_sm <- u_sm[!is.na(u_sm)]
  out <- tidytable::tidytable(tmp__ = u_sm)
  names(out) <- smiles_colname
  out
}

.smiles_cache_store <- local({
  .env <- new.env(parent = emptyenv())
  list(
    get = function(key) {
      if (exists(key, envir = .env, inherits = FALSE)) {
        .env[[key]]
      } else {
        NULL
      }
    },
    set = function(key, val) {
      rm(list = ls(envir = .env, all.names = TRUE), envir = .env)
      .env[[key]] <- val
      invisible(val)
    },
    invalidate = function() {
      rm(list = ls(envir = .env, all.names = TRUE), envir = .env)
      invisible(NULL)
    }
  )
})

#' Load SMILES cache or create empty template
#' @keywords internal
load_smiles_cache <- function(cache, smiles_colname) {
  if (is.null(cache)) {
    return(create_empty_smiles_template(smiles_colname))
  }

  cache_key <- NULL
  if (is.character(cache) && length(cache) == 1L && file.exists(cache)) {
    fi <- file.info(cache)
    cache_key <- paste(
      normalizePath(cache, mustWork = FALSE),
      as.character(fi$mtime),
      as.character(fi$size),
      smiles_colname,
      sep = "|"
    )
    hit <- .smiles_cache_store$get(cache_key)
    if (!is.null(hit)) {
      log_debug("Loaded %d cached SMILES (in-memory)", nrow(hit))
      return(hit)
    }
  }

  tryCatch(
    {
      cached <- safe_fread(
        file = cache,
        file_type = "SMILES cache"
      )
      log_debug("Loaded %d cached SMILES", nrow(cached))
      if (!is.null(cache_key)) {
        .smiles_cache_store$set(cache_key, cached)
      }
      cached
    },
    error = function(e) {
      log_warn("Cache load failed: %s", conditionMessage(e))
      create_empty_smiles_template(smiles_colname)
    }
  )
}

#' Create empty SMILES result template
#' @keywords internal
create_empty_smiles_template <- function(smiles_colname) {
  tidytable::tidytable(
    !!as.name(smiles_colname) := NA_character_,
    structure_smiles = NA_character_,
    structure_inchikey = NA_character_,
    structure_molecular_formula = NA_character_,
    structure_exact_mass = NA_real_,
    structure_smiles_no_stereo = NA_character_,
    structure_xlogp = NA_real_
  )
}
