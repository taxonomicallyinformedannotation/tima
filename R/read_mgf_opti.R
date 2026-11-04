#' Format MGF charge string into an integer compatible format.
#'
#' Handles both the trailing-sign convention used by Mascot (`"1+"`, `"1-"`)
#' and the leading-sign convention used by some tools (`"-1"`, `"1"`).
#'
#' @include logs_utils.R
#' @param x [character] `character`
#' @return `character`, charge without +/- at the end but - as prefix if needed
#' @noRd
.format_charge <- function(x) {
  # Detect negative before stripping: trailing "-" OR leading "-"
  negs <- which(endsWith(x, "-") | startsWith(x, "-"))
  # Strip any leading or trailing +/-
  res <- gsub("(^[+-])|([+-]$)", "", x, perl = TRUE)
  if (length(negs)) {
    res[negs] <- paste0("-", res[negs])
  }
  res
}

#' @title Read MGF opti
#'
#' @description This function reads a Mascot Generic Format (MGF) file using
#'     a fast, vectorized approach. It mimics the `MsBackendMgf` implementation
#'     while avoiding per-spectrum `data.frame` allocations and quadratic
#'     row-binding overhead.
#'
#' @details The function extracts all MGF fields and maps them to standard
#'     spectra variable names.
#'
#' @param f [character] Character string specifying the path to a single MGF
#'     file
#' @param msLevel [integer] Integer MS level to assign to spectra (default: 2L
#'     for MS2)
#' @param mapping [character] Named character vector mapping MGF field names to
#'     standard spectra variable names.
#'
#' @return A DataFrame containing the parsed spectra data with standardized
#'     variable names
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Read MGF file with memory-efficient approach
#' library(Spectra)
#'
#' spectra_data <- read_mgf_opti(
#'   f = "path/to/spectra.mgf",
#'   msLevel = 2L
#' )
#'
#' # Create Spectra object from result
#' sps <- Spectra(spectra_data)
#' }
read_mgf_opti <- function(
  f,
  msLevel = 2L, # nolint: object_name_linter. Bioconductor convention.
  mapping = c(
    rtime = "RTINSECONDS",
    acquisitionNum = "SCANS",
    precursorMz = "PEPMASS",
    precursorIntensity = "PEPMASSINT",
    precursorCharge = "CHARGE"
  )
) {
  # Validate inputs
  if (missing(f) || is.null(f) || length(f) == 0L) {
    cli::cli_abort(
      "file path 'f' must be provided",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (length(f) != 1L) {
    cli::cli_abort(
      "please provide a single MGF file. for multiple files, call this function separately for each.",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!file.exists(f)) {
    cli::cli_abort(
      c(
        "MGF file not found",
        "x" = f
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.numeric(msLevel) || msLevel < 1L) {
    cli::cli_abort(
      c(
        "msLevel must be a positive integer",
        "x" = as.character(msLevel)
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Get file size for progress reporting
  file_size_mb <- file.info(f)$size / 1024^2
  log_info(
    "Reading MGF file (%s MB) with optimized parser: %s",
    round(file_size_mb, 2),
    f
  )
  log_debug("Using MS level: %d", msLevel)

  lines <- readLines(f, warn = FALSE)
  needs_line_trim <- startsWith(lines, " ") |
    startsWith(lines, "\t") |
    endsWith(lines, " ") |
    endsWith(lines, "\t") |
    endsWith(lines, "\r")
  if (any(needs_line_trim)) {
    lines[needs_line_trim] <- trimws(lines[needs_line_trim])
  }
  begins <- which(lines == "BEGIN IONS")
  ends <- which(lines == "END IONS")
  n_blocks <- min(length(begins), length(ends))

  log_info("Total spectra read: %s", n_blocks)

  make_empty_mgf_df <- function() {
    log_warn(
      "No spectra with fragment peaks found in file; returning empty result"
    )
    empty <- methods::as(
      data.frame(
        TITLE = character(0L),
        precursorMz = numeric(0L),
        precursorIntensity = numeric(0L),
        stringsAsFactors = FALSE
      ),
      Class = "DataFrame"
    )
    empty$mz <- methods::as(list(), Class = "NumericList")
    empty$intensity <- methods::as(list(), Class = "NumericList")
    empty$msLevel <- integer(0L)
    empty$dataOrigin <- character(0L)
    empty
  }

  if (n_blocks == 0L) {
    return(make_empty_mgf_df())
  }

  s_idx <- begins[seq_len(n_blocks)] + 1L
  e_idx <- ends[seq_len(n_blocks)] - 1L
  lens <- pmax(0L, e_idx - s_idx + 1L)

  if (sum(lens) == 0L) {
    return(make_empty_mgf_df())
  }

  line_idx <- sequence(lens, from = s_idx)
  block_id <- rep.int(seq_len(n_blocks), lens)
  inner_lines <- lines[line_idx]
  rm(lines, line_idx)

  non_empty <- nzchar(inner_lines) & !startsWith(inner_lines, "#")
  if (!all(non_empty)) {
    inner_lines <- inner_lines[non_empty]
    block_id <- block_id[non_empty]
  }

  if (length(inner_lines) == 0L) {
    return(make_empty_mgf_df())
  }

  eq_pos <- regexpr("=", inner_lines, fixed = TRUE)
  is_desc <- eq_pos > 0L

  pk_lines <- inner_lines[!is_desc]
  pk_blocks <- block_id[!is_desc]

  if (length(pk_lines) == 0L) {
    return(make_empty_mgf_df())
  }

  sp_pos <- regexpr(" ", pk_lines, fixed = TRUE)
  tab_pos <- if (all(sp_pos > 1L)) {
    integer(length(pk_lines))
  } else {
    regexpr("\t", pk_lines, fixed = TRUE)
  }
  has_tab <- tab_pos > 0L
  if (!any(has_tab) && all(sp_pos > 1L)) {
    mz_str <- substr(pk_lines, 1L, sp_pos - 1L)
    rest_str <- substring(pk_lines, sp_pos + 1L)
    if (any(startsWith(rest_str, " "))) {
      rest_str <- trimws(rest_str, which = "left")
    }
    valid_pk_line <- nzchar(rest_str)
    if (!all(valid_pk_line)) {
      mz_str <- mz_str[valid_pk_line]
      rest_str <- rest_str[valid_pk_line]
      pk_blocks <- pk_blocks[valid_pk_line]
    }
    if (length(mz_str) == 0L) {
      return(make_empty_mgf_df())
    }
    sp2 <- regexpr(" ", rest_str, fixed = TRUE)
    has_sp2 <- sp2 > 0L
    if (any(has_sp2)) {
      rest_str[has_sp2] <- substr(rest_str[has_sp2], 1L, sp2[has_sp2] - 1L)
    }
    rm(pk_lines, sp_pos, tab_pos, has_tab, sp2, has_sp2)
  } else {
    sep1 <- regexpr("[ \t]+", pk_lines, perl = TRUE)
    sep1_len <- attr(sep1, "match.length")
    valid_pk_line <- sep1 > 1L & (sep1 + sep1_len) <= nchar(pk_lines)
    if (!all(valid_pk_line)) {
      pk_lines <- pk_lines[valid_pk_line]
      pk_blocks <- pk_blocks[valid_pk_line]
      sep1_len <- sep1_len[valid_pk_line]
      sep1 <- sep1[valid_pk_line]
    }

    if (length(pk_lines) == 0L) {
      return(make_empty_mgf_df())
    }

    mz_str <- substr(pk_lines, 1L, sep1 - 1L)
    rest_str <- substring(pk_lines, sep1 + sep1_len)
    rm(pk_lines, sep1, sep1_len, sp_pos, tab_pos, has_tab)

    sep2 <- regexpr("[ \t]", rest_str, perl = TRUE)
    has_sep2 <- sep2 > 0L
    if (any(has_sep2)) {
      rest_str[has_sep2] <- substr(rest_str[has_sep2], 1L, sep2[has_sep2] - 1L)
    }
    rm(sep2, has_sep2)
  }

  valid_blocks <- unique(pk_blocks)
  n_sp <- length(valid_blocks)

  if (n_sp < n_blocks) {
    pk_sp_id <- fastmatch::fmatch(pk_blocks, valid_blocks)
  } else {
    pk_sp_id <- pk_blocks
  }

  mz_all <- as.double(mz_str)
  int_all <- as.double(rest_str)
  rm(mz_str, rest_str, pk_blocks)

  # Check and fix unsorted m/z within any spectrum block
  if (length(mz_all) > 1L) {
    same_blk <- pk_sp_id[-1L] == pk_sp_id[-length(pk_sp_id)]
    unsorted_edges <- which(same_blk & (mz_all[-1L] < mz_all[-length(mz_all)]))
    if (length(unsorted_edges) > 0L) {
      bad_sp <- unique(pk_sp_id[unsorted_edges + 1L])
      in_bad <- pk_sp_id %in% bad_sp
      bad_idx <- which(in_bad)
      ord <- bad_idx[order(pk_sp_id[bad_idx], mz_all[bad_idx])]
      mz_all[bad_idx] <- mz_all[ord]
      int_all[bad_idx] <- int_all[ord]
    }
  }

  grp_fac <- structure(
    pk_sp_id,
    levels = as.character(seq_len(n_sp)),
    class = "factor"
  )
  mz_list <- methods::as(
    unname(split.default(mz_all, grp_fac)),
    Class = "NumericList"
  )
  int_list <- methods::as(
    unname(split.default(int_all, grp_fac)),
    Class = "NumericList"
  )
  rm(mz_all, int_all, pk_sp_id, grp_fac)

  desc_lines <- inner_lines[is_desc]
  desc_blocks <- block_id[is_desc]
  desc_eq <- eq_pos[is_desc]
  rm(inner_lines, block_id, eq_pos, is_desc)

  if (n_sp < n_blocks) {
    desc_sp_id <- fastmatch::fmatch(desc_blocks, valid_blocks)
    keep_d <- !is.na(desc_sp_id)
    desc_lines <- desc_lines[keep_d]
    desc_sp_id <- desc_sp_id[keep_d]
    desc_eq <- desc_eq[keep_d]
  } else {
    desc_sp_id <- desc_blocks
  }
  rm(desc_blocks)

  all_nms <- substr(desc_lines, 1L, desc_eq - 1L)
  all_vals <- substring(desc_lines, desc_eq + 1L)
  rm(desc_lines, desc_eq)

  all_keys <- unique(c(all_nms, "PEPMASS", "PEPMASSINT"))
  n_keys <- length(all_keys)
  if (length(all_nms) > 0L) {
    key_idx <- fastmatch::fmatch(all_nms, all_keys)
    flat_pos <- desc_sp_id + (key_idx - 1L) * n_sp
    first_occ <- !duplicated(flat_pos)
    mat <- matrix(NA_character_, nrow = n_sp, ncol = n_keys)
    mat[flat_pos[first_occ]] <- all_vals[first_occ]
    colnames(mat) <- all_keys
    res <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    res <- data.frame(
      PEPMASS = rep(NA_character_, n_sp),
      PEPMASSINT = rep(NA_character_, n_sp),
      stringsAsFactors = FALSE
    )
  }
  rm(all_nms, all_vals, desc_sp_id)

  if ("PEPMASS" %in% colnames(res)) {
    pm_raw <- res$PEPMASS
    non_na_pm <- which(!is.na(pm_raw) & nzchar(pm_raw))
    pm_mz <- rep(NA_character_, n_sp)
    pm_int <- rep(NA_character_, n_sp)
    if (length(non_na_pm) > 0L) {
      pm_sub <- pm_raw[non_na_pm]
      pm_sep <- regexpr("[ \t]+", pm_sub, perl = TRUE)
      has_pm_sep <- pm_sep > 0L
      if (!all(has_pm_sep)) {
        pm_mz[non_na_pm[!has_pm_sep]] <- pm_sub[!has_pm_sep]
      }
      if (any(has_pm_sep)) {
        idx_s <- which(has_pm_sep)
        s_pos <- pm_sep[idx_s]
        s_len <- attr(pm_sep, "match.length")[idx_s]
        pm_mz[non_na_pm[idx_s]] <- substr(pm_sub[idx_s], 1L, s_pos - 1L)
        pm_rest <- substring(pm_sub[idx_s], s_pos + s_len)
        pm_sep2 <- regexpr("[ \t]", pm_rest, perl = TRUE)
        has_s2 <- pm_sep2 > 0L
        if (any(has_s2)) {
          pm_rest[has_s2] <- substr(pm_rest[has_s2], 1L, pm_sep2[has_s2] - 1L)
        }
        pm_rest[!nzchar(pm_rest)] <- NA_character_
        pm_int[non_na_pm[idx_s]] <- pm_rest
      }
    }
    res$PEPMASS <- pm_mz
    res$PEPMASSINT <- pm_int
  }

  # Format charge field if present
  if ("CHARGE" %in% colnames(res)) {
    res$CHARGE <- .format_charge(res$CHARGE)
  }

  # Map MGF field names to standard spectra variable names
  idx <- match(colnames(res), mapping)
  not_na <- !is.na(idx)
  if (any(not_na)) {
    colnames(res)[not_na] <- names(mapping)[idx][not_na]
  }

  # Convert column types to match core spectra variables
  spv <- Spectra::coreSpectraVariables()
  spv <- spv[!names(spv) %in% c("mz", "intensity")]

  col_names <- colnames(res)
  spv_names <- names(spv)

  col_matches <- match(col_names, spv_names, nomatch = 0L)
  needs_conversion <- col_matches > 0L
  if (any(needs_conversion)) {
    res[needs_conversion] <- Map(
      function(col, type_class) methods::as(object = col, Class = type_class),
      res[needs_conversion],
      spv[col_matches[needs_conversion]]
    )
  }

  # Convert to DataFrame and set up peak lists
  res <- methods::as(object = res, Class = "DataFrame")
  res$mz <- mz_list
  res$intensity <- int_list
  res$dataOrigin <- f

  if (!"msLevel" %in% colnames(res)) {
    res$msLevel <- as.integer(msLevel)
  }

  res
}

#' Read the metadata of an MGF file
#'
#' @description Reads only the `KEY=VALUE` fields of each `BEGIN IONS` /
#'     `END IONS` block. Peaks are not parsed and no spectrum is dropped, so
#'     entries without fragments or without `PEPMASS` (common in retention
#'     time libraries) are kept.
#'
#' @param f [character] Path to a single MGF file.
#'
#' @return A tidytable with one row per block and one character column per
#'     key. Keys are upper-cased; the first value wins when a key repeats.
#'
#' @keywords internal
read_mgf_metadata <- function(f) {
  if (length(f) != 1L || !file.exists(f)) {
    cli::cli_abort(
      c("MGF file not found", "x" = as.character(f)),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  lines <- readLines(f, warn = FALSE)
  needs_line_trim <- startsWith(lines, " ") |
    startsWith(lines, "\t") |
    endsWith(lines, " ") |
    endsWith(lines, "\t") |
    endsWith(lines, "\r")
  if (any(needs_line_trim)) {
    lines[needs_line_trim] <- trimws(lines[needs_line_trim])
  }
  is_begin <- lines == "BEGIN IONS"
  is_end <- lines == "END IONS"
  block <- cumsum(is_begin)
  inside <- block > cumsum(is_end)
  has_eq <- regexpr("=", lines, fixed = TRUE) > 0L
  is_field <- inside & !is_begin & has_eq

  if (!any(is_field)) {
    return(tidytable::tidytable())
  }

  fields <- lines[is_field]
  field_blocks <- block[is_field]
  rm(lines, is_begin, is_end, block, inside, has_eq, is_field)

  eq <- regexpr("=", fields, fixed = TRUE)
  keys <- toupper(trimws(substr(fields, 1L, eq - 1L)))
  vals <- trimws(substring(fields, eq + 1L))

  u_blocks <- unique(field_blocks)
  u_keys <- unique(keys)
  n_b <- length(u_blocks)
  n_k <- length(u_keys)

  b_idx <- fastmatch::fmatch(field_blocks, u_blocks)
  k_idx <- fastmatch::fmatch(keys, u_keys)
  flat_pos <- b_idx + (k_idx - 1L) * n_b
  first_occ <- !duplicated(flat_pos)

  mat <- matrix(NA_character_, nrow = n_b, ncol = n_k)
  mat[flat_pos[first_occ]] <- vals[first_occ]
  colnames(mat) <- u_keys

  tidytable::as_tidytable(as.data.frame(
    mat,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}
