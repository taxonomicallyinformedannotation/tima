#' @title Prepare taxa
#'
#' @description This function prepares taxonomic information for features by
#'     matching organism names to Open Tree of Life taxonomy. Can attribute
#'     all features to a single organism or distribute them across multiple
#'     organisms based on relative intensities in samples.
#'
#' @details Depending on whether features are aligned between samples from
#'     various organisms, this function either:
#'     - Attributes all features to a single organism (if taxon specified), or
#'     - Attributes features to multiple organisms based on their relative
#'       intensities across samples (using metadata)
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include get_organism_taxonomy_ott.R
#' @include safe_fread.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include export_params.R
#' @include export_output.R
#' @include validations_params.R
#'
#' @param input [character] Path to features file with
#'     intensities
#' @param extension [logical] Whether column names contain file
#'     extensions
#' @param name_filename [character] Name of filename column in
#'     metadata
#' @param colname [character] Name of column with biological
#'     source info
#' @param metadata [character] Path to metadata file with
#'     organism info
#' @param org_tax_ott [character] Path to Open Tree of Life
#'     taxonomy file
#' @param output [character] Path for output file
#' @param taxon [character] Organism name to enforce for all
#'     features
#'     (e.g., "Homo sapiens"). If provided, overrides metadata-based assignment.
#' @param min_rel_intensity [numeric] Minimum relative intensity fraction
#'     (default 0.01) to retain an organism attribution in aligned samples.
#'
#' @return Character string path to the prepared taxa file
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' org_tax_ott <- paste0(
#'   "data/interim/libraries/",
#'   "sop/merged/organisms/taxonomies/ott.tsv"
#' )
#' get_file(url = paste0(dir, org_tax_ott), export = org_tax_ott)
#' get_file(
#'   url = paste0(dir, "data/interim/features/example_features.tsv"),
#'   export = get_params(step = "prepare_taxa")$files$features$prepared
#' )
#' prepare_taxa(
#'   taxon = "Homo sapiens",
#'   org_tax_ott = org_tax_ott
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_taxa <- function(
  input = get_params(step = "prepare_taxa")$files$features$prepared,
  extension = get_params(step = "prepare_taxa")$names$extension,
  name_filename = get_params(step = "prepare_taxa")$names$filename,
  colname = get_params(step = "prepare_taxa")$names$taxon,
  metadata = get_params(step = "prepare_taxa")$files$metadata$raw,
  org_tax_ott = get_params(
    step = "prepare_taxa"
  )$files$libraries$sop$merged$organisms$taxonomies$ott,
  output = get_params(step = "prepare_taxa")$files$metadata$prepared,
  taxon = get_params(step = "prepare_taxa")$organisms$taxon,
  min_rel_intensity = 0.01
) {
  ctx <- log_operation("prepare_taxa", taxon = taxon)

  .make_nd_taxa_table <- function(feature_ids) {
    feature_ids <- unique(as.character(feature_ids))
    tidytable::tidytable(
      feature_id = feature_ids,
      sample_organism_name = "ND",
      sample_organism_01_domain = "ND",
      sample_organism_02_kingdom = "ND",
      sample_organism_03_phylum = "ND",
      sample_organism_04_class = "ND",
      sample_organism_05_order = "ND",
      sample_organism_06_family = "ND",
      sample_organism_07_tribe = "ND",
      sample_organism_08_genus = "ND",
      sample_organism_09_species = "ND",
      sample_organism_10_varietas = "ND"
    )
  }

  validate_character(input, param_name = "input", allow_empty = FALSE)
  validate_file_exists(
    path = input,
    file_type = "features file",
    param_name = "input"
  )

  validate_character(
    org_tax_ott,
    param_name = "org_tax_ott",
    allow_empty = FALSE
  )
  validate_file_exists(
    path = org_tax_ott,
    file_type = "OTT taxonomy file",
    param_name = "org_tax_ott"
  )

  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.logical(extension) || length(extension) != 1L) {
    cli::cli_abort(
      "extension must be a single logical value (TRUE/FALSE)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Check if using single taxon or metadata-based assignment
  if (!is.null(taxon) && !is.na(taxon)) {
    log_debug("Assigning all features to single organism: %s", taxon)
  } else {
    validate_character(metadata, param_name = "metadata", allow_empty = FALSE)
    validate_file_exists(
      path = metadata,
      file_type = "metadata file",
      param_name = "metadata"
    )
  }

  # Handle empty taxon string as NULL
  if (!is.null(taxon) && taxon == "") {
    taxon <- NULL
  }

  if (is.null(taxon)) {
    log_debug("Using metadata for organism assignments")
  }

  hdr_feat <- tryCatch(
    names(data.table::fread(file = input, nrows = 0L, showProgress = FALSE)),
    error = function(e) character(0L)
  )
  wanted_feat_cols <- if (!is.null(taxon)) {
    intersect("feature_id", hdr_feat)
  } else {
    intersect(c("feature_id", "sample", "intensity"), hdr_feat)
  }

  feature_table_0 <- if (length(wanted_feat_cols) > 0L) {
    safe_fread(
      file = input,
      file_type = "features table",
      na.strings = c("", "NA"),
      colClasses = "character",
      select = wanted_feat_cols
    )
  } else {
    safe_fread(
      file = input,
      file_type = "features table",
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  }

  if (is.null(taxon)) {
    metadata_table <- safe_fread(
      file = metadata,
      file_type = "metadata table",
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  }

  if (!is.null(taxon)) {
    metadata_table <- data.frame(taxon)
    colnames(metadata_table) <- colname
  } else {
    # Resolve taxon column name with compatibility fallback.
    if (!colname %in% names(metadata_table)) {
      fallback_taxon <- setdiff(c("ATTRIBUTE_species", "organism"), colname)
      fallback_taxon <- fallback_taxon[
        fallback_taxon %in% names(metadata_table)
      ]
      if (length(fallback_taxon) > 0L) {
        colname <- fallback_taxon[[1L]]
        log_warn(
          "Metadata column '%s' not found; using '%s' instead",
          get_params(step = "prepare_taxa")$names$taxon,
          colname
        )
      } else {
        log_warn(
          "No organism column found in metadata; exporting ND taxonomy for all features"
        )
        taxed_features_table <- .make_nd_taxa_table(feature_table_0$feature_id)
        log_complete(ctx, n_features = nrow(taxed_features_table))
        export_params(
          parameters = get_params(step = "prepare_taxa"),
          step = "prepare_taxa"
        )
        export_output(x = taxed_features_table, file = output)
        rm(taxed_features_table)
        return(output)
      }
    }

    has_organism <- any(
      !is.na(metadata_table[[colname]]) &
        nzchar(trimws(metadata_table[[colname]]))
    )
    if (!has_organism) {
      log_warn(
        "Metadata column '%s' has no organism values; exporting ND taxonomy for all features",
        colname
      )
      taxed_features_table <- .make_nd_taxa_table(feature_table_0$feature_id)
      log_complete(ctx, n_features = nrow(taxed_features_table))
      export_params(
        parameters = get_params(step = "prepare_taxa"),
        step = "prepare_taxa"
      )
      export_output(x = taxed_features_table, file = output)
      rm(taxed_features_table)
      return(output)
    }
  }

  raw_org_col <- as.character(metadata_table[[colname]])
  u_org_raw <- unique(raw_org_col[!is.na(raw_org_col)])
  if (length(u_org_raw) > 0L && any(grepl("|", u_org_raw, fixed = TRUE))) {
    u_org_raw <- unlist(
      strsplit(u_org_raw, "|", fixed = TRUE),
      use.names = FALSE
    )
  }
  organism_table <- tidytable::tidytable(organism = trimws(u_org_raw))

  #  "Retrieving already computed Open Tree of Life Taxonomy"
  # )
  ott_table <- safe_fread(
    file = org_tax_ott,
    file_type = "organism taxonomy (OTT)",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  required_ott_cols <- c(
    "organism_name",
    "organism_taxonomy_ottid",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "organism_taxonomy_04class",
    "organism_taxonomy_05order",
    "organism_taxonomy_06family",
    "organism_taxonomy_07tribe",
    "organism_taxonomy_08genus",
    "organism_taxonomy_09species",
    "organism_taxonomy_10varietas"
  )

  missing_cols <- setdiff(required_ott_cols, names(ott_table))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      c(
        "OTT (Open Tree of Life) taxonomy file is missing required columns",
        "x" = sprintf(
          "Missing columns: %s",
          paste(missing_cols, collapse = ", ")
        ),
        "i" = sprintf(
          "File has columns: %s",
          paste(names(ott_table), collapse = ", ")
        ),
        "!" = "Please ensure your OTT file has the standard TIMA taxonomy columns"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  ott_match_mask <- fastmatch::fmatch(
    ott_table$organism_name,
    organism_table$organism,
    nomatch = 0L
  ) >
    0L
  ott_table <- ott_table[which(ott_match_mask), ]

  organism_table_filled <- organism_table |>
    tidytable::left_join(
      y = ott_table,
      by = c("organism" = "organism_name")
    )
  rm(organism_table, ott_table)

  organism_table_missing <- organism_table_filled |>
    tidytable::filter(is.na(organism_taxonomy_ottid))

  if (nrow(organism_table_missing) != 0) {
    biological_metadata_1 <- organism_table_missing |>
      get_organism_taxonomy_ott()

    biological_metadata <- organism_table_filled |>
      tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
      tidytable::rename(organism_name = organism) |>
      tidytable::bind_rows(biological_metadata_1)
    rm(biological_metadata_1)
  } else {
    biological_metadata <- organism_table_filled |>
      tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
      tidytable::rename(organism_name = organism)
  }
  rm(organism_table_filled, organism_table_missing)

  if (is.null(taxon)) {
    if (!extension) {
      metadata_table <- metadata_table |>
        tidytable::mutate(
          filename = stri_replace_all_fixed(
            str = filename,
            pattern = ".mzML",
            replacement = "",
            vectorize_all = FALSE
          )
        ) |>
        tidytable::mutate(
          filename = stri_replace_all_fixed(
            str = filename,
            pattern = ".mzxML",
            replacement = "",
            vectorize_all = FALSE
          )
        )
    }
  }
  collapse_cols <- c(
    "sample_organism_name",
    "sample_organism_01_domain",
    "sample_organism_02_kingdom",
    "sample_organism_03_phylum",
    "sample_organism_04_class",
    "sample_organism_05_order",
    "sample_organism_06_family",
    "sample_organism_07_tribe",
    "sample_organism_08_genus",
    "sample_organism_09_species",
    "sample_organism_10_varietas"
  )

  expected_cols <- c(
    "feature_id",
    "organismOriginal",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "organism_taxonomy_04class",
    "organism_taxonomy_05order",
    "organism_taxonomy_06family",
    "organism_taxonomy_07tribe",
    "organism_taxonomy_08genus",
    "organism_taxonomy_09species",
    "organism_taxonomy_10varietas"
  )

  if (
    !is.null(taxon) &&
      "feature_id" %in% names(feature_table_0) &&
      nrow(feature_table_0) > 0L &&
      nrow(biological_metadata) > 0L &&
      all(
        setdiff(expected_cols, c("feature_id", "organismOriginal")) %in%
          names(biological_metadata)
      )
  ) {
    u_fid <- as.vector(unique(as.character(feature_table_0$feature_id)))
    rm(feature_table_0, metadata_table)

    one_row <- biological_metadata |>
      tidytable::distinct(
        organism_name,
        organism_taxonomy_01domain,
        organism_taxonomy_02kingdom,
        organism_taxonomy_03phylum,
        organism_taxonomy_04class,
        organism_taxonomy_05order,
        organism_taxonomy_06family,
        organism_taxonomy_07tribe,
        organism_taxonomy_08genus,
        organism_taxonomy_09species,
        organism_taxonomy_10varietas
      ) |>
      tidytable::mutate(feature_id = "1") |>
      tidytable::select(
        feature_id,
        sample_organism_name = organism_name,
        sample_organism_01_domain = organism_taxonomy_01domain,
        sample_organism_02_kingdom = organism_taxonomy_02kingdom,
        sample_organism_03_phylum = organism_taxonomy_03phylum,
        sample_organism_04_class = organism_taxonomy_04class,
        sample_organism_05_order = organism_taxonomy_05order,
        sample_organism_06_family = organism_taxonomy_06family,
        sample_organism_07_tribe = organism_taxonomy_07tribe,
        sample_organism_08_genus = organism_taxonomy_08genus,
        sample_organism_09_species = organism_taxonomy_09species,
        sample_organism_10_varietas = organism_taxonomy_10varietas
      ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::everything(),
        .fns = as.character
      )) |>
      tidytable::group_by(feature_id) |>
      clean_collapse(cols = collapse_cols) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.character),
        .fns = ~ tidytable::replace_na(.x = .x, replace = "ND")
      ))
    rm(biological_metadata)

    n_fid <- length(u_fid)
    out_list <- c(
      list(feature_id = u_fid),
      lapply(
        as.list(one_row)[collapse_cols],
        function(col_v) rep(col_v[[1L]], n_fid)
      )
    )
    taxed_features_table <- tidytable::as_tidytable(out_list)
  } else {
    if (!is.null(taxon)) {
      metadata_table_joined <- tidytable::inner_join(
        x = feature_table_0 |>
          tidytable::mutate(join = "x"),
        y = biological_metadata |>
          tidytable::select(organismOriginal = organism_name) |>
          tidytable::mutate(join = "x")
      ) |>
        tidytable::select(-join)
    } else {
      if (!"sample" %in% names(feature_table_0)) {
        cli::cli_abort(
          c(
            "Features file does not contain the 'sample' column",
            "i" = sprintf(
              "Found columns: %s",
              paste(names(feature_table_0), collapse = ", ")
            ),
            "x" = "Features file must have a 'sample' column for metadata mapping"
          ),
          class = c("tima_validation_error", "tima_error"),
          call = NULL
        )
      }

      if (!name_filename %in% names(metadata_table)) {
        cli::cli_abort(
          c(
            "Metadata file does not contain the filename column",
            "i" = sprintf(
              "Expected column '%s' as specified in 'names$filename' parameter",
              name_filename
            ),
            "x" = sprintf(
              "Found columns: %s",
              paste(names(metadata_table), collapse = ", ")
            ),
            "!" = "Please ensure the 'names$filename' parameter matches a column in your metadata file"
          ),
          class = c("tima_validation_error", "tima_error"),
          call = NULL
        )
      }

      has_int_col <- "intensity" %in%
        names(feature_table_0) &&
        !is.null(min_rel_intensity) &&
        is.numeric(min_rel_intensity) &&
        length(min_rel_intensity) == 1L &&
        !is.na(min_rel_intensity) &&
        min_rel_intensity > 0
      int_vals <- if (has_int_col) {
        suppressWarnings(as.double(feature_table_0$intensity))
      } else {
        NULL
      }
      use_int_filter <- has_int_col &&
        any(!is.na(int_vals) & is.finite(int_vals) & int_vals > 0)

      feat_cols_keep <- intersect(
        c("feature_id", "sample"),
        names(feature_table_0)
      )
      meta_cols_keep <- unique(c(name_filename, colname))
      meta_distinct <- .fast_distinct_with_key_guard(
        tidytable::as_tidytable(as.list(metadata_table)[meta_cols_keep]),
        meta_cols_keep
      )
      meta_selected <- meta_distinct |>
        tidytable::select(
          organismOriginal = tidyselect::all_of(x = colname),
          tidyselect::everything()
        )
      meta_expanded <- if (
        any(grepl("|", meta_selected$organismOriginal, fixed = TRUE))
      ) {
        meta_selected |>
          tidytable::separate_rows(organismOriginal, sep = "\\|")
      } else {
        meta_selected
      }
      rm(meta_distinct, meta_selected)

      if (use_int_filter) {
        # Aggregate positive sample intensity per (feature_id, sample) before
        # mapping to organisms, then retain organisms with relative intensity >=
        # min_rel_intensity (always keeping the highest-intensity organism per feature).
        f_raw <- as.character(feature_table_0$feature_id)
        s_raw <- as.character(feature_table_0$sample)
        w_raw <- ifelse(
          !is.na(int_vals) & is.finite(int_vals) & int_vals > 0,
          int_vals,
          0
        )
        fs_dt <- data.table::data.table(
          feature_id = f_raw,
          sample = s_raw,
          intensity = w_raw
        )[, list(intensity = sum(intensity)), by = c("feature_id", "sample")]
        feat_distinct <- tidytable::as_tidytable(fs_dt[, c(
          "feature_id",
          "sample"
        )])
        fs_int <- fs_dt$intensity
        rm(fs_dt, f_raw, s_raw, w_raw)
      } else {
        feat_distinct <- .fast_distinct_with_key_guard(
          tidytable::as_tidytable(as.list(feature_table_0)[feat_cols_keep]),
          feat_cols_keep
        )
        fs_int <- NULL
      }
      if (
        name_filename %in%
          names(meta_expanded) &&
          anyDuplicated(meta_expanded[[name_filename]]) == 0L
      ) {
        fn_key <- meta_expanded[[name_filename]]
        s_key <- feat_distinct$sample
        m_meta <- match(s_key, fn_key)
        metadata_table_joined <- tidytable::as_tidytable(as.list(feat_distinct))
        data.table::set(
          metadata_table_joined,
          j = "organismOriginal",
          value = meta_expanded$organismOriginal[m_meta]
        )
        if (use_int_filter) {
          data.table::set(
            metadata_table_joined,
            j = ".int_w",
            value = fs_int
          )
        }
        col_ord_j <- intersect(
          c("feature_id", "organismOriginal", "sample", ".int_w"),
          names(metadata_table_joined)
        )
        if (!identical(names(metadata_table_joined), col_ord_j)) {
          data.table::setcolorder(metadata_table_joined, col_ord_j)
        }
      } else {
        if (use_int_filter) {
          data.table::set(feat_distinct, j = ".int_w", value = fs_int)
        }
        metadata_table_joined <-
          tidytable::left_join(
            x = feat_distinct,
            y = meta_expanded,
            by = c("sample" = name_filename)
          ) |>
          tidytable::select(
            tidyselect::any_of("feature_id"),
            organismOriginal,
            tidyselect::everything()
          )
      }
      rm(meta_expanded, feat_distinct, fs_int)

      if (
        use_int_filter &&
          nrow(metadata_table_joined) > 0L &&
          ".int_w" %in% names(metadata_table_joined)
      ) {
        valid_org <- !is.na(metadata_table_joined$organismOriginal)
        if (any(valid_org)) {
          fo_keys <- paste(
            metadata_table_joined$feature_id[valid_org],
            metadata_table_joined$organismOriginal[valid_org],
            sep = "\r"
          )
          u_fo <- unique(fo_keys)
          fo_idx <- fastmatch::fmatch(fo_keys, u_fo)
          org_int <- as.numeric(
            rowsum(
              metadata_table_joined$.int_w[valid_org],
              fo_idx,
              reorder = FALSE,
              na.rm = TRUE
            )
          )
          u_feat <- sub("\r.*$", "", u_fo)
          feat_tot <- stats::ave(org_int, u_feat, FUN = sum)
          feat_max <- stats::ave(org_int, u_feat, FUN = max)
          keep_fo <- u_fo[
            feat_tot <= 0 |
              org_int >= feat_max |
              (org_int / feat_tot) >= min_rel_intensity
          ]
          row_keys <- paste(
            metadata_table_joined$feature_id,
            metadata_table_joined$organismOriginal,
            sep = "\r"
          )
          keep_mask <- !valid_org |
            !is.na(fastmatch::fmatch(row_keys, keep_fo))
          metadata_table_joined <- metadata_table_joined[which(keep_mask), ]
        }
        data.table::set(metadata_table_joined, j = ".int_w", value = NULL)
      }

      if (nrow(metadata_table_joined) == 0L) {
        log_warn(
          "No metadata rows matched features by sample name; exporting ND taxonomy for all features"
        )
        taxed_features_table <- .make_nd_taxa_table(feature_table_0$feature_id)
        log_complete(ctx, n_features = nrow(taxed_features_table))
        export_params(
          parameters = get_params(step = "prepare_taxa"),
          step = "prepare_taxa"
        )
        export_output(x = taxed_features_table, file = output)
        rm(taxed_features_table)
        return(output)
      }
    }
    rm(feature_table_0, metadata_table)

    avail_cols <- unique(c(
      names(metadata_table_joined),
      setdiff(names(biological_metadata), "organism_name")
    ))
    missing_select_cols <- setdiff(expected_cols, avail_cols)
    if (length(missing_select_cols) > 0L) {
      cli::cli_abort(
        c(
          "Failed to prepare taxa: missing expected columns after joining with OTT taxonomy",
          "x" = sprintf(
            "Missing columns: %s",
            paste(missing_select_cols, collapse = ", ")
          ),
          "i" = sprintf(
            "Available columns: %s",
            paste(avail_cols, collapse = ", ")
          ),
          "!" = "This may indicate: (1) empty organism list, (2) mismatched organism names, or (3) corrupted OTT file"
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    if (
      nrow(metadata_table_joined) > 256L &&
        "feature_id" %in% names(metadata_table_joined)
    ) {
      org_raw <- as.character(metadata_table_joined$organismOriginal)
      u_org_raw <- unique(org_raw)
      u_org_trim <- trimws(u_org_raw)
      org_trimmed <- if (identical(u_org_trim, u_org_raw)) {
        org_raw
      } else {
        u_org_trim[match(org_raw, u_org_raw)]
      }
      feat_org <- tidytable::tidytable(
        feature_id = metadata_table_joined$feature_id,
        organismOriginal = org_trimmed
      ) |>
        .fast_distinct_with_key_guard(c("feature_id", "organismOriginal"))
      rm(metadata_table_joined, org_raw, org_trimmed)

      f_vec <- as.character(feat_org$feature_id)
      o_vec <- as.character(feat_org$organismOriginal)
      o_key <- o_vec
      na_o <- is.na(o_key)
      if (any(na_o)) {
        o_key[na_o] <- "<NA>"
      }
      u_f <- as.vector(unique(f_vec))
      n_uf <- length(u_f)

      if (n_uf == length(f_vec)) {
        combo_sig <- o_key
        u_combos <- as.vector(unique(combo_sig))
        combo_id_per_f <- as.vector(fastmatch::fmatch(combo_sig, u_combos))
        first_f_for_combo <- match(seq_along(u_combos), combo_id_per_f)
        rep_dt <- tidytable::tidytable(
          feature_id = as.character(seq_along(u_combos)),
          organismOriginal = o_vec[first_f_for_combo]
        )
      } else {
        f_id <- as.vector(fastmatch::fmatch(f_vec, u_f))
        attr(u_f, ".match.hash") <- NULL
        if (is.unsorted(f_id)) {
          ord <- order(f_id, method = "radix")
          f_id <- f_id[ord]
          o_key <- o_key[ord]
          o_vec <- o_vec[ord]
        }
        counts <- tabulate(f_id, nbins = n_uf)
        ends <- cumsum(counts)
        starts <- ends - counts + 1L

        combo_sig <- character(n_uf)
        idx1 <- which(counts == 1L)
        if (length(idx1) > 0L) {
          combo_sig[idx1] <- o_key[starts[idx1]]
        }
        u_lens <- sort(unique(counts[counts > 1L]))
        combo_sig <- Reduce(
          function(cur_sig, L) {
            idx_L <- which(counts == L)
            s_L <- starts[idx_L]
            cols <- lapply(0L:(L - 1L), function(k) o_key[s_L + k])
            cur_sig[idx_L] <- do.call(paste, c(cols, list(sep = "\r")))
            cur_sig
          },
          u_lens,
          init = combo_sig
        )

        u_combos <- as.vector(unique(combo_sig))
        combo_id_per_f <- as.vector(fastmatch::fmatch(combo_sig, u_combos))
        first_f_for_combo <- match(seq_along(u_combos), combo_id_per_f)
        rep_lens <- counts[first_f_for_combo]
        rep_idx <- sequence(rep_lens, from = starts[first_f_for_combo])
        rep_dt <- tidytable::tidytable(
          feature_id = as.character(rep.int(seq_along(u_combos), rep_lens)),
          organismOriginal = o_vec[rep_idx]
        )
      }

      joined_rep <- rep_dt |>
        tidytable::left_join(
          y = biological_metadata,
          by = c("organismOriginal" = "organism_name")
        ) |>
        tidytable::distinct() |>
        tidytable::select(
          feature_id,
          sample_organism_name = organismOriginal,
          sample_organism_01_domain = organism_taxonomy_01domain,
          sample_organism_02_kingdom = organism_taxonomy_02kingdom,
          sample_organism_03_phylum = organism_taxonomy_03phylum,
          sample_organism_04_class = organism_taxonomy_04class,
          sample_organism_05_order = organism_taxonomy_05order,
          sample_organism_06_family = organism_taxonomy_06family,
          sample_organism_07_tribe = organism_taxonomy_07tribe,
          sample_organism_08_genus = organism_taxonomy_08genus,
          sample_organism_09_species = organism_taxonomy_09species,
          sample_organism_10_varietas = organism_taxonomy_10varietas
        ) |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::everything(),
          .fns = as.character
        )) |>
        tidytable::group_by(feature_id) |>
        clean_collapse(cols = collapse_cols) |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::where(fn = is.character),
          .fns = ~ tidytable::replace_na(.x = .x, replace = "ND")
        ))
      rm(biological_metadata, feat_org, rep_dt)

      m_rep <- as.vector(fastmatch::fmatch(
        as.character(combo_id_per_f),
        joined_rep$feature_id
      ))
      taxed_features_table <- joined_rep[m_rep, ]
      data.table::set(taxed_features_table, j = "feature_id", value = u_f)
    } else {
      joined_data <- metadata_table_joined |>
        tidytable::mutate(organismOriginal = trimws(organismOriginal)) |>
        tidytable::left_join(
          y = biological_metadata,
          by = c("organismOriginal" = "organism_name")
        ) |>
        tidytable::distinct()

      taxed_features_table <- joined_data |>
        tidytable::select(
          feature_id,
          sample_organism_name = organismOriginal,
          sample_organism_01_domain = organism_taxonomy_01domain,
          sample_organism_02_kingdom = organism_taxonomy_02kingdom,
          sample_organism_03_phylum = organism_taxonomy_03phylum,
          sample_organism_04_class = organism_taxonomy_04class,
          sample_organism_05_order = organism_taxonomy_05order,
          sample_organism_06_family = organism_taxonomy_06family,
          sample_organism_07_tribe = organism_taxonomy_07tribe,
          sample_organism_08_genus = organism_taxonomy_08genus,
          sample_organism_09_species = organism_taxonomy_09species,
          sample_organism_10_varietas = organism_taxonomy_10varietas
        ) |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::everything(),
          .fns = as.character
        )) |>
        tidytable::group_by(feature_id) |>
        clean_collapse(cols = collapse_cols) |>
        tidytable::mutate(tidytable::across(
          .cols = tidyselect::where(fn = is.character),
          .fns = ~ tidytable::replace_na(.x = .x, replace = "ND")
        ))
      rm(biological_metadata, metadata_table_joined, joined_data)
    }
  }

  log_complete(ctx, n_features = nrow(taxed_features_table))

  export_params(
    parameters = get_params(step = "prepare_taxa"),
    step = "prepare_taxa"
  )
  export_output(x = taxed_features_table, file = output)
  rm(taxed_features_table)
  output
}
