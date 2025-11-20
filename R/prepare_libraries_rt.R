#' @title Prepare libraries of retention times
#'
#' @description This function prepares retention time libraries by combining
#'     experimental and in silico predicted retention times from multiple sources
#'     (MGF files, CSV files). It standardizes retention time units, validates
#'     structures, and creates both RT libraries and pseudo structure-organism
#'     pairs for RT-based annotation.
#'
#' @include get_params.R
#' @include import_spectra.R
#'
#' @param mgf_exp Character vector of paths to MGF files with experimental RT
#' @param mgf_is Character vector of paths to MGF files with in silico predicted RT
#' @param temp_exp Character vector of paths to CSV files with experimental RT
#' @param temp_is Character vector of paths to CSV files with in silico predicted RT
#' @param output_rt Character string path for prepared RT library output
#' @param output_sop Character string path for pseudo SOP output
#' @param col_ik Character string name of InChIKey column in MGF
#' @param col_na Character string name of chompound name column in MGF
#' @param col_rt Character string name of retention time column in MGF
#' @param col_sm Character string name of SMILES column in MGF
#' @param name_inchikey Character string name of InChIKey column in CSV
#' @param name_name Character string name of compound name column in CSV
#' @param name_rt Character string name of retention time column in CSV
#' @param name_smiles Character string name of SMILES column in CSV
#' @param unit_rt Character string RT unit: "seconds" or "minutes"
#'
#' @return Character string path to the prepared retention time library
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_rt()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_rt <- function(
  mgf_exp = get_params(
    step = "prepare_libraries_rt"
  )$files$libraries$temporal$exp$mgf,
  mgf_is = get_params(
    step = "prepare_libraries_rt"
  )$files$libraries$temporal$is$mgf,
  temp_exp = get_params(
    step = "prepare_libraries_rt"
  )$files$libraries$temporal$exp$csv,
  temp_is = get_params(
    step = "prepare_libraries_rt"
  )$files$libraries$temporal$is$csv,
  output_rt = get_params(
    step = "prepare_libraries_rt"
  )$files$libraries$temporal$prepared,
  output_sop = get_params(
    step = "prepare_libraries_rt"
  )$files$libraries$sop$prepared$rt,
  col_ik = get_params(step = "prepare_libraries_rt")$names$mgf$inchikey,
  col_na = get_params(step = "prepare_libraries_rt")$names$mgf$name,
  col_rt = get_params(step = "prepare_libraries_rt")$names$mgf$retention_time,
  col_sm = get_params(step = "prepare_libraries_rt")$names$mgf$smiles,
  name_inchikey = get_params(step = "prepare_libraries_rt")$names$inchikey,
  name_name = get_params(step = "prepare_libraries_rt")$names$compound_name,
  name_rt = get_params(step = "prepare_libraries_rt")$names$rt$library,
  name_smiles = get_params(step = "prepare_libraries_rt")$names$smiles,
  unit_rt = get_params(step = "prepare_libraries_rt")$units$rt
) {
  # Validate core arguments and normalize outputs ----
  # Validate RT unit
  if (!unit_rt %in% c("seconds", "minutes")) {
    stop("unit_rt must be 'seconds' or 'minutes', got: ", unit_rt)
  }

  # Validate output paths and create directories
  if (!is.character(output_rt) || length(output_rt) != 1L) {
    stop("output_rt must be a single character string")
  }
  dir.create(dirname(output_rt), showWarnings = FALSE, recursive = TRUE)

  if (!is.character(output_sop) || length(output_sop) != 1L) {
    stop("output_sop must be a single character string")
  }
  dir.create(dirname(output_sop), showWarnings = FALSE, recursive = TRUE)

  logger::log_info("Preparing retention time libraries")
  logger::log_debug("RT unit: {unit_rt}")

  ## default transforms from `Spectra`
  if (!is.na(col_rt) && col_rt == "RTINSECONDS") {
    col_rt <- "rtime"
  }
  if (!is.na(col_sm) && col_sm == "SMILES") {
    col_sm <- "smiles"
  }

  # Normalize input vectors ----
  .normalize_input_vec <- function(x) {
    # Accept NULL or character vectors, return NULL if empty/NA/blank
    # Handle list() from YAML params (when fields are empty)
    if (is.null(x) || (is.list(x) && length(x) == 0L)) {
      return(NULL)
    }
    # If it's a list with elements, try to unlist
    if (is.list(x)) {
      x <- unlist(x, use.names = FALSE)
    }
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(trimws(x))]
    if (length(x) == 0L) NULL else x
  }

  mgf_exp <- .normalize_input_vec(mgf_exp)
  mgf_is <- .normalize_input_vec(mgf_is)
  temp_exp <- .normalize_input_vec(temp_exp)
  temp_is <- .normalize_input_vec(temp_is)

  # Readers ----
  rts_from_mgf <- function(mgf) {
    # logger::log_trace("Importing spectra")
    spectra <- mgf |> purrr::map(.f = import_spectra)
    # logger::log_trace("Extracting retention times")
    rts <- spectra |>
      # TODO
      purrr::map(.f = function(x) {
        x@backend@spectraData |>
          data.frame() |>
          tidytable::as_tidytable()
      }) |>
      tidytable::bind_rows() |>
      tidytable::select(tidyselect::any_of(c(
        rt = col_rt,
        inchikey = col_ik,
        smiles = col_sm,
        structure_name = col_na
      )))
    rts
  }

  rts_from_tab <- function(tab) {
    # logger::log_trace("Importing file")
    tab |>
      purrr::map(.f = tidytable::fread) |>
      tidytable::bind_rows() |>
      tidytable::select(tidyselect::any_of(c(
        rt = name_rt,
        inchikey = name_inchikey,
        smiles = name_smiles,
        structure_name = name_name
      )))
  }

  # Polishing utilities ----
  polish_df <- function(df, type = "experimental", unit = unit_rt) {
    # Ensure columns exist even if missing from sources
    if (!"inchikey" %in% colnames(df)) {
      df$inchikey <- NA_character_
    }
    if (!"smiles" %in% colnames(df)) {
      df$smiles <- NA_character_
    }
    if (!"structure_name" %in% colnames(df)) {
      df$structure_name <- NA_character_
    }

    df_tmp <- df |>
      data.frame() |>
      tidytable::mutate(type = type) |>
      tidytable::mutate(rt = as.numeric(rt)) |>
      tidytable::filter(!is.na(rt)) |>
      tidytable::filter(!is.na(smiles)) |>
      tidytable::distinct()

    # Normalize RT to minutes using centralized utility
    df_tmp <- df_tmp |>
      tidytable::mutate(
        rt = normalize_rt_to_minutes(rt, unit = unit, quiet = TRUE)
      )

    df_tmp
  }

  complete_df <- function(df) {
    df_full <- df |>
      tidytable::filter(!is.na(inchikey) & inchikey != "")
    df_missing <- df |>
      tidytable::filter(is.na(inchikey) | inchikey == "")
    missing_n <- nrow(df_missing)

    if (missing_n > 0L) {
      logger::log_warn(
        "There are ",
        missing_n,
        " entries without InChIKey.",
        " We would recommend you adding them but will try completing.",
        " We will query them on the fly, this might take some time."
      )
    }

    smiles <- unique(df_missing$smiles)

    ## TODO replace with process_smiles
    get_inchikey <- function(smiles, toolkit = "rdkit") {
      url <- paste0(
        "https://api.naturalproducts.net/latest/convert/inchikey?smiles=",
        utils::URLencode(smiles),
        "&toolkit=",
        toolkit
      )
      tryCatch(expr = jsonlite::fromJSON(txt = url), error = function(e) {
        NA_character_
      })
    }

    inchikey <- purrr::map(.x = smiles, .f = get_inchikey) |> as.character()
    df_missing <- df_missing |>
      tidytable::select(-inchikey) |>
      tidytable::left_join(tidytable::tidytable(smiles, inchikey))

    df_completed <- df_full |>
      tidytable::bind_rows(df_missing) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.character),
        .fns = function(x) tidytable::na_if(x, "NA")
      )) |>
      tidytable::select(
        rt,
        structure_smiles = smiles,
        structure_inchikey = inchikey,
        structure_name,
        type
      ) |>
      tidytable::distinct()

    missing_n2 <- df_completed |>
      tidytable::filter(is.na(structure_inchikey)) |>
      nrow()

    if (missing_n2 > 0L) {
      logger::log_warn(
        "There were still ",
        missing_n2,
        " entries for which no InChIKey could not be found in the end."
      )
    }

    df_completed
  }

  empty_df <- tidytable::tidytable(
    rt = NA_real_,
    structure_smiles = NA_character_,
    structure_inchikey = NA_character_,
    structure_name = NA_character_,
    type = NA_character_
  )

  # Build RT tables from sources ----
  # from mgf
  rts_exp_1 <- if (!is.null(mgf_exp)) {
    mgf_exp |>
      rts_from_mgf() |>
      polish_df() |>
      complete_df()
  } else {
    empty_df
  }
  rts_is_1 <- if (!is.null(mgf_is)) {
    mgf_is |>
      rts_from_mgf() |>
      polish_df(type = "predicted") |>
      complete_df()
  } else {
    empty_df
  }

  # from csv
  rts_exp_2 <- if (!is.null(temp_exp)) {
    temp_exp |>
      rts_from_tab() |>
      polish_df() |>
      complete_df()
  } else {
    empty_df
  }
  rts_is_2 <- if (!is.null(temp_is)) {
    temp_is |>
      rts_from_tab() |>
      polish_df(type = "predicted") |>
      complete_df()
  } else {
    empty_df
  }

  df_rts <- tidytable::bind_rows(rts_exp_1, rts_exp_2, rts_is_1, rts_is_2) |>
    tidytable::filter(!is.na(as.numeric(rt))) |>
    tidytable::filter(!is.na((structure_inchikey)))

  # Free memory
  rm(rts_exp_1, rts_exp_2, rts_is_1, rts_is_2)

  sop <- df_rts |>
    tidytable::select(structure_smiles, structure_inchikey, structure_name) |>
    tidytable::distinct() |>
    tidytable::mutate(
      structure_inchikey_connectivity_layer = stringi::stri_sub(
        str = structure_inchikey,
        from = 1L,
        to = 14L
      ),
      organism_name = NA_character_
    )

  rts <- df_rts |>
    tidytable::select(-structure_smiles) |>
    tidytable::distinct() |>
    tidytable::mutate(
      candidate_structure_inchikey_connectivity_layer = gsub(
        pattern = "-.*",
        replacement = "",
        x = structure_inchikey,
        perl = TRUE
      )
    ) |>
    ## TODO REMINDER FOR NOW
    tidytable::distinct(
      rt,
      candidate_structure_inchikey_connectivity_layer,
      type
    )

  rm(df_rts)

  if (nrow(rts) == 0) {
    logger::log_warn(
      "No retention time library found, returning empty retention time and sop tables."
    )
    sop <- tidytable::tidytable(
      structure_name = NA_character_,
      structure_smiles = NA_character_,
      structure_inchikey = NA_character_,
      structure_inchikey_connectivity_layer = NA_character_,
      organism_name = NA_character_
    )
    rts <- tidytable::tidytable(
      rt = NA_real_,
      candidate_structure_inchikey_connectivity_layer = NA_character_,
      type = NA_character_
    )
  }

  export_params(
    parameters = get_params(step = "prepare_libraries_rt"),
    step = "prepare_libraries_rt"
  )
  export_output(x = rts, file = output_rt)
  # Write SOP with organism_name encoded as 'NA' to roundtrip to NA on read
  sop_to_write <- sop |>
    tidytable::mutate(
      organism_name = ifelse(is.na(organism_name), "NA", organism_name)
    )
  export_output(x = sop_to_write, file = output_sop)
  rm(rts, sop)
  return(c("rt" = output_rt, "sop" = output_sop))
}
