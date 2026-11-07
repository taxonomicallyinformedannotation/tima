#' @title Prepare libraries of structure organism pairs LOTUS
#'
#' @description This function prepares the LOTUS.
#'     It standardizes columns, extracts 2D InChIKeys, rounds numeric values,
#'     and removes duplicates.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include safe_fread.R
#' @include select_sop_columns.R
#' @include logs_utils.R
#' @include export_output.R
#'
#' @param input [character] Path to the raw LOTUS data file
#' @param output [character] Path for the prepared output file
#'
#' @return Character string path to the prepared structure-organism pairs
#'     library file
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_lotus()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_lotus <- function(
  input = get_params(
    step = "prepare_libraries_sop_lotus"
  )$files$libraries$sop$raw$lotus,
  output = get_params(
    step = "prepare_libraries_sop_lotus"
  )$files$libraries$sop$prepared$lotus
) {
  ctx <- log_operation("prepare_libraries_sop_lotus", input = input)

  if (file.exists(input)) {
    file_size <- file.info(input)$size
    log_debug(
      "Loading LOTUS database: %s (%s)",
      basename(input),
      format_bytes(file_size)
    )

    lotus_raw <- safe_fread(
      file = input,
      file_type = "LOTUS structure-organism pairs",
      na.strings = c("", "NA"),
      colClasses = "character"
    )
    ## InChIKey connectivity layer is recomputed from SMILES
    ## via process_smiles() in split_tables_sop().
    data.table::set(
      lotus_raw,
      j = "structure_inchikey_2D",
      value = rep.int(NA_character_, nrow(lotus_raw))
    )
    if ("structure_nameTraditional" %in% names(lotus_raw)) {
      data.table::setnames(
        lotus_raw,
        old = "structure_nameTraditional",
        new = "structure_name"
      )
    }
    lotus_prepared <- lotus_raw |>
      select_sop_columns() |>
      round_reals() |>
      tidytable::distinct()
    rm(lotus_raw)

    log_complete(ctx, n_pairs = nrow(lotus_prepared))
  } else {
    log_warn(
      "LOTUS database not found at: %s",
      input
    )
    log_warn("Returning empty placeholder file")
    lotus_prepared <- fake_sop_columns()
  }

  # Export prepared data
  export_output(x = lotus_prepared, file = output)
  rm(lotus_prepared)

  output
}
