#' @title Prepare libraries of structure organism pairs LOTUS
#'
#' @description This function prepares the LOTUS (LOng-lasting, cUraTed collection
#'     of cOnnectivity daTa for natural products) structure-organism pairs database.
#'     It standardizes columns, extracts 2D InChIKeys, rounds numeric values,
#'     and removes duplicates.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include safe_fread.R
#' @include select_sop_columns.R
#' @include logs_utils.R
#'
#' @param input Character string path to the raw LOTUS data file
#' @param output Character string path for the prepared output file
#'
#' @return Character string path to the prepared structure-organism pairs
#'     library file
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

  # Process LOTUS data if available
  if (file.exists(input)) {
    file_size <- file.info(input)$size
    log_debug(
      "Loading LOTUS database: %s (%s)",
      basename(input),
      format_bytes(file_size)
    )

    lotus_prepared <- input |>
      safe_fread(
        file_type = "LOTUS structure-organism pairs",
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::mutate(
        # Extract 2D InChIKey (first 14 characters = connectivity layer)
        structure_inchikey_2D = stringi::stri_sub(
          str = structure_inchikey,
          from = 1L,
          to = 14L
        )
      ) |>
      tidytable::rename(structure_name = structure_nameTraditional) |>
      select_sop_columns() |>
      round_reals() |>
      tidytable::distinct()

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
  # log_trace("Exporting prepared LOTUS data")
  export_output(x = lotus_prepared, file = output)

  return(output)
}
