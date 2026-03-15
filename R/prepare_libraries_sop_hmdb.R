#' @title Prepare libraries of structure organism pairs HMDB
#'
#' @description This function prepares HMDB (Human Metabolome Database)
#'     structure-organism pairs by parsing SDF files, extracting metadata,
#'     and formatting for TIMA annotation workflows.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#' @include prepare_libraries_sop_hmdb_like.R
#'
#' @param input [character] Character string path to HMDB SDF zip file
#' @param output [character] Character string path for prepared HMDB library output
#'
#' @return Character string path to prepared HMDB structure-organism pairs
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_hmdb()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_hmdb <- function(
  input = get_params(
    step = "prepare_libraries_sop_hmdb"
  )$files$libraries$sop$raw$hmdb,
  output = get_params(
    step = "prepare_libraries_sop_hmdb"
  )$files$libraries$sop$prepared$hmdb
) {
  prepare_libraries_sop_hmdb_like(
    input = input,
    output = output,
    source_name = "HMDB"
  )
}
