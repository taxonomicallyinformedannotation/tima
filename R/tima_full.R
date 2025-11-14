#' @title Tima Full
#'
#' @description This function runs the complete TIMA annotation workflow from
#'     start to finish, including all preparation, library loading, annotation,
#'     weighting, and output steps. Executes the full targets pipeline and
#'     saves logs with timestamps.
#'
#' @include go_to_cache.R
#'
#' @return NULL (invisibly). Runs complete workflow as side effect and saves
#'     timestamped log files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tima_full()
#' }
tima_full <- function() {
  start <- Sys.time()

  # Clean up previous log file
  if (file.exists("tima.log")) {
    file.remove("tima.log")
  }

  logger::log_info("Starting complete TIMA annotation workflow")
  logger::log_info("Authors: AR")
  logger::log_info("Contributors: PMA")

  # Navigate to cache directory
  go_to_cache()

  # Run the targets pipeline
  logger::log_info("Executing targets pipeline...")
  tryCatch(
    {
      targets::tar_make(names = tidyselect::matches("^ann_pre$"))
    },
    error = function(e) {
      logger::log_error("Pipeline failed: {conditionMessage(e)}")
      stop("TIMA full workflow failed: ", conditionMessage(e))
    }
  )

  end <- Sys.time()
  elapsed <- format(end - start)
  logger::log_success("Complete workflow finished in ", elapsed)

  # Archive log file with timestamp
  logs <- "tima.log"
  if (file.exists(logs)) {
    output_log <- paste0(
      "data/processed/",
      format(end, format = "%Y%m%d_%H%M%S"),
      "_",
      logs
    )

    success <- file.copy(from = logs, to = output_log)

    if (success) {
      file.remove(logs)
      logger::log_info("Log saved to: {output_log}")
    } else {
      logger::log_warn("Failed to archive log file")
    }
  }

  invisible(NULL)
}
