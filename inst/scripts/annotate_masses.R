start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("annotates based on exact mass (MS1)")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ", crayon::blue("CH"), crayon::cyan("MS"), "\n")

targets::tar_make(names = matches("^ann_ms1_pre"), script = system.file("/pipelines/_targets.R", "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
