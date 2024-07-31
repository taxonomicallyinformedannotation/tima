start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script performs",
  crayon::green("filters annotations"),
  "based on",
  crayon::blue("retention time matching")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("^ann_fil"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
