start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green(
    "prepares a library of retention times",
    "from MGF or tabular data. \n"
  )
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

targets::tar_make(names = matches("lib_rt"), script = system.file("/pipelines/_targets.R", "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
