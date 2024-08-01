start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script",
  crayon::green("prepares parameters")
)
log_debug("Authors: ", crayon::green("AR"))
log_debug("Contributors: ...")

targets::tar_make(names = matches("par_"), script = system.file("pipelines/_targets.R", package = "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
