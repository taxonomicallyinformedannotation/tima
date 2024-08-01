start <- Sys.time()

require(
  package = "tima",
  quietly = TRUE
)

log_debug(
  "This script performs",
  crayon::green("taxonomically informed scoring"),
  "and followed by",
  crayon::blue("chemical consistency informed scoring")
)
log_debug(
  "Authors: ",
  crayon::green("AR"),
  ",",
  crayon::blue("PMA"),
  "\n"
)
log_debug("Contributors: ...")

targets::tar_make(names = matches("ann_pre$"), script = system.file("/pipelines/_targets.R", "tima"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
