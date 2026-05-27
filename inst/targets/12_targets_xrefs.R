# Cross-reference targets section.

targets_section_xrefs <- function() {
  list(tar_target(
    name = lib_xrefs,
    command = {
      get_compounds_xrefs(output = paths$data$interim$xrefs$compounds)
    },
    format = "file"
  ))
}
