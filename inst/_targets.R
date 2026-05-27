# Pipeline graph body composed from section modules.

# Detect this script's own directory so section paths are portable
# regardless of working directory or installation location.
.inst_dir <- local({
  frames <- sys.frames()
  ofiles <- Filter(Negate(is.null), lapply(frames, `[[`, "ofile"))
  if (length(ofiles) > 0L) {
    dirname(normalizePath(ofiles[[length(ofiles)]], mustWork = FALSE))
  } else {
    system.file(package = "tima")
  }
})
.targets_dir <- file.path(.inst_dir, "targets")

source(file.path(.targets_dir, "00_targets_setup.R"))
source(file.path(.targets_dir, "01_targets_architecture.R"))
source(file.path(.targets_dir, "02_targets_parameters.R"))
source(file.path(.targets_dir, "10_targets_inputs.R"))
source(file.path(.targets_dir, "11_targets_libraries.R"))
source(file.path(.targets_dir, "12_targets_xrefs.R"))
source(file.path(.targets_dir, "13_targets_annotations.R"))
source(file.path(.targets_dir, "14_targets_features.R"))
source(file.path(.targets_dir, "15_targets_benchmark.R"))
source(file.path(.targets_dir, "16_targets_filtering.R"))
source(file.path(.targets_dir, "17_targets_weighting.R"))
source(file.path(.targets_dir, "20_targets_export_mztab.R"))
source(file.path(.targets_dir, "30_targets_benchmark.R"))

list(
  targets_section_architecture(),
  targets_section_parameters(),
  targets_section_inputs(),
  targets_section_libraries(),
  targets_section_xrefs(),
  targets_section_annotations(),
  targets_section_features(),
  targets_section_taxa(),
  targets_section_filtering(),
  targets_section_weighting(),
  targets_section_export_mztab(),
  targets_section_benchmark()
)
