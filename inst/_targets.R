# Pipeline graph body composed from section modules.

# Resolve the directory containing this file's sibling "targets/" folder.
# Works when sourced interactively, via targets, or from an installed package.
.targets_dir <- local({
  # 1. Inspect source() frames for an _targets.R path and validate candidates.
  frames <- sys.frames()
  ofiles <- Filter(Negate(is.null), lapply(frames, `[[`, "ofile"))
  for (f in rev(ofiles)) {
    candidate <- file.path(
      dirname(normalizePath(f, mustWork = FALSE)),
      "targets"
    )
    if (dir.exists(candidate)) return(candidate)
  }
  # 2. Installed package path.
  pkg <- system.file("targets", package = "tima")
  if (nzchar(pkg) && dir.exists(pkg)) {
    return(pkg)
  }
  # 3. Dev fallback when wd is repository root.
  file.path("inst", "targets")
})
.inst_dir <- dirname(.targets_dir)

source(file.path(.targets_dir, "00_targets_setup.R"))
source(file.path(.targets_dir, "01_targets_architecture.R"))
source(file.path(.targets_dir, "02_targets_parameters.R"))
source(file.path(.targets_dir, "10_targets_inputs.R"))
source(file.path(.targets_dir, "11_targets_libraries.R"))
source(file.path(.targets_dir, "12_targets_xrefs.R"))
source(file.path(.targets_dir, "13_targets_annotations.R"))
source(file.path(.targets_dir, "14_targets_features.R"))
source(file.path(.targets_dir, "15_targets_taxa.R"))
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
