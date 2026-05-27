# Pipeline graph body composed from section modules.

source("inst/targets/00_targets_setup.R")
source("inst/targets/01_targets_architecture.R")
source("inst/targets/02_targets_parameters.R")
source("inst/targets/10_targets_inputs.R")
source("inst/targets/11_targets_libraries.R")
source("inst/targets/12_targets_xrefs.R")
source("inst/targets/13_targets_annotations.R")
source("inst/targets/14_targets_features.R")
source("inst/targets/15_targets_benchmark.R")
source("inst/targets/16_targets_filtering.R")
source("inst/targets/17_targets_weighting.R")
source("inst/targets/20_targets_export_mztab.R")
source("inst/targets/30_targets_benchmark.R")

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
