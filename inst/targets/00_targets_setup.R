# Shared targets setup for the pipeline definition.

library(targets)

tar_config_set(script = "inst/_targets.R")

tar_option_set(
  packages = "tima",
  memory = "transient",
  garbage_collection = TRUE
)

# Maps abbreviation -> YAML filename for programmatic target generation.
# Used to auto-generate default, user, and final parameter targets.
PARAM_STEPS <- c(
  ann_mas = "annotate_masses",
  ann_spe = "annotate_spectra",
  cre_com = "create_components",
  cre_edg_spe = "create_edges_spectra",
  fil_ann = "filter_annotations",
  exp_mzt = "write_mztab",
  pre_ann_gnp = "prepare_annotations_gnps",
  pre_ann_mzt = "prepare_annotations_mztab",
  pre_ann_mzm = "prepare_annotations_mzmine",
  pre_ann_sir = "prepare_annotations_sirius",
  pre_ann_spe = "prepare_annotations_spectra",
  pre_fea_com = "prepare_features_components",
  pre_fea_edg = "prepare_features_edges",
  pre_fea_tab = "prepare_features_tables",
  pre_lib_rt = "prepare_libraries_rt",
  pre_lib_sop_big = "prepare_libraries_sop_bigg",
  pre_lib_sop_clo = "prepare_libraries_sop_closed",
  pre_lib_sop_ecm = "prepare_libraries_sop_ecmdb",
  pre_lib_sop_hmd = "prepare_libraries_sop_hmdb",
  pre_lib_sop_lot = "prepare_libraries_sop_lotus",
  pre_lib_sop_mer = "prepare_libraries_sop_merged",
  pre_lib_spe = "prepare_libraries_spectra",
  pre_tax = "prepare_taxa",
  wei_ann = "weight_annotations"
)
