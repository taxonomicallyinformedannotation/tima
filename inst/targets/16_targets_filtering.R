# Filtering targets section.

targets_section_filtering <- function() {
  tar_target(
    name = ann_fil,
    command = {
      filter_annotations(
        annotations = c(
          gnps = ann_spe_exp_gnp_pre,
          mzmine = ann_spe_exp_mzm_pre,
          mztab = ann_spe_exp_mzt_pre,
          spectral = ann_spe_pre,
          sirius = ann_sir_pre_str,
          ms1 = ann_ms1_pre_ann
        ),
        features = fea_pre,
        rts = lib_rt_rts,
        output = par_fil_ann$files$annotations$filtered,
        tolerance_rt = par_fil_ann$ms$tolerances$rt$library
      )
    },
    format = "file"
  )
}
