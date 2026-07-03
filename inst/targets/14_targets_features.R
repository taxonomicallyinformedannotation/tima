# Features targets section.

targets_section_features <- function() {
  list(
    tar_target(
      name = fea_edg_spe,
      command = {
        create_edges_spectra(
          input = input_spectra,
          output = par_cre_edg_spe$files$networks$spectral$edges$raw$spectral,
          name_source = par_cre_edg_spe$names$source,
          name_target = par_cre_edg_spe$names$target,
          method = par_cre_edg_spe$similarities$methods$edges,
          ppm = par_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = par_cre_edg_spe$ms$tolerances$mass$dalton$ms2
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_com,
      command = {
        create_components(
          input = fea_edg_pre,
          output = par_cre_com$files$networks$spectral$components$raw
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_edg_pre,
      command = {
        prepare_features_edges(
          input = c(ms1 = ann_ms1_pre_edg, spectral = fea_edg_spe),
          output = par_pre_fea_edg$files$networks$spectral$edges$prepared,
          name_source = par_pre_fea_edg$names$source,
          name_target = par_pre_fea_edg$names$target
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_com_pre,
      command = {
        prepare_features_components(
          input = fea_com,
          output = par_pre_fea_com$files$networks$spectral$components$prepared
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_pre,
      command = {
        prepare_features_tables(
          features = input_features,
          output = par_pre_fea_tab$files$features$prepared,
          candidates = par_pre_fea_tab$annotations$candidates$samples,
          name_features = par_pre_fea_tab$names$features,
          name_rt = par_pre_fea_tab$names$rt$features,
          name_mz = par_pre_fea_tab$names$precursor
        )
      },
      format = "file"
    )
  )
}
