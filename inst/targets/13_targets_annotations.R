# Annotations targets section.

targets_section_annotations <- function() {
  ann_spe_libs <- c(
    "is_nor",
    "is_wik",
    "exp_env",
    "exp_int",
    "exp_gnp",
    "exp_mb",
    "exp_mer",
    "exp_mul"
  )
  list(
    tar_target(
      name = ann_ms1_pre,
      command = {
        annotate_masses(
          features = fea_pre,
          library = lib_mer_key,
          output_annotations = par_ann_mas$files$annotations$prepared$structural$ms1,
          output_edges = par_ann_mas$files$networks$spectral$edges$raw$ms1,
          name_source = par_ann_mas$names$source,
          name_target = par_ann_mas$names$target,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc,
          adducts_list = par_ann_mas$ms$adducts,
          clusters_list = par_ann_mas$ms$clusters,
          solvents_list = par_ann_mas$ms$solvents,
          neutral_losses_list = par_ann_mas$ms$neutral_losses,
          ms_mode = par_ann_mas$ms$polarity,
          tolerance_ppm = par_ann_mas$ms$tolerances$mass$ppm$ms1,
          tolerance_rt = par_ann_mas$ms$tolerances$rt$adducts
        )
      },
      format = "file",
      garbage_collection = TRUE
    ),
    .tar_file_slice(
      "ann_ms1_pre",
      c(ann_ms1_pre_ann = 1L, ann_ms1_pre_edg = 2L)
    ),
    tar_target(
      name = ann_spe_exp_gnp_pre,
      command = {
        prepare_annotations_gnps(
          input = par_pre_ann_gnp$files$annotations$raw$spectral$gnps,
          output = par_pre_ann_gnp$files$annotations$prepared$structural$gnps,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file"
    ),
    tar_target(
      name = ann_spe_exp_mzm_pre,
      command = {
        prepare_annotations_mzmine(
          input = par_pre_ann_mzm$files$annotations$raw$spectral$mzmine,
          output = par_pre_ann_mzm$files$annotations$prepared$structural$mzmine,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file"
    ),
    tar_target(
      name = ann_spe_exp_mzt_pre,
      command = {
        prepare_annotations_mztab(
          input = par_pre_ann_mzt$files$mztab$raw,
          output = par_pre_ann_mzt$files$annotations$prepared$structural$mztab,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file"
    ),
    lapply(c("pos", "neg"), function(pol) {
      libs_expr <- as.call(c(
        quote(c),
        lapply(paste0("lib_spe_", ann_spe_libs, "_pre_", pol), as.symbol)
      ))
      tar_target_raw(
        name = paste0("ann_spe_", pol),
        command = substitute(
          annotate_spectra(
            input = input_spectra,
            ms1_annotations = ann_ms1_pre_ann,
            libraries = libs,
            polarity = pol,
            output = gsub(
              pattern = ".tsv.gz",
              replacement = suffix,
              x = par_ann_spe$files$annotations$raw$spectral$spectral,
              fixed = TRUE
            ),
            method = par_ann_spe$similarities$methods$annotations,
            threshold = par_ann_spe$similarities$thresholds$annotations,
            ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
            dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
            cutoff = par_ann_spe$ms$thresholds$ms2$intensity,
            approx = par_ann_spe$annotations$ms2approx
          ),
          list(
            libs = libs_expr,
            pol = pol,
            suffix = paste0("_", pol, ".tsv.gz")
          )
        ),
        format = "file",
        garbage_collection = TRUE
      )
    }),
    tar_target(
      name = ann_spe_pre,
      command = {
        prepare_annotations_spectra(
          input = c(ann_spe_neg, ann_spe_pos),
          output = par_pre_ann_spe$files$annotations$prepared$structural$spectral,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file",
      garbage_collection = TRUE
    ),
    tar_target(
      name = ann_sir_pre,
      command = {
        prepare_annotations_sirius(
          input_directory = par_pre_ann_sir$files$annotations$raw$sirius,
          output_ann = par_pre_ann_sir$files$annotations$prepared$structural$sirius,
          output_can = par_pre_ann_sir$files$annotations$prepared$canopus,
          output_for = par_pre_ann_sir$files$annotations$prepared$formula,
          sirius_version = par_pre_ann_sir$tools$sirius$version,
          max_analog_abs_mz_error = par_pre_ann_sir$tools$sirius$max_analog_abs_mz_error,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file",
      garbage_collection = TRUE
    ),
    .tar_file_slice(
      "ann_sir_pre",
      c(
        ann_sir_pre_can = 1L,
        ann_sir_pre_for = 2L,
        ann_sir_pre_str = 3L
      )
    )
  )
}
