# Annotations targets section.

targets_section_annotations <- function() {
  list(
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
        format = "file"
      ),
      tar_target(
        name = ann_ms1_pre_ann,
        command = {
          ann_ms1_pre[[1L]]
        },
        format = "file"
      ),
      tar_target(
        name = ann_ms1_pre_edg,
        command = {
          ann_ms1_pre[[2L]]
        },
        format = "file"
      )
    ),
    list(
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
      list(
        tar_target(
          name = ann_spe_pos,
          command = {
            annotate_spectra(
              input = input_spectra,
              ms1_annotations = ann_ms1_pre_ann,
              libraries = c(
                lib_spe_is_nor_pre_pos,
                lib_spe_is_wik_pre_pos,
                lib_spe_exp_env_pre_pos,
                lib_spe_exp_int_pre_pos,
                lib_spe_exp_gnp_pre_pos,
                lib_spe_exp_mb_pre_pos,
                lib_spe_exp_mer_pre_pos
              ),
              polarity = "pos",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_pos.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral$spectral,
                fixed = TRUE
              ),
              method = par_ann_spe$similarities$methods$annotations,
              threshold = par_ann_spe$similarities$thresholds$annotations,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              cutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          },
          format = "file"
        ),
        tar_target(
          name = ann_spe_neg,
          command = {
            annotate_spectra(
              input = input_spectra,
              ms1_annotations = ann_ms1_pre_ann,
              libraries = c(
                lib_spe_is_nor_pre_neg,
                lib_spe_is_wik_pre_neg,
                lib_spe_exp_env_pre_neg,
                lib_spe_exp_int_pre_neg,
                lib_spe_exp_gnp_pre_neg,
                lib_spe_exp_mb_pre_neg,
                lib_spe_exp_mer_pre_neg
              ),
              polarity = "neg",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_neg.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral$spectral,
                fixed = TRUE
              ),
              method = par_ann_spe$similarities$methods$annotations,
              threshold = par_ann_spe$similarities$thresholds$annotations,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              cutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          },
          format = "file"
        ),
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
          format = "file"
        )
      )
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
      format = "file"
    ),
    tar_target(
      name = ann_sir_pre_can,
      command = {
        ann_sir_pre[[1L]]
      },
      format = "file"
    ),
    tar_target(
      name = ann_sir_pre_for,
      command = {
        ann_sir_pre[[2L]]
      },
      format = "file"
    ),
    tar_target(
      name = ann_sir_pre_str,
      command = {
        ann_sir_pre[[3L]]
      },
      format = "file"
    )
  )
}
