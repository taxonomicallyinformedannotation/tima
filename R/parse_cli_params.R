#' @title Parse CLI parameters
#'
#' @description This function parses command line parameters
#'
#' @export
#'
#' @noRd
#'
#' @param arguments CLI arguments
#' @param parameters Parameters
#'
#' @return Parameters coming from the CLI
#'
#' @examples NULL
parse_cli_params <- function(arguments, parameters) {
  if (exists("arguments")) {
    if (!is.null(arguments$ann_can_fin)) {
      parameters$annotations$candidates$final <- as.numeric(arguments$ann_can_fin)
    }
    if (!is.null(arguments$ann_ms1only)) {
      parameters$annotations$ms1only <- as.logical(arguments$ann_ms1only)
    }
    if (!is.null(arguments$ann_ms2_app)) {
      parameters$annotations$ms2approx <- as.logical(arguments$ann_ms2_app)
    }
    if (!is.null(arguments$ann_thr_con)) {
      parameters$annotations$thresholds$consistency <-
        as.numeric(arguments$ann_thr_con)
    }
    if (!is.null(arguments$ann_thr_ms1_bio)) {
      parameters$annotations$thresholds$ms1$biological <-
        as.numeric(arguments$ann_thr_ms1_bio)
    }
    if (!is.null(arguments$ann_thr_ms1_che)) {
      parameters$annotations$thresholds$ms1$chemical <-
        as.numeric(arguments$ann_thr_ms1_che)
    }
    if (!is.null(arguments$ann_thr_ms1_con)) {
      parameters$annotations$thresholds$ms1$condition <-
        as.character(arguments$ann_thr_ms1_con)
    }
    if (!is.null(arguments$ann_thr_ms2_sim_ann)) {
      parameters$annotations$thresholds$ms2$similarity <-
        as.numeric(arguments$ann_thr_ms2_sim_ann)
    }
    if (!is.null(arguments$ann_thr_ms2_sim_edg)) {
      parameters$annotations$thresholds$ms2$similarity$edges <-
        as.numeric(arguments$ann_thr_ms2_sim_edg)
    }
    if (!is.null(arguments$fil_pat)) {
      parameters$files$pattern <- as.character(arguments$fil_pat)
    }
    # if (!is.null(arguments$fil_ann_raw_spe)) {
    #   parameters$files$annotations$raw$spectral <-
    #     as.character(arguments$fil_ann_raw_spe)
    # }
    if (!is.null(arguments$fil_ann_raw_spe_gnp)) {
      parameters$files$annotations$raw$spectral$gnps <-
        as.character(arguments$fil_ann_raw_spe_gnp)
    }
    if (!is.null(arguments$fil_ann_raw_spe_spe)) {
      parameters$files$annotations$raw$spectral$spectral <-
        as.character(arguments$fil_ann_raw_spe_spe)
    }
    if (!is.null(arguments$fil_ann_raw_sir)) {
      parameters$files$annotations$raw$sirius <-
        as.character(arguments$fil_ann_raw_sir)
    }
    if (!is.null(arguments$fil_ann_fil)) {
      parameters$files$annotations$filtered <- as.character(arguments$fil_ann_fil)
    }
    if (!is.null(arguments$fil_ann_pre_can)) {
      parameters$files$annotations$prepared$canopus <-
        as.character(arguments$fil_ann_pre_can)
    }
    if (!is.null(arguments$fil_ann_pre_for)) {
      parameters$files$annotations$prepared$formula <-
        as.character(arguments$fil_ann_pre_for)
    }
    if (!is.null(arguments$fil_ann_pre_str)) {
      parameters$files$annotations$prepared$structural <-
        as.character(arguments$fil_ann_pre_str)
    }
    if (!is.null(arguments$fil_ann_pro)) {
      parameters$files$annotations$processed <- as.character(arguments$fil_ann_pro)
    }
    if (!is.null(arguments$fil_fea_raw)) {
      parameters$files$features$raw <- as.character(arguments$fil_fea_raw)
    }
    if (!is.null(arguments$fil_fea_pre)) {
      parameters$files$features$prepared <- as.character(arguments$fil_fea_pre)
    }
    if (!is.null(arguments$fil_lib_sop_raw_clo)) {
      parameters$files$libraries$sop$raw$closed <-
        as.character(arguments$fil_lib_sop_raw_clo)
    }
    if (!is.null(arguments$fil_lib_sop_raw_ecm)) {
      parameters$files$libraries$sop$raw$ecmdb <-
        as.character(arguments$fil_lib_sop_raw_ecm)
    }
    if (!is.null(arguments$fil_lib_sop_raw_hmd)) {
      parameters$files$libraries$sop$raw$hmdb <-
        as.character(arguments$fil_lib_sop_raw_hmd)
    }
    if (!is.null(arguments$fil_lib_sop_raw_lot)) {
      parameters$files$libraries$sop$raw$lotus <-
        as.character(arguments$fil_lib_sop_raw_lot)
    }
    if (!is.null(arguments$fil_lib_sop_pre)) {
      parameters$files$libraries$sop$prepared <-
        as.character(arguments$fil_lib_sop_pre)
    }
    if (!is.null(arguments$fil_lib_spe_neg)) {
      parameters$files$libraries$spectral$neg <-
        as.character(arguments$fil_lib_spe_neg)
    }
    if (!is.null(arguments$fil_lib_spe_pos)) {
      parameters$files$libraries$spectral$pos <-
        as.character(arguments$fil_lib_spe_pos)
    }
    if (!is.null(arguments$fil_lib_spe_raw)) {
      parameters$files$libraries$spectral$raw <-
        as.character(arguments$fil_lib_spe_raw)
    }
    if (!is.null(arguments$fil_lib_tem_exp_csv)) {
      parameters$files$libraries$temporal$exp$csv <-
        as.character(arguments$fil_lib_tem_exp_csv)
    }
    if (!is.null(arguments$fil_lib_tem_exp_mgf_neg)) {
      parameters$files$libraries$temporal$exp$mgf$neg <-
        as.character(arguments$fil_lib_tem_exp_mgf_neg)
    }
    if (!is.null(arguments$fil_lib_tem_exp_mgf_pos)) {
      parameters$files$libraries$temporal$exp$mgf$pos <-
        as.character(arguments$fil_lib_tem_exp_mgf_pos)
    }
    if (!is.null(arguments$fil_lib_tem_is_csv)) {
      parameters$files$libraries$temporal$is$csv <-
        as.character(arguments$fil_lib_tem_is_csv)
    }
    if (!is.null(arguments$fil_lib_tem_is_mgf_neg)) {
      parameters$files$libraries$temporal$is$mgf$neg <-
        as.character(arguments$fil_lib_tem_is_mgf_neg)
    }
    if (!is.null(arguments$fil_lib_tem_is_mgf_pos)) {
      parameters$files$libraries$temporal$is$mgf$pos <-
        as.character(arguments$fil_lib_tem_is_mgf_pos)
    }
    if (!is.null(arguments$fil_lib_tem_pre)) {
      parameters$files$libraries$temporal$prepared <-
        as.character(arguments$fil_lib_tem_pre)
    }
    if (!is.null(arguments$fil_net_spe_edg_raw)) {
      parameters$files$networks$spectral$edges$raw <-
        as.character(arguments$fil_net_spe_edg_raw)
    }
    if (!is.null(arguments$fil_net_spe_edg_pre)) {
      parameters$files$networks$spectral$edges$prepared <-
        as.character(arguments$fil_net_spe_edg_pre)
    }
    if (!is.null(arguments$fil_net_spe_com_raw)) {
      parameters$files$networks$spectral$components$raw <-
        as.character(arguments$fil_net_spe_com_raw)
    }
    if (!is.null(arguments$fil_net_spe_com_pre)) {
      parameters$files$networks$spectral$components$prepared <-
        as.character(arguments$fil_net_spe_com_pre)
    }
    if (!is.null(arguments$fil_met_raw)) {
      parameters$files$metadata$raw <- as.character(arguments$fil_met_raw)
    }
    if (!is.null(arguments$fil_met_pre)) {
      parameters$files$metadata$prepared <- as.character(arguments$fil_met_pre)
    }
    if (!is.null(arguments$fil_spe_raw)) {
      parameters$files$spectral$raw <- as.character(arguments$fil_spe_raw)
    }
    if (!is.null(arguments$gnps_id)) {
      parameters$gnps$id <- as.character(arguments$gnps_id)
    }
    if (!is.null(arguments$gnps_workflow)) {
      parameters$gnps$workflow <- as.character(arguments$gnps_workflow)
    }
    if (!is.null(arguments$ms_add_neg)) {
      parameters$ms$adducts$neg <- as.character(arguments$ms_add_neg)
    }
    if (!is.null(arguments$ms_add_pos)) {
      parameters$ms$adducts$pos <- as.character(arguments$ms_add_pos)
    }
    if (!is.null(arguments$ms_clu_neg)) {
      parameters$ms$clusters$neg <- as.character(arguments$ms_clu_neg)
    }
    if (!is.null(arguments$ms_clu_pos)) {
      parameters$ms$clusters$pos <- as.character(arguments$ms_clu_pos)
    }
    if (!is.null(arguments$ms_neu)) {
      parameters$ms$neutral_losses <- as.character(arguments$ms_neu)
    }
    if (!is.null(arguments$ms_pol)) {
      parameters$ms$polarity <- as.character(arguments$ms_pol)
    }
    if (!is.null(arguments$ms_thr_ms2_int)) {
      parameters$ms$thresholds$ms2$intensity <- as.numeric(arguments$ms_thr_ms2_int)
    }
    if (!is.null(arguments$ms_tol_mas_ppm_ms1)) {
      parameters$ms$tolerances$mass$ppm$ms1 <-
        as.numeric(arguments$ms_tol_mas_ppm_ms1)
    }
    if (!is.null(arguments$ms_tol_mas_ppm_ms2)) {
      parameters$ms$tolerances$mass$ppm$ms2 <-
        as.numeric(arguments$ms_tol_mas_ppm_ms2)
    }
    if (!is.null(arguments$ms_tol_mas_dal_ms1)) {
      parameters$ms$tolerances$mass$dalton$ms1 <-
        as.numeric(arguments$ms_tol_mas_dal_ms1)
    }
    if (!is.null(arguments$ms_tol_mas_dal_ms2)) {
      parameters$ms$tolerances$mass$dalton$ms2 <-
        as.numeric(arguments$ms_tol_mas_dal_ms2)
    }
    if (!is.null(arguments$ms_tol_rt_add)) {
      parameters$ms$tolerances$rt$adducts <- as.numeric(arguments$ms_tol_rt_add)
    }
    if (!is.null(arguments$ms_tol_rt_lib)) {
      parameters$ms$tolerances$rt$library <- as.numeric(arguments$ms_tol_rt_lib)
    }
    if (!is.null(arguments$names_adduct)) {
      parameters$names$adduct <- as.character(arguments$names_adduct)
    }
    if (!is.null(arguments$names_extension)) {
      parameters$names$extension <- as.logical(arguments$names_extension)
    }
    if (!is.null(arguments$names_features)) {
      parameters$names$features <- as.character(arguments$names_features)
    }
    if (!is.null(arguments$names_filename)) {
      parameters$names$filename <- as.character(arguments$names_filename)
    }
    if (!is.null(arguments$names_inchikey)) {
      parameters$names$inchikey <- as.character(arguments$names_inchikey)
    }
    if (!is.null(arguments$names_lib)) {
      parameters$names$libraries <- as.character(arguments$names_lib)
    }
    if (!is.null(arguments$names_mgf_ad)) {
      parameters$names$mgf$adduct <- as.character(arguments$names_mgf_ad)
    }
    if (!is.null(arguments$names_mgf_ce)) {
      parameters$names$mgf$collision_energy <- as.character(arguments$names_mgf_ce)
    }
    if (!is.null(arguments$names_mgf_ci)) {
      parameters$names$mgf$compound_id <- as.character(arguments$names_mgf_ci)
    }
    if (!is.null(arguments$names_mgf_em)) {
      parameters$names$mgf$exact_mass <- as.character(arguments$names_mgf_em)
    }
    if (!is.null(arguments$names_mgf_in)) {
      parameters$names$mgf$inchi <- as.character(arguments$names_mgf_in)
    }
    if (!is.null(arguments$names_mgf_io)) {
      parameters$names$mgf$inchi_no_stereo <- as.character(arguments$names_mgf_io)
    }
    if (!is.null(arguments$names_mgf_ik)) {
      parameters$names$mgf$inchikey <- as.character(arguments$names_mgf_ik)
    }
    if (!is.null(arguments$names_mgf_il)) {
      parameters$names$mgf$inchikey_no_stereo <-
        as.character(arguments$names_mgf_il)
    }
    if (!is.null(arguments$names_mgf_mf)) {
      parameters$names$mgf$molecular_formula <- as.character(arguments$names_mgf_mf)
    }
    if (!is.null(arguments$names_mgf_na)) {
      parameters$names$mgf$name <- as.character(arguments$names_mgf_na)
    }
    if (!is.null(arguments$names_mgf_po)) {
      parameters$names$mgf$polarity <- as.character(arguments$names_mgf_po)
    }
    # if (!is.null(arguments$names_mgf_pc)) {
    #   parameters$names$mgf$precursor_charge <-
    #     as.character(arguments$names_mgf_pc)
    # }
    # if (!is.null(arguments$names_mgf_pm)) {
    #   parameters$names$mgf$precursor_mz <- as.character(arguments$names_mgf_pm)
    # }
    if (!is.null(arguments$names_mgf_sm)) {
      parameters$names$mgf$smiles <- as.character(arguments$names_mgf_sm)
    }
    if (!is.null(arguments$names_mgf_sn)) {
      parameters$names$mgf$smiles_no_stereo <- as.character(arguments$names_mgf_sn)
    }
    if (!is.null(arguments$names_mgf_si)) {
      parameters$names$mgf$spectrum_id <- as.character(arguments$names_mgf_si)
    }
    if (!is.null(arguments$names_mgf_sp)) {
      parameters$names$mgf$splash <- as.character(arguments$names_mgf_sp)
    }
    if (!is.null(arguments$names_mgf_sy)) {
      parameters$names$mgf$synonyms <- as.character(arguments$names_mgf_sy)
    }
    if (!is.null(arguments$names_mgf_xl)) {
      parameters$names$mgf$xlogp <- as.character(arguments$names_mgf_xl)
    }
    if (!is.null(arguments$names_precursor)) {
      parameters$names$precursor <- as.character(arguments$names_precursor)
    }
    if (!is.null(arguments$names_rt_fea)) {
      parameters$names$rt$features <- as.character(arguments$names_rt_fea)
    }
    if (!is.null(arguments$names_rt_lib)) {
      parameters$names$rt$library <- as.character(arguments$names_rt_lib)
    }
    if (!is.null(arguments$names_smiles)) {
      parameters$names$smiles <- as.character(arguments$names_smiles)
    }
    if (!is.null(arguments$names_source)) {
      parameters$names$source <- as.character(arguments$names_source)
    }
    if (!is.null(arguments$names_target)) {
      parameters$names$target <- as.character(arguments$names_target)
    }
    if (!is.null(arguments$names_taxon)) {
      parameters$names$taxon <- as.character(arguments$names_taxon)
    }
    if (!is.null(arguments$org_can)) {
      parameters$organisms$candidates <- as.numeric(arguments$org_can)
    }
    if (!is.null(arguments$org_fil_mod)) {
      parameters$organisms$filter$mode <- as.logical(arguments$org_fil_mod)
    }
    if (!is.null(arguments$org_fil_lev)) {
      parameters$organisms$filter$level <- as.character(arguments$org_fil_lev)
    }
    if (!is.null(arguments$org_fil_val)) {
      parameters$organisms$filter$value <- as.character(arguments$org_fil_val)
    }
    if (!is.null(arguments$org_tax)) {
      parameters$organisms$taxon <- as.character(arguments$org_tax)
    }
    if (!is.null(arguments$too_met)) {
      parameters$tools$metadata <- as.character(arguments$too_met)
    }
    if (!is.null(arguments$too_net_spe_com)) {
      parameters$tools$networks$spectral$components <-
        as.character(arguments$too_net_spe_com)
    }
    if (!is.null(arguments$too_net_spe_edg)) {
      parameters$tools$networks$spectral$edges <-
        as.character(arguments$too_net_spe_edg)
    }
    if (!is.null(arguments$too_sir_ver)) {
      parameters$tools$sirius$version <- as.integer(arguments$too_sir_ver)
    }
    if (!is.null(arguments$too_tax_bio)) {
      parameters$tools$taxonomies$biological <- as.character(arguments$too_tax_bio)
    }
    if (!is.null(arguments$too_tax_che)) {
      parameters$tools$taxonomies$chemical <- as.character(arguments$too_tax_che)
    }
    if (!is.null(arguments$units_rt)) {
      parameters$units$rt <- as.character(arguments$units_rt)
    }
    if (!is.null(arguments$wei_glo_bio)) {
      parameters$weights$global$biological <- as.numeric(arguments$wei_glo_bio)
    }
    if (!is.null(arguments$wei_glo_che)) {
      parameters$weights$global$chemical <- as.numeric(arguments$wei_glo_che)
    }
    if (!is.null(arguments$wei_glo_spe)) {
      parameters$weights$global$spectral <- as.numeric(arguments$wei_glo_spe)
    }
    if (!is.null(arguments$wei_bio_01)) {
      parameters$weights$biological$domain <- as.numeric(arguments$wei_bio_01)
    }
    if (!is.null(arguments$wei_bio_02)) {
      parameters$weights$biological$kingdom <- as.numeric(arguments$wei_bio_02)
    }
    if (!is.null(arguments$wei_bio_03)) {
      parameters$weights$biological$phylum <- as.numeric(arguments$wei_bio_03)
    }
    if (!is.null(arguments$wei_bio_04)) {
      parameters$weights$biological$class <- as.numeric(arguments$wei_bio_04)
    }
    if (!is.null(arguments$wei_bio_05)) {
      parameters$weights$biological$order <- as.numeric(arguments$wei_bio_05)
    }
    if (!is.null(arguments$wei_bio_06)) {
      parameters$weights$biological$infraorder <- as.numeric(arguments$wei_bio_06)
    }
    if (!is.null(arguments$wei_bio_07)) {
      parameters$weights$biological$family <- as.numeric(arguments$wei_bio_07)
    }
    if (!is.null(arguments$wei_bio_08)) {
      parameters$weights$biological$subfamily <- as.numeric(arguments$wei_bio_08)
    }
    if (!is.null(arguments$wei_bio_09)) {
      parameters$weights$biological$tribe <- as.numeric(arguments$wei_bio_09)
    }
    if (!is.null(arguments$wei_bio_10)) {
      parameters$weights$biological$subtribe <- as.numeric(arguments$wei_bio_10)
    }
    if (!is.null(arguments$wei_bio_11)) {
      parameters$weights$biological$genus <- as.numeric(arguments$wei_bio_11)
    }
    if (!is.null(arguments$wei_bio_12)) {
      parameters$weights$biological$subgenus <- as.numeric(arguments$wei_bio_12)
    }
    if (!is.null(arguments$wei_bio_13)) {
      parameters$weights$biological$species <- as.numeric(arguments$wei_bio_13)
    }
    if (!is.null(arguments$wei_bio_14)) {
      parameters$weights$biological$subspecies <- as.numeric(arguments$wei_bio_14)
    }
    if (!is.null(arguments$wei_bio_15)) {
      parameters$weights$biological$variety <- as.numeric(arguments$wei_bio_15)
    }
    if (!is.null(arguments$wei_che_11)) {
      parameters$weights$chemical$cla$kingdom <- as.numeric(arguments$wei_che_11)
    }
    if (!is.null(arguments$wei_che_12)) {
      parameters$weights$chemical$cla$superclass <- as.numeric(arguments$wei_che_12)
    }
    if (!is.null(arguments$wei_che_13)) {
      parameters$weights$chemical$cla$class <- as.numeric(arguments$wei_che_13)
    }
    if (!is.null(arguments$wei_che_14)) {
      parameters$weights$chemical$cla$parent <- as.numeric(arguments$wei_che_14)
    }
    if (!is.null(arguments$wei_che_21)) {
      parameters$weights$chemical$npc$pathway <- as.numeric(arguments$wei_che_21)
    }
    if (!is.null(arguments$wei_che_22)) {
      parameters$weights$chemical$npc$superclass <- as.numeric(arguments$wei_che_22)
    }
    if (!is.null(arguments$wei_che_23)) {
      parameters$weights$chemical$npc$class <- as.numeric(arguments$wei_che_23)
    }
    if (!is.null(arguments$compounds_names)) {
      parameters$options$compounds_names <- as.logical(arguments$compounds_names)
    }
    if (!is.null(arguments$force)) {
      parameters$options$force <- as.logical(arguments$force)
    }
    if (!is.null(arguments$high_confidence)) {
      parameters$options$high_confidence <- as.logical(arguments$high_confidence)
    }
    if (!is.null(arguments$remove_ties)) {
      parameters$options$remove_ties <- as.logical(arguments$remove_ties)
    }
    if (!is.null(arguments$summarise)) {
      parameters$options$summarise <- as.logical(arguments$summarise)
    }
  }
  return(parameters)
}
