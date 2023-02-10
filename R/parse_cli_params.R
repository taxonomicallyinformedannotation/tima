#' @title Parse CLI parameters
#'
#' @description This function parses command line parameters
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
parse_cli_params <- function() {
  log_debug("Loading command line arguments")

  if (exists("arguments")) {
    if (!is.null(arguments$ann_can_ini)) {
      params$annotations$candidates$initial <- arguments$ann_can_ini
    }
    if (!is.null(arguments$ann_can_fin)) {
      params$annotations$candidates$final <- arguments$ann_can_fin
    }
    if (!is.null(arguments$ann_ms1only)) {
      params$annotations$ms1only <- arguments$ann_ms1only
    }
    if (!is.null(arguments$ann_ms1_ann)) {
      params$annotations$ms1$annotate <- arguments$ann_ms1_ann
    }
    if (!is.null(arguments$ann_ms1_thr_bio)) {
      params$annotations$ms1$thresholds$biological <- arguments$ann_ms1_thr_bio
    }
    if (!is.null(arguments$ann_ms1_thr_che)) {
      params$annotations$ms1$thresholds$chemical <- arguments$ann_ms1_thr_che
    }
    if (!is.null(arguments$ann_ms1_thr_con)) {
      params$annotations$ms1$thresholds$condition <- arguments$ann_ms1_thr_con
    }
    if (!is.null(arguments$ann_ms2_app)) {
      params$annotations$ms2$approx <- arguments$ann_ms2_app
    }
    if (!is.null(arguments$ann_ms2_met)) {
      params$annotations$ms2$method <- arguments$ann_ms2_met
    }
    if (!is.null(arguments$ann_ms2_thr_con)) {
      params$annotations$ms2$thresholds$condition <- arguments$ann_ms2_thr_con
    }
    if (!is.null(arguments$ann_ms2_thr_pea_abs)) {
      params$annotations$ms2$thresholds$peaks$absolute <- arguments$ann_ms2_thr_pea_abs
    }
    if (!is.null(arguments$ann_ms2_thr_pea_rat)) {
      params$annotations$ms2$thresholds$peaks$ratio <- arguments$ann_ms2_thr_pea_rat
    }
    if (!is.null(arguments$ann_ms2_thr_sim)) {
      params$annotations$ms2$thresholds$similarity <- arguments$ann_ms2_thr_sim
    }
    if (!is.null(arguments$fil_ann_raw_spe)) {
      params$files$annotations$raw$spectral <- arguments$fil_ann_raw_spe
    }
    if (!is.null(arguments$fil_ann_raw_sir)) {
      params$files$annotations$raw$sirius <- arguments$fil_ann_raw_sir
    }
    if (!is.null(arguments$fil_ann_pre)) {
      params$files$annotations$pretreated <- arguments$fil_ann_pre
    }
    if (!is.null(arguments$fil_ann_fil)) {
      params$files$annotations$filled <- arguments$fil_ann_fil
    }
    if (!is.null(arguments$fil_ann_tre)) {
      params$files$annotations$treated <- arguments$fil_ann_tre
    }
    if (!is.null(arguments$fil_ann_pro)) {
      params$files$annotations$processed <- arguments$fil_ann_pro
    }
    if (!is.null(arguments$fil_fea_raw)) {
      params$files$features$raw <- arguments$fil_fea_raw
    }
    if (!is.null(arguments$fil_lib_add_pro)) {
      params$files$libraries$adducts$processed <- arguments$fil_lib_add_pro
    }
    if (!is.null(arguments$fil_lib_sop_raw_clo)) {
      params$files$libraries$sop$raw$closed <- arguments$fil_lib_sop_raw_clo
    }
    if (!is.null(arguments$fil_lib_sop_raw_lot)) {
      params$files$libraries$sop$raw$lotus <- arguments$fil_lib_sop_raw_lot
    }
    if (!is.null(arguments$fil_lib_sop_pro)) {
      params$files$libraries$sop$processed <- arguments$fil_lib_sop_pro
    }
    if (!is.null(arguments$fil_lib_sop_mer)) {
      params$files$libraries$sop$merged <- arguments$fil_lib_sop_mer
    }
    if (!is.null(arguments$fil_lib_spe_neg)) {
      params$files$libraries$spectral$neg <- arguments$fil_lib_spe_neg
    }
    if (!is.null(arguments$fil_lib_spe_pos)) {
      params$files$libraries$spectral$pos <- arguments$fil_lib_spe_pos
    }
    if (!is.null(arguments$fil_net_spe_edg_raw)) {
      params$files$networks$spectral$edges$raw <- arguments$fil_net_spe_edg_raw
    }
    if (!is.null(arguments$fil_net_spe_edg_pro)) {
      params$files$networks$spectral$edges$processed <- arguments$fil_net_spe_edg_pro
    }
    if (!is.null(arguments$fil_net_spe_com_raw)) {
      params$files$networks$spectral$components$raw <- arguments$fil_net_spe_com_raw
    }
    if (!is.null(arguments$fil_tax_raw)) {
      params$files$taxa$raw <- arguments$fil_tax_raw
    }
    if (!is.null(arguments$fil_tax_pro)) {
      params$files$taxa$processed <- arguments$fil_tax_pro
    }
    if (!is.null(arguments$fil_spe_raw)) {
      params$files$spectral$raw <- arguments$fil_spe_raw
    }
    if (!is.null(arguments$gnps_id)) {
      params$gnps$id <- arguments$gnps_id
    }
    if (!is.null(arguments$gnps_nap)) {
      params$gnps$nap <- arguments$gnps_nap
    }
    if (!is.null(arguments$gnps_workflow)) {
      params$gnps$workflow <- arguments$gnps_workflow
    }
    if (!is.null(arguments$ms_add_neg)) {
      params$ms$adducts$neg <- arguments$ms_add_neg
    }
    if (!is.null(arguments$ms_add_pos)) {
      params$ms$adducts$pos <- arguments$ms_add_pos
    }
    if (!is.null(arguments$ms_int_thr_ms1)) {
      params$ms$intensity$thresholds$ms1 <- arguments$ms_int_thr_ms1
    }
    if (!is.null(arguments$ms_int_thr_ms2)) {
      params$ms$intensity$thresholds$ms2 <- arguments$ms_int_thr_ms2
    }
    if (!is.null(arguments$ms_pol)) {
      params$ms$polarity <- arguments$ms_pol
    }
    if (!is.null(arguments$ms_tol_mas_ppm_ms1)) {
      params$ms$tolerances$mass$ppm$ms1 <- arguments$ms_tol_mas_ppm_ms1
    }
    if (!is.null(arguments$ms_tol_mas_ppm_ms2)) {
      params$ms$tolerances$mass$ppm$ms2 <- arguments$ms_tol_mas_ppm_ms2
    }
    if (!is.null(arguments$ms_tol_mas_dal_ms1)) {
      params$ms$tolerances$mass$dalton$ms1 <- arguments$ms_tol_mas_dal_ms1
    }
    if (!is.null(arguments$ms_tol_mas_dal_ms2)) {
      params$ms$tolerances$mass$dalton$ms2 <- arguments$ms_tol_mas_dal_ms2
    }
    if (!is.null(arguments$ms_tol_rt_min)) {
      params$ms$tolerances$rt$minutes <- arguments$ms_tol_rt_min
    }
    if (!is.null(arguments$names_extension)) {
      params$names$extension <- arguments$names_extension
    }
    if (!is.null(arguments$names_features)) {
      params$names$features <- arguments$names_features
    }
    if (!is.null(arguments$names_precursor)) {
      params$names$precursor <- arguments$names_precursor
    }
    if (!is.null(arguments$names_rt)) {
      params$names$rt <- arguments$names_rt
    }
    if (!is.null(arguments$names_source)) {
      params$names$source <- arguments$names_source
    }
    if (!is.null(arguments$names_target)) {
      params$names$target <- arguments$names_target
    }
    if (!is.null(arguments$names_taxon)) {
      params$names$taxon <- arguments$names_taxon
    }
    if (!is.null(arguments$org_can)) {
      params$organisms$candidates <- arguments$org_can
    }
    if (!is.null(arguments$org_fil_mod)) {
      params$organisms$filter$mode <- arguments$org_fil_mod
    }
    if (!is.null(arguments$org_fil_lev)) {
      params$organisms$filter$level <- arguments$org_fil_lev
    }
    if (!is.null(arguments$org_fil_val)) {
      params$organisms$filter$value <- arguments$org_fil_val
    }
    if (!is.null(arguments$org_tax)) {
      params$organisms$taxon <- arguments$org_tax
    }
    if (!is.null(arguments$too_met)) {
      params$tools$metadata <- arguments$too_met
    }
    if (!is.null(arguments$too_net_spe_com)) {
      params$tools$networks$spectral$components <- arguments$too_net_spe_com
    }
    if (!is.null(arguments$too_net_spe_edg)) {
      params$tools$networks$spectral$edges <- arguments$too_net_spe_edg
    }
    if (!is.null(arguments$too_tax_bio)) {
      params$tools$taxonomies$biological <- arguments$too_tax_bio
    }
    if (!is.null(arguments$too_tax_che)) {
      params$tools$taxonomies$chemical <- arguments$too_tax_che
    }
    if (!is.null(arguments$wei_glo_bio)) {
      params$weights$global$biological <- arguments$wei_glo_bio
    }
    if (!is.null(arguments$wei_glo_che)) {
      params$weights$global$chemical <- arguments$wei_glo_che
    }
    if (!is.null(arguments$wei_glo_spe)) {
      params$weights$global$spectral <- arguments$wei_glo_spe
    }
    if (!is.null(arguments$wei_bio_01)) {
      params$weights$biological$domain <- arguments$wei_bio_01
    }
    if (!is.null(arguments$wei_bio_02)) {
      params$weights$biological$kingdom <- arguments$wei_bio_02
    }
    if (!is.null(arguments$wei_bio_03)) {
      params$weights$biological$phylum <- arguments$wei_bio_03
    }
    if (!is.null(arguments$wei_bio_04)) {
      params$weights$biological$class <- arguments$wei_bio_04
    }
    if (!is.null(arguments$wei_bio_05)) {
      params$weights$biological$order <- arguments$wei_bio_05
    }
    if (!is.null(arguments$wei_bio_06)) {
      params$weights$biological$infraorder <- arguments$wei_bio_06
    }
    if (!is.null(arguments$wei_bio_07)) {
      params$weights$biological$family <- arguments$wei_bio_07
    }
    if (!is.null(arguments$wei_bio_08)) {
      params$weights$biological$subfamily <- arguments$wei_bio_08
    }
    if (!is.null(arguments$wei_bio_09)) {
      params$weights$biological$tribe <- arguments$wei_bio_09
    }
    if (!is.null(arguments$wei_bio_10)) {
      params$weights$biological$subtribe <- arguments$wei_bio_10
    }
    if (!is.null(arguments$wei_bio_11)) {
      params$weights$biological$genus <- arguments$wei_bio_11
    }
    if (!is.null(arguments$wei_bio_12)) {
      params$weights$biological$subgenus <- arguments$wei_bio_12
    }
    if (!is.null(arguments$wei_bio_13)) {
      params$weights$biological$species <- arguments$wei_bio_13
    }
    if (!is.null(arguments$wei_bio_14)) {
      params$weights$biological$subspecies <- arguments$wei_bio_14
    }
    if (!is.null(arguments$wei_bio_15)) {
      params$weights$biological$variety <- arguments$wei_bio_15
    }
    if (!is.null(arguments$wei_che_01)) {
      params$weights$chemical$pathway <- arguments$wei_che_01
    }
    if (!is.null(arguments$wei_che_02)) {
      params$weights$chemical$superclass <- arguments$wei_che_02
    }
    if (!is.null(arguments$wei_che_03)) {
      params$weights$chemical$class <- arguments$wei_che_03
    }
    if (!is.null(arguments$fast)) {
      params$options$fast <- arguments$fast
    }
    if (!is.null(arguments$force)) {
      params$options$force <- arguments$force
    }
    if (!is.null(arguments$parallel)) {
      params$options$parallel <- arguments$parallel
    }
  }

  if (exists("params")) {
    return(params)
  }
}
