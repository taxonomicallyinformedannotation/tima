utils::globalVariables(
  c(
    "arguments"
  )
)

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
      params$annotations$candidates$initial <- as.numeric(arguments$ann_can_ini)
    }
    if (!is.null(arguments$ann_can_fin)) {
      params$annotations$candidates$final <- as.numeric(arguments$ann_can_fin)
    }
    if (!is.null(arguments$ann_ms1only)) {
      params$annotations$ms1only <- as.logical(arguments$ann_ms1only)
    }
    if (!is.null(arguments$ann_ms1_ann)) {
      params$annotations$ms1$annotate <- as.logical(arguments$ann_ms1_ann)
    }
    if (!is.null(arguments$ann_ms1_thr_bio)) {
      params$annotations$ms1$thresholds$biological <-
        as.numeric(arguments$ann_ms1_thr_bio)
    }
    if (!is.null(arguments$ann_ms1_thr_che)) {
      params$annotations$ms1$thresholds$chemical <-
        as.numeric(arguments$ann_ms1_thr_che)
    }
    if (!is.null(arguments$ann_ms1_thr_con)) {
      params$annotations$ms1$thresholds$condition <-
        as.character(arguments$ann_ms1_thr_con)
    }
    if (!is.null(arguments$ann_ms2_app)) {
      params$annotations$ms2$approx <- as.logical(arguments$ann_ms2_app)
    }
    if (!is.null(arguments$ann_ms2_thr_sim)) {
      params$annotations$ms2$thresholds$similarity <-
        as.numeric(arguments$ann_ms2_thr_sim)
    }
    if (!is.null(arguments$fil_pat)) {
      params$files$pattern <- as.character(arguments$fil_pat)
    }
    if (!is.null(arguments$fil_ann_raw_spe)) {
      params$files$annotations$raw$spectral <-
        as.character(arguments$fil_ann_raw_spe)
    }
    if (!is.null(arguments$fil_ann_raw_sir)) {
      params$files$annotations$raw$sirius <-
        as.character(arguments$fil_ann_raw_sir)
    }
    if (!is.null(arguments$fil_ann_fil)) {
      params$files$annotations$filtered <- as.character(arguments$fil_ann_fil)
    }
    if (!is.null(arguments$fil_ann_pre)) {
      params$files$annotations$prepared <- as.character(arguments$fil_ann_pre)
    }
    if (!is.null(arguments$fil_ann_pro)) {
      params$files$annotations$processed <- as.character(arguments$fil_ann_pro)
    }
    if (!is.null(arguments$fil_fea_raw)) {
      params$files$features$raw <- as.character(arguments$fil_fea_raw)
    }
    if (!is.null(arguments$fil_fea_pre)) {
      params$files$features$prepared <- as.character(arguments$fil_fea_pre)
    }
    if (!is.null(arguments$fil_lib_add_pro)) {
      params$files$libraries$adducts$prepared <-
        as.character(arguments$fil_lib_add_pro)
    }
    if (!is.null(arguments$fil_lib_sop_raw_clo)) {
      params$files$libraries$sop$raw$closed <-
        as.character(arguments$fil_lib_sop_raw_clo)
    }
    if (!is.null(arguments$fil_lib_sop_raw_lot)) {
      params$files$libraries$sop$raw$lotus <-
        as.character(arguments$fil_lib_sop_raw_lot)
    }
    if (!is.null(arguments$fil_lib_sop_pro)) {
      params$files$libraries$sop$prepared <-
        as.character(arguments$fil_lib_sop_pro)
    }
    if (!is.null(arguments$fil_lib_spe_neg)) {
      params$files$libraries$spectral$neg <-
        as.character(arguments$fil_lib_spe_neg)
    }
    if (!is.null(arguments$fil_lib_spe_pos)) {
      params$files$libraries$spectral$pos <-
        as.character(arguments$fil_lib_spe_pos)
    }
    if (!is.null(arguments$fil_lib_spe_raw)) {
      params$files$libraries$spectral$raw <-
        as.character(arguments$fil_lib_spe_raw)
    }
    if (!is.null(arguments$fil_net_spe_edg_raw)) {
      params$files$networks$spectral$edges$raw <-
        as.character(arguments$fil_net_spe_edg_raw)
    }
    if (!is.null(arguments$fil_net_spe_edg_pro)) {
      params$files$networks$spectral$edges$prepared <-
        as.character(arguments$fil_net_spe_edg_pro)
    }
    if (!is.null(arguments$fil_net_spe_com_raw)) {
      params$files$networks$spectral$components$raw <-
        as.character(arguments$fil_net_spe_com_raw)
    }
    if (!is.null(arguments$fil_tax_raw)) {
      params$files$taxa$raw <- as.character(arguments$fil_tax_raw)
    }
    if (!is.null(arguments$fil_tax_pro)) {
      params$files$taxa$prepared <- as.character(arguments$fil_tax_pro)
    }
    if (!is.null(arguments$fil_spe_raw)) {
      params$files$spectral$raw <- as.character(arguments$fil_spe_raw)
    }
    if (!is.null(arguments$gnps_id)) {
      params$gnps$id <- as.character(arguments$gnps_id)
    }
    if (!is.null(arguments$gnps_workflow)) {
      params$gnps$workflow <- as.character(arguments$gnps_workflow)
    }
    if (!is.null(arguments$ms_add_neg)) {
      params$ms$adducts$neg <- as.character(arguments$ms_add_neg)
    }
    if (!is.null(arguments$ms_add_pos)) {
      params$ms$adducts$pos <- as.character(arguments$ms_add_pos)
    }
    if (!is.null(arguments$ms_int_thr_ms1)) {
      params$ms$intensity$thresholds$ms1 <- as.numeric(arguments$ms_int_thr_ms1)
    }
    if (!is.null(arguments$ms_int_thr_ms2)) {
      params$ms$intensity$thresholds$ms2 <- as.numeric(arguments$ms_int_thr_ms2)
    }
    if (!is.null(arguments$ms_pol)) {
      params$ms$polarity <- as.character(arguments$ms_pol)
    }
    if (!is.null(arguments$ms_tol_mas_ppm_ms1)) {
      params$ms$tolerances$mass$ppm$ms1 <-
        as.numeric(arguments$ms_tol_mas_ppm_ms1)
    }
    if (!is.null(arguments$ms_tol_mas_ppm_ms2)) {
      params$ms$tolerances$mass$ppm$ms2 <-
        as.numeric(arguments$ms_tol_mas_ppm_ms2)
    }
    if (!is.null(arguments$ms_tol_mas_dal_ms1)) {
      params$ms$tolerances$mass$dalton$ms1 <-
        as.numeric(arguments$ms_tol_mas_dal_ms1)
    }
    if (!is.null(arguments$ms_tol_mas_dal_ms2)) {
      params$ms$tolerances$mass$dalton$ms2 <-
        as.numeric(arguments$ms_tol_mas_dal_ms2)
    }
    if (!is.null(arguments$ms_tol_rt_min)) {
      params$ms$tolerances$rt$minutes <- as.numeric(arguments$ms_tol_rt_min)
    }
    if (!is.null(arguments$names_extension)) {
      params$names$extension <- as.logical(arguments$names_extension)
    }
    if (!is.null(arguments$names_features)) {
      params$names$features <- as.character(arguments$names_features)
    }
    if (!is.null(arguments$names_filename)) {
      params$names$filename <- as.character(arguments$names_filename)
    }
    if (!is.null(arguments$names_mgf_ce)) {
      params$names$mgf$collision_energy <- as.character(arguments$names_mgf_ce)
    }
    if (!is.null(arguments$names_mgf_ci)) {
      params$names$mgf$compound_id <- as.character(arguments$names_mgf_ci)
    }
    if (!is.null(arguments$names_mgf_em)) {
      params$names$mgf$exact_mass <- as.character(arguments$names_mgf_em)
    }
    if (!is.null(arguments$names_mgf_in)) {
      params$names$mgf$inchi <- as.character(arguments$names_mgf_in)
    }
    if (!is.null(arguments$names_mgf_io)) {
      params$names$mgf$inchi_2D <- as.character(arguments$names_mgf_io)
    }
    if (!is.null(arguments$names_mgf_ik)) {
      params$names$mgf$inchikey <- as.character(arguments$names_mgf_ik)
    }
    if (!is.null(arguments$names_mgf_il)) {
      params$names$mgf$inchikey_2D <- as.character(arguments$names_mgf_il)
    }
    if (!is.null(arguments$names_mgf_mf)) {
      params$names$mgf$molecular_formula <- as.character(arguments$names_mgf_mf)
    }
    if (!is.null(arguments$names_mgf_na)) {
      params$names$mgf$name <- as.character(arguments$names_mgf_na)
    }
    if (!is.null(arguments$names_mgf_po)) {
      params$names$mgf$polarity <- as.character(arguments$names_mgf_po)
    }
    # if (!is.null(arguments$names_mgf_pc)) {
    #   params$names$mgf$precursor_charge <-
    #     as.character(arguments$names_mgf_pc)
    # }
    # if (!is.null(arguments$names_mgf_pm)) {
    #   params$names$mgf$precursor_mz <- as.character(arguments$names_mgf_pm)
    # }
    if (!is.null(arguments$names_mgf_sm)) {
      params$names$mgf$smiles <- as.character(arguments$names_mgf_sm)
    }
    if (!is.null(arguments$names_mgf_sn)) {
      params$names$mgf$smiles_2D <- as.character(arguments$names_mgf_sn)
    }
    if (!is.null(arguments$names_mgf_si)) {
      params$names$mgf$spectrum_id <- as.character(arguments$names_mgf_si)
    }
    if (!is.null(arguments$names_mgf_sp)) {
      params$names$mgf$splash <- as.character(arguments$names_mgf_sp)
    }
    if (!is.null(arguments$names_mgf_sy)) {
      params$names$mgf$synonyms <- as.character(arguments$names_mgf_sy)
    }
    if (!is.null(arguments$names_mgf_xl)) {
      params$names$mgf$xlogp <- as.character(arguments$names_mgf_xl)
    }
    if (!is.null(arguments$names_precursor)) {
      params$names$precursor <- as.character(arguments$names_precursor)
    }
    if (!is.null(arguments$names_rt)) {
      params$names$rt <- as.character(arguments$names_rt)
    }
    if (!is.null(arguments$names_source)) {
      params$names$source <- as.character(arguments$names_source)
    }
    if (!is.null(arguments$names_target)) {
      params$names$target <- as.character(arguments$names_target)
    }
    if (!is.null(arguments$names_taxon)) {
      params$names$taxon <- as.character(arguments$names_taxon)
    }
    if (!is.null(arguments$org_can)) {
      params$organisms$candidates <- as.numeric(arguments$org_can)
    }
    if (!is.null(arguments$org_fil_mod)) {
      params$organisms$filter$mode <- as.logical(arguments$org_fil_mod)
    }
    if (!is.null(arguments$org_fil_lev)) {
      params$organisms$filter$level <- as.character(arguments$org_fil_lev)
    }
    if (!is.null(arguments$org_fil_val)) {
      params$organisms$filter$value <- as.character(arguments$org_fil_val)
    }
    if (!is.null(arguments$org_tax)) {
      params$organisms$taxon <- as.character(arguments$org_tax)
    }
    if (!is.null(arguments$too_met)) {
      params$tools$metadata <- as.character(arguments$too_met)
    }
    if (!is.null(arguments$too_net_spe_com)) {
      params$tools$networks$spectral$components <-
        as.character(arguments$too_net_spe_com)
    }
    if (!is.null(arguments$too_net_spe_edg)) {
      params$tools$networks$spectral$edges <-
        as.character(arguments$too_net_spe_edg)
    }
    if (!is.null(arguments$too_tax_bio)) {
      params$tools$taxonomies$biological <- as.character(arguments$too_tax_bio)
    }
    if (!is.null(arguments$too_tax_che)) {
      params$tools$taxonomies$chemical <- as.character(arguments$too_tax_che)
    }
    if (!is.null(arguments$units_rt)) {
      params$units$rt <- as.character(arguments$units_rt)
    }
    if (!is.null(arguments$wei_glo_bio)) {
      params$weights$global$biological <- as.numeric(arguments$wei_glo_bio)
    }
    if (!is.null(arguments$wei_glo_che)) {
      params$weights$global$chemical <- as.numeric(arguments$wei_glo_che)
    }
    if (!is.null(arguments$wei_glo_spe)) {
      params$weights$global$spectral <- as.numeric(arguments$wei_glo_spe)
    }
    if (!is.null(arguments$wei_bio_01)) {
      params$weights$biological$domain <- as.numeric(arguments$wei_bio_01)
    }
    if (!is.null(arguments$wei_bio_02)) {
      params$weights$biological$kingdom <- as.numeric(arguments$wei_bio_02)
    }
    if (!is.null(arguments$wei_bio_03)) {
      params$weights$biological$phylum <- as.numeric(arguments$wei_bio_03)
    }
    if (!is.null(arguments$wei_bio_04)) {
      params$weights$biological$class <- as.numeric(arguments$wei_bio_04)
    }
    if (!is.null(arguments$wei_bio_05)) {
      params$weights$biological$order <- as.numeric(arguments$wei_bio_05)
    }
    if (!is.null(arguments$wei_bio_06)) {
      params$weights$biological$infraorder <- as.numeric(arguments$wei_bio_06)
    }
    if (!is.null(arguments$wei_bio_07)) {
      params$weights$biological$family <- as.numeric(arguments$wei_bio_07)
    }
    if (!is.null(arguments$wei_bio_08)) {
      params$weights$biological$subfamily <- as.numeric(arguments$wei_bio_08)
    }
    if (!is.null(arguments$wei_bio_09)) {
      params$weights$biological$tribe <- as.numeric(arguments$wei_bio_09)
    }
    if (!is.null(arguments$wei_bio_10)) {
      params$weights$biological$subtribe <- as.numeric(arguments$wei_bio_10)
    }
    if (!is.null(arguments$wei_bio_11)) {
      params$weights$biological$genus <- as.numeric(arguments$wei_bio_11)
    }
    if (!is.null(arguments$wei_bio_12)) {
      params$weights$biological$subgenus <- as.numeric(arguments$wei_bio_12)
    }
    if (!is.null(arguments$wei_bio_13)) {
      params$weights$biological$species <- as.numeric(arguments$wei_bio_13)
    }
    if (!is.null(arguments$wei_bio_14)) {
      params$weights$biological$subspecies <- as.numeric(arguments$wei_bio_14)
    }
    if (!is.null(arguments$wei_bio_15)) {
      params$weights$biological$variety <- as.numeric(arguments$wei_bio_15)
    }
    if (!is.null(arguments$wei_che_11)) {
      params$weights$chemical$cla$kingdom <- as.numeric(arguments$wei_che_11)
    }
    if (!is.null(arguments$wei_che_12)) {
      params$weights$chemical$cla$superclass <- as.numeric(arguments$wei_che_12)
    }
    if (!is.null(arguments$wei_che_13)) {
      params$weights$chemical$cla$class <- as.numeric(arguments$wei_che_13)
    }
    if (!is.null(arguments$wei_che_14)) {
      params$weights$chemical$cla$parent <- as.numeric(arguments$wei_che_14)
    }
    if (!is.null(arguments$wei_che_21)) {
      params$weights$chemical$npc$pathway <- as.numeric(arguments$wei_che_21)
    }
    if (!is.null(arguments$wei_che_22)) {
      params$weights$chemical$npc$superclass <- as.numeric(arguments$wei_che_22)
    }
    if (!is.null(arguments$wei_che_23)) {
      params$weights$chemical$npc$class <- as.numeric(arguments$wei_che_23)
    }
    if (!is.null(arguments$force)) {
      params$options$force <- as.logical(arguments$force)
    }
    if (!is.null(arguments$summarise)) {
      params$options$summarise <- as.logical(arguments$summarise)
    }
  }

  if (exists("params")) {
    return(params)
  }
}
