#' @title Prepare params
#'
#' @description This function prepares main parameters
#'
#' @include create_dir.R
#' @include export_params.R
#' @include get_params.R
#' @include load_yaml_files.R
#'
#' @param params_small params_small
#' @param params_advanced params_advanced
#' @param step Step
#'
#' @return The path to the yaml files containing prepared parameters
#'
#' @examples NULL
prepare_params <- function(params_small = get_params(step = "prepare_params"),
                           params_advanced = get_params(step = "prepare_params_advanced"),
                           step = NA) {
  list <- load_yaml_files()
  yamls_params <- list$yamls_params

  log_debug(x = "All params")
  ann_can_fin <- params_advanced$annotations$candidates$final
  ann_ms1only <- params_advanced$annotations$ms1only
  ann_ms2_app <- params_advanced$annotations$ms2approx
  ann_thr_con <- params_advanced$annotations$thresholds$consistency
  ann_thr_ms1_bio <- params_advanced$annotations$thresholds$ms1$biological
  ann_thr_ms1_che <- params_advanced$annotations$thresholds$ms1$chemical
  ann_thr_ms1_con <- params_advanced$annotations$thresholds$ms1$condition
  # fil_pat <- params_advanced$files$pattern
  # fil_ann_raw_spe <- params_advanced$files$annotations$raw$spectral
  fil_ann_raw_spe_gnp <- params_advanced$files$annotations$raw$spectral$gnps
  fil_ann_raw_spe_spe <- params_advanced$files$annotations$raw$spectral$spectral
  # fil_ann_raw_sir <- params_advanced$files$annotations$raw$sirius
  fil_ann_pre_can <- params_advanced$files$annotations$prepared$canopus
  fil_ann_pre_for <- params_advanced$files$annotations$prepared$formula
  fil_ann_pre_str <- params_advanced$files$annotations$prepared$structural
  fil_ann_pre_str_gnp <- params_advanced$files$annotations$prepared$structural$gnps
  # fil_ann_pre_str_ms1 <- params_advanced$files$annotations$prepared$structural$ms1
  fil_ann_pre_str_sir <- params_advanced$files$annotations$prepared$structural$sirius
  fil_ann_pre_str_spe <- params_advanced$files$annotations$prepared$structural$spectral
  fil_ann_fil <- params_advanced$files$annotations$filtered
  fil_ann_pro <- params_advanced$files$annotations$processed
  # fil_fea_raw <- params_advanced$files$features$raw
  fil_fea_pre <- params_advanced$files$features$prepared
  fil_lib_sop_raw_clo <- params_advanced$files$libraries$sop$raw$closed
  fil_lib_sop_raw_ecm <- params_advanced$files$libraries$sop$raw$ecmdb
  fil_lib_sop_raw_hmd <- params_advanced$files$libraries$sop$raw$hmdb
  fil_lib_sop_raw_lot <- params_advanced$files$libraries$sop$raw$lotus
  fil_lib_sop_pre <- params_advanced$files$libraries$sop$prepared
  fil_lib_sop_pre_clo <- params_advanced$files$libraries$sop$prepared$closed
  fil_lib_sop_pre_ecm <- params_advanced$files$libraries$sop$prepared$ecmdb
  fil_lib_sop_pre_hmd <- params_advanced$files$libraries$sop$prepared$hmdb
  fil_lib_sop_pre_lot <- params_advanced$files$libraries$sop$prepared$lotus
  fil_lib_sop_pre_rt <- params_advanced$files$libraries$sop$prepared$rt
  # fil_lib_sop_pre_spe <- params_advanced$files$libraries$sop$prepared$spectral
  # fil_lib_sop_pre_wik <- params_advanced$files$libraries$sop$prepared$wikidata
  fil_lib_sop_mer_key <- params_advanced$files$libraries$sop$merged$keys
  fil_lib_sop_mer_org_nam <- params_advanced$files$libraries$sop$merged$organisms$names
  fil_lib_sop_mer_org_tax_ott <- params_advanced$files$libraries$sop$merged$organisms$taxonomies$ott
  fil_lib_sop_mer_str_ste <- params_advanced$files$libraries$sop$merged$structures$stereo
  fil_lib_sop_mer_str_met <- params_advanced$files$libraries$sop$merged$structures$metadata
  fil_lib_sop_mer_str_nam <- params_advanced$files$libraries$sop$merged$structures$names
  fil_lib_sop_mer_str_tax_cla <- params_advanced$files$libraries$sop$merged$structures$taxonomies$cla
  fil_lib_sop_mer_str_tax_npc <- params_advanced$files$libraries$sop$merged$structures$taxonomies$npc
  fil_lib_spe_neg <- params_advanced$files$libraries$spectral$neg
  fil_lib_spe_pos <- params_advanced$files$libraries$spectral$pos
  fil_lib_spe_raw <- params_advanced$files$libraries$spectral$raw
  fil_lib_tem_exp_csv <- params_advanced$files$libraries$temporal$exp$csv
  fil_lib_tem_exp_mgf_neg <- params_advanced$files$libraries$temporal$exp$mgf$neg
  fil_lib_tem_exp_mgf_pos <- params_advanced$files$libraries$temporal$exp$mgf$pos
  fil_lib_tem_is_csv <- params_advanced$files$libraries$temporal$is$csv
  fil_lib_tem_is_mgf_neg <- params_advanced$files$libraries$temporal$is$mgf$neg
  fil_lib_tem_is_mgf_pos <- params_advanced$files$libraries$temporal$is$mgf$pos
  fil_lib_tem_pre <- params_advanced$files$libraries$temporal$prepared
  # fil_net_spe_edg_raw <- params_advanced$files$networks$spectral$edges$raw
  fil_net_spe_edg_raw_ms1 <- params_advanced$files$networks$spectral$edges$raw$ms1
  fil_net_spe_edg_raw_spe <- params_advanced$files$networks$spectral$edges$raw$spectral
  fil_net_spe_edg_pre <- params_advanced$files$networks$spectral$edges$prepared
  fil_net_spe_com_raw <- params_advanced$files$networks$spectral$components$raw
  fil_net_spe_com_pre <- params_advanced$files$networks$spectral$components$prepared
  # fil_met_raw <- params_advanced$files$metadata$raw
  fil_met_pre <- params_advanced$files$metadata$prepared
  # fil_spe_raw <- params_advanced$files$spectral$raw
  # gnps_id <- params_advanced$gnps$id
  # gnps_workflow <- params_advanced$gnps$workflow
  ms_add_neg <- params_advanced$ms$adducts$neg
  ms_add_pos <- params_advanced$ms$adducts$pos
  ms_clu_neg <- params_advanced$ms$clusters$neg
  ms_clu_pos <- params_advanced$ms$clusters$pos
  ms_neu <- params_advanced$ms$neutral_losses
  # ms_pol <- params_advanced$ms$polarity
  ms_thr_ms2_int <- params_advanced$ms$thresholds$ms2$intensity
  ms_tol_mas_ppm_ms1 <- params_advanced$ms$tolerances$mass$ppm$ms1
  ms_tol_mas_ppm_ms2 <- params_advanced$ms$tolerances$mass$ppm$ms2
  ms_tol_mas_dal_ms1 <- params_advanced$ms$tolerances$mass$dalton$ms1
  ms_tol_mas_dal_ms2 <- params_advanced$ms$tolerances$mass$dalton$ms2
  ms_tol_rt_add <- params_advanced$ms$tolerances$rt$adducts
  ms_tol_rt_lib <- params_advanced$ms$tolerances$rt$library
  names_extension <- params_advanced$names$extension
  names_features <- params_advanced$names$features
  names_filename <- params_advanced$names$filename
  names_inchikey <- params_advanced$names$inchikey
  names_libraries <- params_advanced$names$libraries
  names_mgf_ad <- params_advanced$names$mgf$adduct
  names_mgf_ce <- params_advanced$names$mgf$collision_energy
  names_mgf_ci <- params_advanced$names$mgf$compound_id
  names_mgf_em <- params_advanced$names$mgf$exact_mass
  names_mgf_in <- params_advanced$names$mgf$inchi
  names_mgf_io <- params_advanced$names$mgf$inchi_no_stereo
  names_mgf_ik <- params_advanced$names$mgf$inchikey
  names_mgf_il <- params_advanced$names$mgf$inchikey_connectivity_layer
  names_mgf_mf <- params_advanced$names$mgf$molecular_formula
  names_mgf_na <- params_advanced$names$mgf$name
  names_mgf_po <- params_advanced$names$mgf$polarity
  names_mgf_rt <- params_advanced$names$mgf$retention_time
  names_mgf_sm <- params_advanced$names$mgf$smiles
  names_mgf_sn <- params_advanced$names$mgf$smiles_no_stereo
  names_mgf_si <- params_advanced$names$mgf$spectrum_id
  names_mgf_sp <- params_advanced$names$mgf$splash
  names_mgf_sy <- params_advanced$names$mgf$synonyms
  names_mgf_xl <- params_advanced$names$mgf$xlogp
  names_precursor <- params_advanced$names$precursor
  names_rt_fea <- params_advanced$names$rt$features
  names_rt_lib <- params_advanced$names$rt$library
  names_smiles <- params_advanced$names$smiles
  names_source <- params_advanced$names$source
  names_target <- params_advanced$names$target
  names_taxon <- params_advanced$names$taxon
  org_can <- params_advanced$organisms$candidates
  org_fil_mod <- params_advanced$organisms$filter$mode
  org_fil_lev <- params_advanced$organisms$filter$level
  org_fil_val <- params_advanced$organisms$filter$value
  # org_tax <- params_advanced$organisms$taxon
  sim_met_ann <- params_advanced$similarities$methods$annotations
  sim_met_edg <- params_advanced$similarities$methods$edges
  sim_thr_ann <- params_advanced$similarities$thresholds$annotations
  sim_thr_edg <- params_advanced$similarities$thresholds$edges
  # too_met <- params_advanced$tools$metadata
  # too_net_spe_com <- params_advanced$tools$networks$spectral$components
  # too_net_spe_edg <- params_advanced$tools$networks$spectral$edges
  too_sir_ver <- params_advanced$tools$sirius$version
  # too_tax_bio <- params_advanced$tools$taxonomies$biological
  # too_tax_che <- params_advanced$tools$taxonomies$chemical
  units_rt <- params_advanced$units$rt
  wei_glo_bio <- params_advanced$weights$global$biological
  wei_glo_che <- params_advanced$weights$global$chemical
  wei_glo_spe <- params_advanced$weights$global$spectral
  wei_bio_01 <- params_advanced$weights$biological$domain
  wei_bio_02 <- params_advanced$weights$biological$kingdom
  wei_bio_03 <- params_advanced$weights$biological$phylum
  wei_bio_04 <- params_advanced$weights$biological$class
  wei_bio_05 <- params_advanced$weights$biological$order
  wei_bio_06 <- params_advanced$weights$biological$infraorder
  wei_bio_07 <- params_advanced$weights$biological$family
  wei_bio_08 <- params_advanced$weights$biological$subfamily
  wei_bio_09 <- params_advanced$weights$biological$tribe
  wei_bio_10 <- params_advanced$weights$biological$subtribe
  wei_bio_11 <- params_advanced$weights$biological$genus
  wei_bio_12 <- params_advanced$weights$biological$subgenus
  wei_bio_13 <- params_advanced$weights$biological$species
  wei_bio_14 <- params_advanced$weights$biological$subspecies
  wei_bio_15 <- params_advanced$weights$biological$variety
  wei_che_11 <- params_advanced$weights$chemical$cla$kingdom
  wei_che_21 <- params_advanced$weights$chemical$npc$pathway
  wei_che_12 <- params_advanced$weights$chemical$cla$superclass
  wei_che_22 <- params_advanced$weights$chemical$npc$superclass
  wei_che_13 <- params_advanced$weights$chemical$cla$class
  wei_che_23 <- params_advanced$weights$chemical$npc$class
  wei_che_14 <- params_advanced$weights$chemical$cla$parent
  opt_cpd_nam <- params_advanced$options$compounds_names
  # opt_hig_con <- params_advanced$options$high_confidence
  opt_for <- params_advanced$options$force
  opt_rem_tie <- params_advanced$options$remove_ties
  # opt_sum <- params_advanced$options$summarize

  log_debug(x = "Small params")
  fil_pat <- params_small$files$pattern
  fil_fea_raw <- params_small$files$features$raw
  fil_met_raw <- params_small$files$metadata$raw
  fil_spe_raw <- params_small$files$spectral$raw
  fil_ann_raw_sir <- params_small$files$annotations$raw$sirius
  ms_pol <- params_small$ms$polarity
  org_tax <- params_small$organisms$taxon
  opt_hig_con <- params_small$options$high_confidence
  opt_sum <- params_small$options$summarize

  log_debug(x = "Advanced params")
  yamls_params$`params/prepare_params_advanced`$files$pattern <- fil_pat
  yamls_params$`params/prepare_params_advanced`$files$features$raw <- fil_fea_raw
  yamls_params$`params/prepare_params_advanced`$files$metadata$raw <- fil_met_raw
  yamls_params$`params/prepare_params_advanced`$files$spectral$raw <- fil_spe_raw
  yamls_params$`params/prepare_params_advanced`$files$annotations$raw$sirius <- fil_ann_raw_sir
  yamls_params$`params/prepare_params_advanced`$ms$polarity <- ms_pol
  yamls_params$`params/prepare_params_advanced`$organisms$taxon <- org_tax
  yamls_params$`params/prepare_params_advanced`$options$high_confidence <- opt_hig_con
  yamls_params$`params/prepare_params_advanced`$options$summarize <- opt_sum

  if (!is.null(org_tax)) {
    if (org_tax == "") {
      org_tax <- NULL
    } else {
      fil_met_raw <- NULL
    }
  }

  log_debug(x = "Changing params")
  ## annotate_masses
  yamls_params$annotate_masses$files$annotations$prepared$structural <-
    fil_ann_pre_str
  yamls_params$annotate_masses$files$features$prepared <-
    fil_fea_pre
  yamls_params$annotate_masses$files$libraries$sop$merged$keys <-
    fil_lib_sop_mer_key
  yamls_params$annotate_masses$files$libraries$sop$merged$structures$stereo <-
    fil_lib_sop_mer_str_ste
  yamls_params$annotate_masses$files$libraries$sop$merged$structures$metadata <-
    fil_lib_sop_mer_str_met
  yamls_params$annotate_masses$files$libraries$sop$merged$structures$names <-
    fil_lib_sop_mer_str_nam
  yamls_params$annotate_masses$files$libraries$sop$merged$structures$taxonomies$cla <-
    fil_lib_sop_mer_str_tax_cla
  yamls_params$annotate_masses$files$libraries$sop$merged$structures$taxonomies$npc <-
    fil_lib_sop_mer_str_tax_npc
  yamls_params$annotate_masses$files$networks$spectral$edges$raw$ms1 <-
    fil_net_spe_edg_raw_ms1
  yamls_params$annotate_masses$ms$adducts$neg <-
    ms_add_neg
  yamls_params$annotate_masses$ms$adducts$pos <-
    ms_add_pos
  yamls_params$annotate_masses$ms$clusters$neg <-
    ms_clu_neg
  yamls_params$annotate_masses$ms$clusters$pos <-
    ms_clu_pos
  yamls_params$annotate_masses$ms$neutral_losses <-
    ms_neu
  yamls_params$annotate_masses$ms$polarity <-
    ms_pol
  yamls_params$annotate_masses$ms$tolerances$mass$ppm$ms1 <-
    ms_tol_mas_ppm_ms1
  yamls_params$annotate_masses$ms$tolerances$mass$dalton$ms1 <-
    ms_tol_mas_dal_ms1
  yamls_params$annotate_masses$ms$tolerances$rt$adducts <-
    ms_tol_rt_add
  yamls_params$annotate_masses$names$source <-
    names_source
  yamls_params$annotate_masses$names$target <-
    names_target
  yamls_params$annotate_masses$options$force <-
    opt_for

  ## annotate_spectra
  yamls_params$annotate_spectra$annotations$ms2approx <-
    ann_ms2_app
  yamls_params$annotate_spectra$files$annotations$raw$spectral$spectral <-
    fil_ann_raw_spe_spe
  yamls_params$annotate_spectra$files$libraries$spectral$neg <-
    fil_lib_spe_neg
  yamls_params$annotate_spectra$files$libraries$spectral$pos <-
    fil_lib_spe_pos
  yamls_params$annotate_spectra$files$spectral$raw <-
    fil_spe_raw
  yamls_params$annotate_spectra$ms$thresholds$ms2$intensity <-
    ms_thr_ms2_int
  yamls_params$annotate_spectra$ms$polarity <-
    ms_pol
  yamls_params$annotate_spectra$ms$tolerances$mass$ppm$ms2 <-
    ms_tol_mas_ppm_ms2
  yamls_params$annotate_spectra$ms$tolerances$mass$dalton$ms2 <-
    ms_tol_mas_dal_ms2
  yamls_params$annotate_spectra$similarities$methods$annotations <-
    sim_met_ann
  yamls_params$annotate_spectra$similarities$thresholds$annotations <-
    sim_thr_ann

  ## create_components
  yamls_params$create_components$files$networks$spectral$edges$prepared <-
    fil_net_spe_edg_pre
  yamls_params$create_components$files$networks$spectral$components$raw <-
    fil_net_spe_com_raw

  ## create_edges_spectra
  yamls_params$create_edges_spectra$files$networks$spectral$edges$raw$spectral <-
    fil_net_spe_edg_raw_spe
  yamls_params$create_edges_spectra$files$spectral$raw <-
    fil_spe_raw
  yamls_params$create_edges_spectra$ms$thresholds$ms2$intensity <-
    ms_thr_ms2_int
  yamls_params$create_edges_spectra$ms$tolerances$mass$ppm$ms2 <-
    ms_tol_mas_ppm_ms2
  yamls_params$create_edges_spectra$ms$tolerances$mass$dalton$ms2 <-
    ms_tol_mas_dal_ms2
  yamls_params$create_edges_spectra$names$source <-
    names_source
  yamls_params$create_edges_spectra$names$target <-
    names_target
  yamls_params$create_edges_spectra$similarities$methods$edges <-
    sim_met_edg
  yamls_params$create_edges_spectra$similarities$thresholds$edges <-
    sim_thr_edg

  ## filter_annotations
  yamls_params$filter_annotations$files$annotations$filtered <-
    fil_ann_fil
  yamls_params$filter_annotations$files$annotations$prepared$structural <-
    fil_ann_pre_str
  yamls_params$filter_annotations$files$features$prepared <-
    fil_fea_pre
  yamls_params$filter_annotations$files$libraries$temporal$prepared <-
    fil_lib_tem_pre
  yamls_params$filter_annotations$ms$tolerances$rt$library <-
    ms_tol_rt_lib

  ## prepare_annotations_gnps
  yamls_params$prepare_annotations_gnps$files$annotations$raw$spectral$gnps <-
    fil_ann_raw_spe_gnp
  yamls_params$prepare_annotations_gnps$files$annotations$prepared$structural$gnps <-
    fil_ann_pre_str_gnp
  yamls_params$prepare_annotations_gnps$files$libraries$sop$merged$structures$stereo <-
    fil_lib_sop_mer_str_ste
  yamls_params$prepare_annotations_gnps$files$libraries$sop$merged$structures$metadata <-
    fil_lib_sop_mer_str_met
  yamls_params$prepare_annotations_gnps$files$libraries$sop$merged$structures$names <-
    fil_lib_sop_mer_str_nam
  yamls_params$prepare_annotations_gnps$files$libraries$sop$merged$structures$taxonomies$cla <-
    fil_lib_sop_mer_str_tax_cla
  yamls_params$prepare_annotations_gnps$files$libraries$sop$merged$structures$taxonomies$npc <-
    fil_lib_sop_mer_str_tax_npc

  ## prepare_annotations_sirius
  yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius <-
    fil_ann_raw_sir
  yamls_params$prepare_annotations_sirius$files$annotations$prepared$canopus <-
    fil_ann_pre_can
  yamls_params$prepare_annotations_sirius$files$annotations$prepared$formula <-
    fil_ann_pre_for
  yamls_params$prepare_annotations_sirius$files$annotations$prepared$structural$sirius <-
    fil_ann_pre_str_sir
  yamls_params$prepare_annotations_sirius$files$libraries$sop$merged$structures$stereo <-
    fil_lib_sop_mer_str_ste
  yamls_params$prepare_annotations_sirius$files$libraries$sop$merged$structures$metadata <-
    fil_lib_sop_mer_str_met
  yamls_params$prepare_annotations_sirius$files$libraries$sop$merged$structures$names <-
    fil_lib_sop_mer_str_nam
  yamls_params$prepare_annotations_sirius$files$libraries$sop$merged$structures$taxonomies$cla <-
    fil_lib_sop_mer_str_tax_cla
  yamls_params$prepare_annotations_sirius$files$libraries$sop$merged$structures$taxonomies$npc <-
    fil_lib_sop_mer_str_tax_npc
  yamls_params$prepare_annotations_sirius$tools$sirius$version <-
    too_sir_ver

  ## prepare_annotations_spectra
  yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral$spectral <-
    fil_ann_raw_spe_spe
  yamls_params$prepare_annotations_spectra$files$annotations$prepared$structural$spectral <-
    fil_ann_pre_str_spe
  yamls_params$prepare_annotations_spectra$files$libraries$sop$merged$structures$stereo <-
    fil_lib_sop_mer_str_ste
  yamls_params$prepare_annotations_spectra$files$libraries$sop$merged$structures$metadata <-
    fil_lib_sop_mer_str_met
  yamls_params$prepare_annotations_spectra$files$libraries$sop$merged$structures$names <-
    fil_lib_sop_mer_str_nam
  yamls_params$prepare_annotations_spectra$files$libraries$sop$merged$structures$taxonomies$cla <-
    fil_lib_sop_mer_str_tax_cla
  yamls_params$prepare_annotations_spectra$files$libraries$sop$merged$structures$taxonomies$npc <-
    fil_lib_sop_mer_str_tax_npc

  ## prepare_features_components
  yamls_params$prepare_features_components$files$networks$spectral$components$raw <-
    fil_net_spe_com_raw
  yamls_params$prepare_features_components$files$networks$spectral$components$prepared <-
    fil_net_spe_com_pre

  ## prepare_features_edges
  yamls_params$prepare_features_edges$files$networks$spectral$edges$raw$ms1 <-
    fil_net_spe_edg_raw_ms1
  yamls_params$prepare_features_edges$files$networks$spectral$edges$raw$spectral <-
    fil_net_spe_edg_raw_spe
  yamls_params$prepare_features_edges$files$networks$spectral$edges$prepared <-
    fil_net_spe_edg_pre
  yamls_params$prepare_features_edges$names$source <-
    names_source
  yamls_params$prepare_features_edges$names$target <-
    names_target

  ## prepare_features_tables
  yamls_params$prepare_features_tables$files$features$raw <-
    fil_fea_raw
  yamls_params$prepare_features_tables$files$features$prepared <-
    fil_fea_pre
  yamls_params$prepare_features_tables$names$features <-
    names_features
  yamls_params$prepare_features_tables$names$precursor <-
    names_precursor
  yamls_params$prepare_features_tables$names$rt$features <-
    names_rt_fea

  ## prepare_libraries_rt
  yamls_params$prepare_libraries_rt$files$libraries$sop$prepared$rt <-
    fil_lib_sop_pre_rt
  yamls_params$prepare_libraries_rt$files$libraries$temporal$exp$csv <-
    fil_lib_tem_exp_csv
  yamls_params$prepare_libraries_rt$files$libraries$temporal$exp$mgf$neg <-
    fil_lib_tem_exp_mgf_neg
  yamls_params$prepare_libraries_rt$files$libraries$temporal$exp$mgf$pos <-
    fil_lib_tem_exp_mgf_pos
  yamls_params$prepare_libraries_rt$files$libraries$temporal$is$csv <-
    fil_lib_tem_is_csv
  yamls_params$prepare_libraries_rt$files$libraries$temporal$is$mgf$neg <-
    fil_lib_tem_is_mgf_neg
  yamls_params$prepare_libraries_rt$files$libraries$temporal$is$mgf$pos <-
    fil_lib_tem_is_mgf_pos
  yamls_params$prepare_libraries_rt$files$libraries$temporal$prepared <-
    fil_lib_tem_pre
  yamls_params$prepare_libraries_rt$names$inchikey <-
    names_inchikey
  yamls_params$prepare_libraries_rt$names$mgf$inchikey <-
    names_mgf_ik
  yamls_params$prepare_libraries_rt$names$mgf$retention_time <-
    names_mgf_rt
  yamls_params$prepare_libraries_rt$names$mgf$smiles <-
    names_mgf_sm
  yamls_params$prepare_libraries_rt$names$rt$library <-
    names_rt_lib
  yamls_params$prepare_libraries_rt$names$smiles <-
    names_smiles
  yamls_params$prepare_libraries_rt$units$rt <-
    units_rt

  ## prepare_libraries_sop_closed
  yamls_params$prepare_libraries_sop_closed$files$libraries$sop$raw$closed <-
    fil_lib_sop_raw_clo
  yamls_params$prepare_libraries_sop_closed$files$libraries$sop$prepared$closed <-
    fil_lib_sop_pre_clo

  ## prepare_libraries_sop_ecmdb
  yamls_params$prepare_libraries_sop_ecmdb$files$libraries$sop$raw$ecmdb <-
    fil_lib_sop_raw_ecm
  yamls_params$prepare_libraries_sop_ecmdb$files$libraries$sop$prepared$ecmdb <-
    fil_lib_sop_pre_ecm

  ## prepare_libraries_sop_hmdb
  yamls_params$prepare_libraries_sop_hmdb$files$libraries$sop$raw$hmdb <-
    fil_lib_sop_raw_hmd
  yamls_params$prepare_libraries_sop_hmdb$files$libraries$sop$prepared$hmdb <-
    fil_lib_sop_pre_hmd

  ## prepare_libraries_sop_lotus
  yamls_params$prepare_libraries_sop_lotus$files$libraries$sop$raw$lotus <-
    fil_lib_sop_raw_lot
  yamls_params$prepare_libraries_sop_lotus$files$libraries$sop$prepared$lotus <-
    fil_lib_sop_pre_lot

  ## prepare_libraries_sop_merged
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$prepared <-
    fil_lib_sop_pre
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$keys <-
    fil_lib_sop_mer_key
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$organisms$names <-
    fil_lib_sop_mer_org_nam
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$organisms$taxonomies$ott <-
    fil_lib_sop_mer_org_tax_ott
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$structures$stereo <-
    fil_lib_sop_mer_str_ste
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$structures$metadata <-
    fil_lib_sop_mer_str_met
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$structures$names <-
    fil_lib_sop_mer_str_nam
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$structures$taxonomies$cla <-
    fil_lib_sop_mer_str_tax_cla
  yamls_params$prepare_libraries_sop_merged$files$libraries$sop$merged$structures$taxonomies$npc <-
    fil_lib_sop_mer_str_tax_npc
  yamls_params$prepare_libraries_sop_merged$organisms$filter$mode <-
    org_fil_mod
  yamls_params$prepare_libraries_sop_merged$organisms$filter$level <-
    org_fil_lev
  yamls_params$prepare_libraries_sop_merged$organisms$filter$value <-
    org_fil_val

  ## prepare_libraries_spectra
  yamls_params$prepare_libraries_spectra$files$libraries$spectral$raw <-
    fil_lib_spe_raw
  yamls_params$prepare_libraries_spectra$names$libraries <-
    names_libraries
  yamls_params$prepare_libraries_spectra$names$mgf$adduct <-
    names_mgf_ad
  yamls_params$prepare_libraries_spectra$names$mgf$collision_energy <-
    names_mgf_ce
  yamls_params$prepare_libraries_spectra$names$mgf$compound_id <-
    names_mgf_ci
  yamls_params$prepare_libraries_spectra$names$mgf$exact_mass <-
    names_mgf_em
  yamls_params$prepare_libraries_spectra$names$mgf$inchi <-
    names_mgf_in
  yamls_params$prepare_libraries_spectra$names$mgf$inchi_no_stereo <-
    names_mgf_io
  yamls_params$prepare_libraries_spectra$names$mgf$inchikey <-
    names_mgf_ik
  yamls_params$prepare_libraries_spectra$names$mgf$inchikey_connectivity_layer <-
    names_mgf_il
  yamls_params$prepare_libraries_spectra$names$mgf$molecular_formula <-
    names_mgf_mf
  yamls_params$prepare_libraries_spectra$names$mgf$name <-
    names_mgf_na
  yamls_params$prepare_libraries_spectra$names$mgf$polarity <-
    names_mgf_po
  yamls_params$prepare_libraries_spectra$names$mgf$smiles <-
    names_mgf_sm
  yamls_params$prepare_libraries_spectra$names$mgf$smiles_no_stereo <-
    names_mgf_sn
  yamls_params$prepare_libraries_spectra$names$mgf$spectrum_id <-
    names_mgf_si
  yamls_params$prepare_libraries_spectra$names$mgf$splash <-
    names_mgf_sp
  yamls_params$prepare_libraries_spectra$names$mgf$synonyms <-
    names_mgf_sy
  yamls_params$prepare_libraries_spectra$names$mgf$xlogp <-
    names_mgf_xl

  ## prepare_taxa
  yamls_params$prepare_taxa$files$features$raw <-
    fil_fea_raw
  yamls_params$prepare_taxa$files$metadata$raw <-
    fil_met_raw
  yamls_params$prepare_taxa$files$metadata$prepared <-
    fil_met_pre
  yamls_params$prepare_taxa$files$libraries$sop$merged$organisms$taxonomies$ott <-
    fil_lib_sop_mer_org_tax_ott
  yamls_params$prepare_taxa$names$extension <-
    names_extension
  yamls_params$prepare_taxa$names$features <-
    names_features
  yamls_params$prepare_taxa$names$filename <-
    names_filename
  yamls_params$prepare_taxa$names$taxon <-
    names_taxon
  yamls_params$prepare_taxa$organisms$candidates <-
    org_can
  yamls_params$prepare_taxa$organisms$taxon <-
    org_tax

  ## weight_annotations
  yamls_params$weight_annotations$annotations$candidates$final <-
    ann_can_fin
  yamls_params$weight_annotations$annotations$ms1only <-
    ann_ms1only
  yamls_params$weight_annotations$annotations$thresholds$consistency <-
    ann_thr_con
  yamls_params$weight_annotations$annotations$thresholds$ms1$biological <-
    ann_thr_ms1_bio
  yamls_params$weight_annotations$annotations$thresholds$ms1$chemical <-
    ann_thr_ms1_che
  yamls_params$weight_annotations$annotations$thresholds$ms1$condition <-
    ann_thr_ms1_con
  yamls_params$weight_annotations$files$pattern <-
    fil_pat
  yamls_params$weight_annotations$files$annotations$filtered <-
    fil_ann_fil
  yamls_params$weight_annotations$files$annotations$prepared$canopus <-
    fil_ann_pre_can
  yamls_params$weight_annotations$files$annotations$prepared$formula <-
    fil_ann_pre_for
  yamls_params$weight_annotations$files$annotations$processed <-
    fil_ann_pro
  yamls_params$weight_annotations$files$libraries$sop$merged$keys <-
    fil_lib_sop_mer_key
  yamls_params$weight_annotations$files$libraries$sop$merged$organisms$taxonomies$ott <-
    fil_lib_sop_mer_org_tax_ott
  yamls_params$weight_annotations$files$libraries$sop$merged$structures$stereo <-
    fil_lib_sop_mer_str_ste
  yamls_params$weight_annotations$files$networks$spectral$components$prepared <-
    fil_net_spe_com_pre
  yamls_params$weight_annotations$files$networks$spectral$edges$prepared <-
    fil_net_spe_edg_pre
  yamls_params$weight_annotations$files$metadata$prepared <-
    fil_met_pre
  yamls_params$weight_annotations$weights$global$biological <-
    wei_glo_bio
  yamls_params$weight_annotations$weights$global$chemical <-
    wei_glo_che
  yamls_params$weight_annotations$weights$global$spectral <-
    wei_glo_spe
  yamls_params$weight_annotations$weights$biological$domain <-
    wei_bio_01
  yamls_params$weight_annotations$weights$biological$kingdom <-
    wei_bio_02
  yamls_params$weight_annotations$weights$biological$phylum <-
    wei_bio_03
  yamls_params$weight_annotations$weights$biological$class <-
    wei_bio_04
  yamls_params$weight_annotations$weights$biological$order <-
    wei_bio_05
  yamls_params$weight_annotations$weights$biological$infraorder <-
    wei_bio_06
  yamls_params$weight_annotations$weights$biological$family <-
    wei_bio_07
  yamls_params$weight_annotations$weights$biological$subfamily <-
    wei_bio_08
  yamls_params$weight_annotations$weights$biological$tribe <-
    wei_bio_09
  yamls_params$weight_annotations$weights$biological$subtribe <-
    wei_bio_10
  yamls_params$weight_annotations$weights$biological$genus <-
    wei_bio_11
  yamls_params$weight_annotations$weights$biological$subgenus <-
    wei_bio_12
  yamls_params$weight_annotations$weights$biological$species <-
    wei_bio_13
  yamls_params$weight_annotations$weights$biological$subspecies <-
    wei_bio_14
  yamls_params$weight_annotations$weights$biological$variety <-
    wei_bio_15
  yamls_params$weight_annotations$weights$chemical$cla$kingdom <-
    wei_che_11
  yamls_params$weight_annotations$weights$chemical$npc$pathway <-
    wei_che_12
  yamls_params$weight_annotations$weights$chemical$cla$superclass <-
    wei_che_13
  yamls_params$weight_annotations$weights$chemical$npc$superclass <-
    wei_che_14
  yamls_params$weight_annotations$weights$chemical$cla$class <-
    wei_che_21
  yamls_params$weight_annotations$weights$chemical$npc$class <-
    wei_che_22
  yamls_params$weight_annotations$weights$chemical$cla$parent <-
    wei_che_23
  yamls_params$weight_annotations$options$compounds_names <-
    opt_cpd_nam
  yamls_params$weight_annotations$options$force <-
    opt_for
  yamls_params$weight_annotations$options$high_confidence <-
    opt_hig_con
  yamls_params$weight_annotations$options$remove_ties <-
    opt_rem_tie
  yamls_params$weight_annotations$options$summarize <-
    opt_sum

  log_debug(x = "Changing filenames")
  ## annotate_masses
  yamls_params$annotate_masses$files$annotations$prepared$structural <-
    yamls_params$annotate_masses$files$annotations$prepared$structural |>
    purrr::map(.f = replace_id)
  yamls_params$annotate_masses$files$features$prepared <-
    yamls_params$annotate_masses$files$features$prepared |>
    purrr::map(.f = replace_id)
  yamls_params$annotate_masses$files$networks$spectral$edges$raw <-
    yamls_params$annotate_masses$files$networks$spectral$edges$raw |>
    purrr::map(.f = replace_id)

  ## annotate_spectra
  yamls_params$annotate_spectra$files$annotations$raw$spectral$spectral <-
    yamls_params$annotate_spectra$files$annotations$raw$spectral$spectral |>
    purrr::map(.f = replace_id)
  # yamls_params$annotate_spectra$files$spectral$raw <-
  #   yamls_params$annotate_spectra$files$spectral$raw |>
  #   purrr::map(.f = replace_id)

  ## create_edges_spectra
  yamls_params$create_edges_spectra$files$networks$spectral$edges$raw <-
    yamls_params$create_edges_spectra$files$networks$spectral$edges$raw |>
    purrr::map(.f = replace_id)
  # yamls_params$create_edges_spectra$files$spectral$raw <-
  #   yamls_params$create_edges_spectra$files$spectral$raw |>
  #   purrr::map(.f = replace_id)

  ## create_components
  yamls_params$create_components$files$networks$spectral$edges$prepared <-
    yamls_params$create_components$files$networks$spectral$edges$prepared |>
    purrr::map(.f = replace_id)
  yamls_params$create_components$files$networks$spectral$components$raw <-
    yamls_params$create_components$files$networks$spectral$components$raw |>
    purrr::map(.f = replace_id)

  ## filter_annotations
  yamls_params$filter_annotations$files$annotations$filtered <-
    yamls_params$filter_annotations$files$annotations$filtered |>
    purrr::map(.f = replace_id)
  yamls_params$filter_annotations$files$annotations$prepared$structural$gnps <-
    yamls_params$filter_annotations$files$annotations$prepared$structural$gnps |>
    purrr::map(.f = replace_id)
  yamls_params$filter_annotations$files$annotations$prepared$structural$ms1 <-
    yamls_params$filter_annotations$files$annotations$prepared$structural$ms1 |>
    purrr::map(.f = replace_id)
  yamls_params$filter_annotations$files$annotations$prepared$structural$sirius <-
    yamls_params$filter_annotations$files$annotations$prepared$structural$sirius |>
    purrr::map(.f = replace_id)
  yamls_params$filter_annotations$files$annotations$prepared$structural$spectral <-
    yamls_params$filter_annotations$files$annotations$prepared$structural$spectral |>
    purrr::map(.f = replace_id)
  yamls_params$filter_annotations$files$features$prepared <-
    yamls_params$filter_annotations$files$features$prepared |>
    purrr::map(.f = replace_id)

  ## prepare_features_tables
  # yamls_params$prepare_features_tables$files$features$raw <-
  #   yamls_params$prepare_features_tables$files$features$raw |>
  #   purrr::map(.f = replace_id)
  yamls_params$prepare_features_tables$files$features$prepared <-
    yamls_params$prepare_features_tables$files$features$prepared |>
    purrr::map(.f = replace_id)

  ## prepare_features_components
  yamls_params$prepare_features_components$files$networks$spectral$components$raw <-
    yamls_params$prepare_features_components$files$networks$spectral$components$raw |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_features_components$files$networks$spectral$components$prepared <-
    yamls_params$prepare_features_components$files$networks$spectral$components$prepared |>
    purrr::map(.f = replace_id)

  ## prepare_features_edges
  yamls_params$prepare_features_edges$files$networks$spectral$edges$raw$ms1 <-
    yamls_params$prepare_features_edges$files$networks$spectral$edges$raw$ms1 |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_features_edges$files$networks$spectral$edges$raw$spectral <-
    yamls_params$prepare_features_edges$files$networks$spectral$edges$raw$spectral |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_features_edges$files$networks$spectral$edges$prepared <-
    yamls_params$prepare_features_edges$files$networks$spectral$edges$prepared |>
    purrr::map(.f = replace_id)

  ## prepare_annotations_gnps
  yamls_params$prepare_annotations_gnps$files$annotations$raw$spectral$gnps <-
    yamls_params$prepare_annotations_gnps$files$annotations$raw$spectral$gnps |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_annotations_gnps$files$annotations$prepared$structural$gnps <-
    yamls_params$prepare_annotations_gnps$files$annotations$prepared$structural$gnps |>
    purrr::map(.f = replace_id)

  ## prepare_annotations_sirius
  # yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius <-
  #   yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius |>
  #   purrr::map(.f = replace_id)
  yamls_params$prepare_annotations_sirius$files$annotations$prepared$canopus <-
    yamls_params$prepare_annotations_sirius$files$annotations$prepared$canopus |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_annotations_sirius$files$annotations$prepared$formula <-
    yamls_params$prepare_annotations_sirius$files$annotations$prepared$formula |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_annotations_sirius$files$annotations$prepared$structural <-
    yamls_params$prepare_annotations_sirius$files$annotations$prepared$structural |>
    purrr::map(.f = replace_id)

  ## prepare_annotations_spectra
  yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral$spectral <-
    yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral$spectral |>
    purrr::map(.f = replace_id)
  yamls_params$prepare_annotations_spectra$files$annotations$prepared$structural <-
    yamls_params$prepare_annotations_spectra$files$annotations$prepared$structural |>
    purrr::map(.f = replace_id)

  ## prepare_taxa
  # yamls_params$prepare_taxa$files$features$raw <-
  #   yamls_params$prepare_taxa$files$features$raw |>
  #   purrr::map(.f = replace_id)
  # yamls_params$prepare_taxa$files$metadata$raw <-
  #   yamls_params$prepare_taxa$files$metadata$raw |>
  #   purrr::map(.f = replace_id)
  yamls_params$prepare_taxa$files$metadata$prepared <-
    yamls_params$prepare_taxa$files$metadata$prepared |>
    purrr::map(.f = replace_id)

  ## weight_annotations
  yamls_params$weight_annotations$files$annotations$filtered <-
    yamls_params$weight_annotations$files$annotations$filtered |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$annotations$prepared$canopus <-
    yamls_params$weight_annotations$files$annotations$prepared$canopus |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$annotations$prepared$formula <-
    yamls_params$weight_annotations$files$annotations$prepared$formula |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$annotations$processed <-
    yamls_params$weight_annotations$files$annotations$processed |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$features$prepared <-
    yamls_params$weight_annotations$files$features$prepared |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$networks$spectral$components$prepared <-
    yamls_params$weight_annotations$files$networks$spectral$components$prepared |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$networks$spectral$edges$prepared <-
    yamls_params$weight_annotations$files$networks$spectral$edges$prepared |>
    purrr::map(.f = replace_id)
  yamls_params$weight_annotations$files$metadata$prepared <-
    yamls_params$weight_annotations$files$metadata$prepared |>
    purrr::map(.f = replace_id)

  ## advanced
  # yamls_params$`params/prepare_params_advanced`$files$annotations$raw$spectral$gnps <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$raw$spectral$gnps |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$raw$spectral$spectral <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$raw$spectral$spectral |>
  #   purrr::map(.f = replace_id)
  # # yamls_params$`params/prepare_params_advanced`$files$annotations$raw$sirius <-
  # #   yamls_params$`params/prepare_params_advanced`$files$annotations$raw$sirius |>
  # #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$raw$spectral$spectral <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$raw$spectral$spectral |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural$gnps <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural$gnps |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural$ms1 <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural$ms1 |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural$sirius <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural$sirius |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$canopus <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$canopus |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$formula <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$formula |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$prepared$structural |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$filtered <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$filtered |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$annotations$processed <-
  #   yamls_params$`params/prepare_params_advanced`$files$annotations$processed |>
  #   purrr::map(.f = replace_id)
  # # yamls_params$`params/prepare_params_advanced`$files$features$raw <-
  # #   yamls_params$`params/prepare_params_advanced`$files$features$raw |>
  # #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$features$prepared <-
  #   yamls_params$`params/prepare_params_advanced`$files$features$prepared |>
  #   purrr::map(.f = replace_id)
  # # yamls_params$`params/prepare_params_advanced`$files$metadata$raw <-
  # #   yamls_params$`params/prepare_params_advanced`$files$metadata$raw |>
  # #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$metadata$prepared <-
  #   yamls_params$`params/prepare_params_advanced`$files$metadata$prepared |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$networks$spectral$components$raw <-
  #   yamls_params$`params/prepare_params_advanced`$files$networks$spectral$components$raw |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$networks$spectral$components$prepared <-
  #   yamls_params$`params/prepare_params_advanced`$files$networks$spectral$components$prepared |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$raw <-
  #   yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$raw |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$raw$ms1 <-
  #   yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$raw$ms1 |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$raw$spectral <-
  #   yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$raw$spectral |>
  #   purrr::map(.f = replace_id)
  # yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$prepared <-
  #   yamls_params$`params/prepare_params_advanced`$files$networks$spectral$edges$prepared |>
  #   purrr::map(.f = replace_id)
  # # yamls_params$`params/prepare_params_advanced`$files$spectral$raw <-
  # #   yamls_params$`params/prepare_params_advanced`$files$spectral$raw |>
  # #   purrr::map(.f = replace_id)

  yaml_export <- list$yaml_files |>
    gsub(
      pattern = "default",
      replacement = "user",
      fixed = TRUE
    )
  names(yaml_export) <- list$yaml_names

  if (!is.na(step)) {
    ## The dollar is for steps having similar names separated by underscores
    yamls_params <-
      yamls_params[grepl(
        pattern = paste0(step[[1]], "$"),
        x = names(yamls_params),
        perl = TRUE
      )]
    ## The dot is for steps having similar names separated by underscores
    yaml_export <-
      yaml_export[grepl(
        pattern = paste0(step[[1]], "\\."),
        x = yaml_export,
        perl = TRUE
      )]
  }

  log_debug(x = "Exporting params ...")
  create_dir(export = yaml_export[[1]])
  purrr::map(
    .x = seq_along(yamls_params),
    .f = function(x) {
      yaml::write_yaml(x = yamls_params[[x]], file = yaml_export[x])
    }
  )
  return(yaml_export)
}
