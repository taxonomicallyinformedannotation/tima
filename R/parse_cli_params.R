#' @title Parse CLI parameters
#'
#' @description This function parses command line parameters
#'
#' @param arguments CLI arguments
#' @param parameters Parameters
#'
#' @return Parameters coming from the CLI
#'
#' @examples NULL
parse_cli_params <- function(arguments, parameters) {
  mappings <- list(
    ann_can_fin = list(
      path = c("annotations", "candidates", "final"),
      type = as.numeric
    ),
    ann_ms1only = list(path = c("annotations", "ms1only"), type = as.logical),
    ann_ms2_app = list(
      path = c("annotations", "ms2approx"),
      type = as.logical
    ),
    ann_thr_con = list(
      path = c("annotations", "thresholds", "consistency"),
      type = as.numeric
    ),
    ann_thr_ms1_bio = list(
      path = c("annotations", "thresholds", "ms1", "biological"),
      type = as.numeric
    ),
    ann_thr_ms1_che = list(
      path = c("annotations", "thresholds", "ms1", "chemical"),
      type = as.numeric
    ),
    ann_thr_ms1_con = list(
      path = c("annotations", "thresholds", "ms1", "condition"),
      type = as.character
    ),
    ann_thr_ms2_sim_ann = list(
      path = c(
        "annotations",
        "thresholds",
        "ms2",
        "similarity",
        "annotation"
      ),
      type = as.numeric
    ),
    ann_thr_ms2_sim_edg = list(
      path = c("annotations", "thresholds", "ms2", "similarity", "edges"),
      type = as.numeric
    ),
    fil_pat = list(path = c("files", "pattern"), type = as.character),
    # fil_ann_raw_spe = list(path = c("files", "annotations", "raw", "spectral"),
    #                        type = as.character),
    fil_ann_raw_spe_gnp = list(
      path = c("files", "annotations", "raw", "spectral", "gnps"),
      type = as.character
    ),
    fil_ann_raw_spe_spe = list(
      path = c("files", "annotations", "raw", "spectral", "spectral"),
      type = as.character
    ),
    fil_ann_raw_sir = list(
      path = c("files", "annotations", "raw", "sirius"),
      type = as.character
    ),
    fil_ann_fil = list(
      path = c("files", "annotations", "filtered"),
      type = as.character
    ),
    fil_ann_pre_can = list(
      path = c("files", "annotations", "prepared", "canopus"),
      type = as.character
    ),
    fil_ann_pre_for = list(
      path = c("files", "annotations", "prepared", "formula"),
      type = as.character
    ),
    fil_ann_pre_str = list(
      path = c("files", "annotations", "prepared", "structural"),
      type = as.character
    ),
    fil_ann_pro = list(
      path = c("files", "annotations", "processed"),
      type = as.character
    ),
    fil_fea_raw = list(
      path = c("files", "features", "raw"),
      type = as.character
    ),
    fil_fea_pre = list(
      path = c("files", "features", "prepared"),
      type = as.character
    ),
    fil_lib_sop_raw_clo = list(
      path = c("files", "libraries", "sop", "raw", "closed"),
      type = as.character
    ),
    fil_lib_sop_raw_ecm = list(
      path = c("files", "libraries", "sop", "raw", "ecmdb"),
      type = as.character
    ),
    fil_lib_sop_raw_hmd = list(
      path = c("files", "libraries", "sop", "raw", "hmdb"),
      type = as.character
    ),
    fil_lib_sop_raw_lot = list(
      path = c("files", "libraries", "sop", "raw", "lotus"),
      type = as.character
    ),
    fil_lib_sop_pre = list(
      path = c("files", "libraries", "sop", "prepared"),
      type = as.character
    ),
    fil_lib_spe_neg = list(
      path = c("files", "libraries", "spectral", "neg"),
      type = as.character
    ),
    fil_lib_spe_pos = list(
      path = c("files", "libraries", "spectral", "pos"),
      type = as.character
    ),
    fil_lib_spe_raw = list(
      path = c("files", "libraries", "spectral", "raw"),
      type = as.character
    ),
    fil_lib_tem_exp_csv = list(
      path = c("files", "libraries", "temporal", "exp", "csv"),
      type = as.character
    ),
    fil_lib_tem_exp_mgf_neg = list(
      path = c("files", "libraries", "temporal", "exp", "mgf", "neg"),
      type = as.character
    ),
    fil_lib_tem_exp_mgf_pos = list(
      path = c("files", "libraries", "temporal", "exp", "mgf", "pos"),
      type = as.character
    ),
    fil_lib_tem_is_csv = list(
      path = c("files", "libraries", "temporal", "is", "csv"),
      type = as.character
    ),
    fil_lib_tem_is_mgf_neg = list(
      path = c("files", "libraries", "temporal", "is", "mgf", "neg"),
      type = as.character
    ),
    fil_lib_tem_is_mgf_pos = list(
      path = c("files", "libraries", "temporal", "is", "mgf", "pos"),
      type = as.character
    ),
    fil_lib_tem_pre = list(
      path = c("files", "libraries", "temporal", "prepared"),
      type = as.character
    ),
    fil_net_spe_edg_raw = list(
      path = c("files", "networks", "spectral", "edges", "raw"),
      type = as.character
    ),
    fil_net_spe_edg_pre = list(
      path = c("files", "networks", "spectral", "edges", "prepared"),
      type = as.character
    ),
    fil_net_spe_com_raw = list(
      path = c("files", "networks", "spectral", "components", "raw"),
      type = as.character
    ),
    fil_net_spe_com_pre = list(
      path = c("files", "networks", "spectral", "components", "prepared"),
      type = as.character
    ),
    fil_met_raw = list(
      path = c("files", "metadata", "raw"),
      type = as.character
    ),
    fil_met_pre = list(
      path = c("files", "metadata", "prepared"),
      type = as.character
    ),
    fil_spe_raw = list(
      path = c("files", "spectral", "raw"),
      type = as.character
    ),
    gnps_id = list(path = c("gnps", "id"), type = as.character),
    gnps_workflow = list(path = c("gnps", "workflow"), type = as.character),
    ms_add_neg = list(path = c("ms", "adducts", "neg"), type = as.character),
    ms_add_pos = list(path = c("ms", "adducts", "pos"), type = as.character),
    ms_clu_neg = list(path = c("ms", "clusters", "neg"), type = as.character),
    ms_clu_pos = list(path = c("ms", "clusters", "pos"), type = as.character),
    ms_neu = list(path = c("ms", "neutral_losses"), type = as.character),
    ms_pol = list(path = c("ms", "polarity"), type = as.character),
    ms_thr_ms2_int = list(
      path = c("ms", "thresholds", "ms2", "intensity"),
      type = as.numeric
    ),
    ms_tol_mas_ppm_ms1 = list(
      path = c("ms", "tolerances", "mass", "ppm", "ms1"),
      type = as.numeric
    ),
    ms_tol_mas_ppm_ms2 = list(
      path = c("ms", "tolerances", "mass", "ppm", "ms2"),
      type = as.numeric
    ),
    ms_tol_mas_dal_ms1 = list(
      path = c("ms", "tolerances", "mass", "dalton", "ms1"),
      type = as.numeric
    ),
    ms_tol_mas_dal_ms2 = list(
      path = c("ms", "tolerances", "mass", "dalton", "ms2"),
      type = as.numeric
    ),
    ms_tol_rt_add = list(
      path = c("ms", "tolerances", "rt", "adducts"),
      type = as.numeric
    ),
    ms_tol_rt_lib = list(
      path = c("ms", "tolerances", "rt", "library"),
      type = as.numeric
    ),
    names_adduct = list(path = c("names", "adduct"), type = as.character),
    names_extension = list(path = c("names", "extension"), type = as.character),
    names_features = list(path = c("names", "features"), type = as.character),
    names_filename = list(path = c("names", "filename"), type = as.character),
    names_inchikey = list(path = c("names", "inchikey"), type = as.character),
    names_lib = list(path = c("names", "libraries"), type = as.character),
    names_mgf_ad = list(path = c("names", "mgf", "adduct"), type = as.character),
    names_mgf_ce = list(
      path = c("names", "mgf", "collision_energy"),
      type = as.character
    ),
    names_mgf_ci = list(
      path = c("names", "mgf", "compound_id"),
      type = as.character
    ),
    names_mgf_em = list(
      path = c("names", "mgf", "exact_mass"),
      type = as.character
    ),
    names_mgf_in = list(path = c("names", "mgf", "inchi"), type = as.character),
    names_mgf_io = list(
      path = c("names", "mgf", "inchi_no_stereo"),
      type = as.character
    ),
    names_mgf_ik = list(
      path = c("names", "mgf", "inchikey"),
      type = as.character
    ),
    names_mgf_il = list(
      path = c("names", "mgf", "inchikey_no_stereo"),
      type = as.character
    ),
    names_mgf_mf = list(
      path = c("names", "mgf", "molecular_formula"),
      type = as.character
    ),
    names_mgf_na = list(path = c("names", "mgf", "name"), type = as.character),
    names_mgf_po = list(
      path = c("names", "mgf", "polarity"),
      type = as.character
    ),
    # names_mgf_pc = list(path = c("names", "mgf", "precursor_charge"), type = as.character),
    # names_mgf_pm = list(path = c("names", "mgf", "precursor_mz"), type = as.character),
    names_mgf_sm = list(path = c("names", "mgf", "smiles"), type = as.character),
    names_mgf_sn = list(
      path = c("names", "mgf", "smiles_no_stereo"),
      type = as.character
    ),
    names_mgf_si = list(
      path = c("names", "mgf", "spectrum_id"),
      type = as.character
    ),
    names_mgf_sp = list(path = c("names", "mgf", "splash"), type = as.character),
    names_mgf_sy = list(
      path = c("names", "mgf", "synonyms"),
      type = as.character
    ),
    names_mgf_xl = list(path = c("names", "mgf", "xlogp"), type = as.character),
    names_precursor = list(path = c("names", "precursor"), type = as.character),
    names_rt_fea = list(path = c("names", "rt", "features"), type = as.character),
    names_rt_lib = list(path = c("names", "rt", "library"), type = as.character),
    names_smiles = list(path = c("names", "smiles"), type = as.character),
    names_source = list(path = c("names", "source"), type = as.character),
    names_target = list(path = c("names", "target"), type = as.character),
    names_taxon = list(path = c("names", "taxon"), type = as.character),
    org_can = list(
      path = c("organisms", "candidates"),
      type = as.integer
    ),
    org_fil_mod = list(
      path = c("organisms", "filter", "mode"),
      type = as.logical
    ),
    org_fil_lev = list(
      path = c("organisms", "filter", "level"),
      type = as.character
    ),
    org_fil_val = list(
      path = c("organisms", "filter", "value"),
      type = as.character
    ),
    org_tax = list(path = c("organisms", "taxon"), type = as.character),
    too_met = list(path = c("tools", "metadata"), type = as.character),
    too_net_spe_com = list(
      path = c("tools", "networks", "spectral", "components"),
      type = as.character
    ),
    too_net_spe_edg = list(
      path = c("tools", "networks", "spectral", "edges"),
      type = as.character
    ),
    too_sir_ver = list(
      path = c("tools", "sirius", "version"),
      type = as.integer
    ),
    too_tax_bio = list(
      path = c("tools", "taxonomies", "biological"),
      type = as.character
    ),
    too_tax_che = list(
      path = c("tools", "taxonomies", "chemical"),
      type = as.character
    ),
    units_rt = list(path = c("units", "rt"), type = as.character),
    wei_glo_bio = list(
      path = c("weights", "global", "biological"),
      type = as.numeric
    ),
    wei_glo_che = list(
      path = c("weights", "global", "chemical"),
      type = as.numeric
    ),
    wei_glo_spe = list(
      path = c("weights", "global", "spectral"),
      type = as.numeric
    ),
    wei_bio_01 = list(
      path = c("weights", "biological", "domain"),
      type = as.numeric
    ),
    wei_bio_02 = list(
      path = c("weights", "biological", "kingdom"),
      type = as.numeric
    ),
    wei_bio_03 = list(
      path = c("weights", "biological", "phylum"),
      type = as.numeric
    ),
    wei_bio_04 = list(
      path = c("weights", "biological", "class"),
      type = as.numeric
    ),
    wei_bio_05 = list(
      path = c("weights", "biological", "order"),
      type = as.numeric
    ),
    wei_bio_06 = list(
      path = c("weights", "biological", "infraorder"),
      type = as.numeric
    ),
    wei_bio_07 = list(
      path = c("weights", "biological", "family"),
      type = as.numeric
    ),
    wei_bio_08 = list(
      path = c("weights", "biological", "subfamily"),
      type = as.numeric
    ),
    wei_bio_09 = list(
      path = c("weights", "biological", "tribe"),
      type = as.numeric
    ),
    wei_bio_10 = list(
      path = c("weights", "biological", "subtribe"),
      type = as.numeric
    ),
    wei_bio_11 = list(
      path = c("weights", "biological", "genus"),
      type = as.numeric
    ),
    wei_bio_12 = list(
      path = c("weights", "biological", "subgenus"),
      type = as.numeric
    ),
    wei_bio_13 = list(
      path = c("weights", "biological", "species"),
      type = as.numeric
    ),
    wei_bio_14 = list(
      path = c("weights", "biological", "subspecies"),
      type = as.numeric
    ),
    wei_bio_15 = list(
      path = c("weights", "biological", "variety"),
      type = as.numeric
    ),
    wei_che11 = list(
      path = c("weights", "chemical", "cla", "kingdom"),
      type = as.numeric
    ),
    wei_che12 = list(
      path = c("weights", "chemical", "cla", "superclass"),
      type = as.numeric
    ),
    wei_che13 = list(
      path = c("weights", "chemical", "cla", "class"),
      type = as.numeric
    ),
    wei_che14 = list(
      path = c("weights", "chemical", "cla", "parent"),
      type = as.numeric
    ),
    wei_che21 = list(
      path = c("weights", "chemical", "npc", "pathway"),
      type = as.numeric
    ),
    wei_che22 = list(
      path = c("weights", "chemical", "npc", "superclass"),
      type = as.numeric
    ),
    wei_che23 = list(
      path = c("weights", "chemical", "npc", "class"),
      type = as.numeric
    ),
    compounds_names = list(
      path = c("options", "compounds_names"),
      type = as.logical
    ),
    force = list(path = c("options", "force"), type = as.logical),
    high_confidence = list(
      path = c("options", "high_confidence"),
      type = as.logical
    ),
    remove_ties = list(path = c("options", "remove_ties"), type = as.logical),
    summarize = list(path = c("options", "summarize"), type = as.logical)
  )

  parameters <- names(mappings) |>
    purrr::reduce(
      .f = function(parameters, arg_name) {
        if (!is.null(arguments[[arg_name]])) {
          parameters |>
            purrr::modify_in(mappings[[arg_name]]$path, ~ mappings[[arg_name]]$type(arguments[[arg_name]]))
        } else {
          parameters
        }
      },
      .init = parameters
    )
  return(parameters)
}
