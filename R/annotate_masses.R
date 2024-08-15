#' @title Annotate masses
#'
#' @description This function annotates a feature table based on exact mass
#' match. It requires a structural library, its metadata, and lists of adducts,
#' clusters, and neutral losses to be considered. The polarity has to be `pos`
#' or `neg` and retention time and mass tolerances should be given. The feature
#' table is expected to be pre-formatted.
#'
#' @include calculate_mass_of_m.R
#' @include decorate_masses.R
#' @include dist_groups.R
#' @include get_params.R
#' @include harmonize_adducts.R
#' @include log_pipe.R
#' @include round_reals.R
#'
#' @param features Table containing your previous annotation to complement
#' @param output_annotations Output for mass based structural annotations
#' @param output_edges Output for mass based edges
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param library Library containing the keys
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param adducts_list List of adducts to be used
#' @param clusters_list List of clusters to be used
#' @param neutral_losses_list List of neutral losses to be used
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param tolerance_ppm Tolerance to perform annotation. Should be <= 20 ppm
#' @param tolerance_rt Tolerance to group adducts. Should be <= 0.05 minutes
#'
#' @return The path to the files containing MS1 annotations and edges
#'
#' @export
#'
#' @examples
#' tima:::copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' data_interim <- "data/interim/"
#' dir <- paste0(github, repo)
#' dir <- paste0(dir, data_interim)
#' annotate_masses(
#'   features = paste0(dir, "features/example_features.tsv"),
#'   library = paste0(dir, "libraries/sop/merged/keys.tsv"),
#'   str_stereo = paste0(dir, "libraries/sop/merged/structures/stereo.tsv"),
#'   str_met = paste0(dir, "libraries/sop/merged/structures/metadata.tsv"),
#'   str_nam = paste0(dir, "libraries/sop/merged/structures/names.tsv"),
#'   str_tax_cla = paste0(dir, "libraries/sop/merged/structures/taxonomies/classyfire.tsv"),
#'   str_tax_npc = paste0(dir, "libraries/sop/merged/structures/taxonomies/npc.tsv")
#' )
#' unlink("data", recursive = TRUE)
annotate_masses <-
  function(features = get_params(step = "annotate_masses")$files$features$prepared,
           output_annotations = get_params(step = "annotate_masses")$files$annotations$prepared$structural$ms1,
           output_edges = get_params(step = "annotate_masses")$files$networks$spectral$edges$raw,
           name_source = get_params(step = "annotate_masses")$names$source,
           name_target = get_params(step = "annotate_masses")$names$target,
           library = get_params(step = "annotate_masses")$files$libraries$sop$merged$keys,
           str_stereo = get_params(step = "annotate_masses")$files$libraries$sop$merged$structures$stereo,
           str_met = get_params(step = "annotate_masses")$files$libraries$sop$merged$structures$metadata,
           str_nam = get_params(step = "annotate_masses")$files$libraries$sop$merged$structures$names,
           str_tax_cla = get_params(step = "annotate_masses")$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = get_params(step = "annotate_masses")$files$libraries$sop$merged$structures$taxonomies$npc,
           adducts_list = get_params(step = "annotate_masses")$ms$adducts,
           clusters_list = get_params(step = "annotate_masses")$ms$clusters,
           neutral_losses_list = get_params(step = "annotate_masses")$ms$neutral_losses,
           ms_mode = get_params(step = "annotate_masses")$ms$polarity,
           tolerance_ppm = get_params(step = "annotate_masses")$ms$tolerances$mass$ppm$ms1,
           tolerance_rt = get_params(step = "annotate_masses")$ms$tolerances$rt$adducts) {
    stopifnot("Your ppm tolerance must be <= 20" = tolerance_ppm <= 20)
    stopifnot("Your rt tolerance must be <= 0.05" = tolerance_rt <= 0.05)

    features_table <- tidytable::fread(
      file = features,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    if (ms_mode == "pos") {
      adducts <- adducts_list$pos
      clusters <- clusters_list$pos
    }
    if (ms_mode == "neg") {
      adducts <- adducts_list$neg
      clusters <- clusters_list$neg
    }

    structure_organism_pairs_table <-
      tidytable::fread(
        file = library,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::left_join(tidytable::fread(
        file = str_stereo,
        na.strings = c("", "NA"),
        colClasses = "character"
      )) |>
      tidytable::left_join(tidytable::fread(
        file = str_met,
        na.strings = c("", "NA"),
        colClasses = "character"
      )) |>
      tidytable::left_join(tidytable::fread(
        file = str_nam,
        na.strings = c("", "NA"),
        colClasses = "character"
      )) |>
      tidytable::left_join(tidytable::fread(
        file = str_tax_cla,
        na.strings = c("", "NA"),
        colClasses = "character"
      )) |>
      tidytable::left_join(tidytable::fread(
        file = str_tax_npc,
        na.strings = c("", "NA"),
        colClasses = "character"
      )) |>
      tidytable::filter(!is.na(structure_exact_mass)) |>
      tidytable::mutate(tidytable::across(.cols = c("structure_exact_mass"), .fns = as.numeric)) |>
      round_reals()

    log_debug("filtering desired adducts and adding mz tolerance \n")
    df_add_em <- structure_organism_pairs_table |>
      tidytable::filter(!is.na(structure_exact_mass)) |>
      tidytable::distinct(exact_mass = structure_exact_mass) |>
      tidytable::mutate(tidytable::across(.cols = c("exact_mass"), .fns = as.numeric)) |>
      tidytable::mutate(
        value_min = exact_mass - (1E-6 * tolerance_ppm * exact_mass),
        value_max = exact_mass + (1E-6 * tolerance_ppm * exact_mass)
      ) |>
      tidytable::filter(!is.na(value_min)) |>
      tidytable::filter(value_min > 0)

    if (!"adduct" %in% colnames(features_table)) {
      log_debug("No previously attributed adducts detected")
      features_table$adduct <- NA_character_
    } else {
      log_debug(
        "Already",
        nrow(features_table |>
          tidytable::filter(!is.na(adduct))),
        "adducts previously detected"
      )
      # TODO this should be externalized
      adducts_translations <-
        c(
          "-2H" = "-H2",
          # cliqueMS
          "-3H" = "-H3",
          # cliqueMS
          "-2H2O" = "-H4O2 (2xH2O)",
          # mzmine
          "-3H2O" = "-H6O3 (3xH2O)",
          # mzmine
          "-4H2O" = "-H8O4 (4xH2O)",
          # mzmine
          "-5H2O" = "-H10O5 (5xH2O)",
          # mzmine
          "-NH3" = "+H3N",
          # mzmine
          "+2H" = "+H2",
          # mzmine
          "+2K" = "+K2",
          # cliqueMS
          "+2Na" = "+Na2",
          # mzmine
          "+3K" = "+K3",
          # cliqueMS
          "+3Na" = "+Na3",
          # cliqueMS
          "+Acetate" = "+C2H3O2",
          # mzmine
          "+ACN" = "+C2H3N",
          # mzmine
          "+CH3COO" = "+C2H3O2",
          # GNPS
          "+FA" = "+CHO2",
          # mzmine
          "+HAc" = "+C2H4O2",
          # mzmine
          "+Hac" = "+C2H4O2",
          # GNPS
          "+HFA" = "+CH2O2",
          # mzmine
          "+IsoProp" = "+C3H8O",
          # mzmine
          "+MeOH" = "+CH4O",
          # mzmine
          "+NH4" = "+H4N",
          # mzmine
          "[M+CH3COO]-/[M-CH3]-" = "[M+CH3COO]-" # weird MassBank
        )
      features_table <- features_table |>
        harmonize_adducts()
    }

    df_fea_min <- features_table |>
      tidytable::mutate(tidytable::across(.cols = c("mz"), .fns = as.numeric)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)

    if (any(names(features_table) == "rt")) {
      df_fea_min <- df_fea_min |>
        tidytable::mutate(tidytable::across(.cols = c("rt"), .fns = as.numeric))
    } else {
      df_fea_min[, "rt"] <- df_fea_min[, "feature_id"] |>
        as.numeric()
    }

    log_debug("calculating rt tolerance ... \n")
    df_rt_tol <- df_fea_min |>
      tidytable::mutate(
        rt_min = as.numeric(rt - tolerance_rt),
        rt_max = as.numeric(rt + tolerance_rt)
      )

    log_debug("joining within given rt tolerance \n")
    df_couples_diff <- df_rt_tol |>
      dplyr::inner_join(df_fea_min, by = dplyr::join_by(rt_min <= rt, rt_max >= rt)) |>
      tidytable::distinct(
        feature_id = feature_id.x,
        rt = rt.x,
        mz = mz.x,
        adduct = adduct.x,
        feature_id_dest = feature_id.y,
        mz_dest = mz.y,
        adduct_dest = adduct.y
      ) |>
      tidytable::select(tidyselect::everything(), feature_id_dest, mz_dest, adduct_dest) |>
      tidytable::filter(feature_id != feature_id_dest) |>
      log_pipe("adding delta mz tolerance for single charge adducts \n") |>
      tidytable::filter(mz_dest >= mz) |>
      tidytable::mutate(
        delta_min = (mz_dest - (1E-6 * tolerance_ppm * (mz + mz_dest) / 2) - mz),
        delta_max = (mz_dest + (1E-6 * tolerance_ppm * (mz + mz_dest) / 2) - mz)
      )

    rm(df_rt_tol, features_table)

    adducts_table <- adducts |>
      tidytable::tidytable() |>
      tidytable::rename(adduct = 1)
    clusters_table <- clusters |>
      tidytable::tidytable() |>
      tidytable::rename(cluster = 1)

    log_debug("forming adducts and clusters \n")
    add_clu_table <- adducts_table |>
      tidytable::mutate(join = "x") |>
      tidytable::left_join(clusters_table |>
        tidytable::mutate(join = "x")) |>
      tidytable::bind_rows(adducts_table) |>
      tidytable::mutate(adduct = ifelse(
        test = !is.na(cluster),
        yes = paste0(
          adduct |> gsub(
            pattern = "M(?![a-z]).*",
            replacement = "M",
            perl = TRUE
          ),
          "+",
          cluster,
          adduct |> gsub(
            pattern = ".*M(?![a-z])",
            replacement = "",
            perl = TRUE
          )
        ),
        no = adduct
      )) |>
      # Single charge monomers only
      tidytable::filter(grepl(
        pattern = "[M",
        x = adduct,
        fixed = TRUE
      )) |>
      tidytable::filter(grepl(pattern = "](\\+|\\-)", x = adduct)) |>
      tidytable::mutate_rowwise(adduct_mass = -1 * calculate_mass_of_m(adduct_string = adduct, mz = 0))

    rm(clusters_table)

    # TODO add safety if no monocharged?

    log_debug("calculating delta mz for single charge adducts and clusters \n")
    differences <-
      dist_groups(
        d = stats::dist(add_clu_table$adduct_mass),
        g = add_clu_table$adduct
      ) |>
      tidytable::mutate(
        Group1 = Label |>
          gsub(
            pattern = "Between ",
            replacement = "",
            fixed = TRUE
          ) |>
          gsub(pattern = " and .*", replacement = ""),
        Group2 = Label |>
          gsub(pattern = ".* and ", replacement = "")
      ) |>
      tidytable::select(-Item1, -Item2, -Label) |>
      ## remove redundancy among clusters
      ## Keep proton first
      tidytable::mutate(l = stringi::stri_length(Group1)) |>
      tidytable::arrange(l) |>
      tidytable::select(-l) |>
      tidytable::distinct(Distance, .keep_all = TRUE)

    neutral_losses <- neutral_losses_list |>
      tidytable::tidytable() |>
      tidytable::rename(loss = 1) |>
      tidytable::mutate_rowwise(mass = loss |>
        gsub(pattern = " .*", replacement = "") |>
        MetaboCoreUtils::calculateMass())

    log_debug("joining within given delta mz tolerance (neutral losses) \n")
    df_nl <- df_couples_diff |>
      dplyr::inner_join(neutral_losses, by = dplyr::join_by(delta_min <= mass, delta_max >= mass)) |>
      tidytable::filter(!is.na(loss)) |>
      tidytable::distinct(feature_id, adduct, loss, mass, feature_id_dest, adduct_dest)

    log_debug("joining within given delta mz tolerance (adducts) \n")
    df_add <- df_couples_diff |>
      dplyr::inner_join(differences, by = dplyr::join_by(delta_min <= Distance, delta_max >= Distance)) |>
      tidytable::filter(!is.na(Group1)) |>
      tidytable::mutate(tidytable::across(.cols = c("rt"), .fns = as.character)) |>
      tidytable::mutate(
        adduct = ifelse(
          test = is.na(adduct),
          yes = as.character(Group1),
          no = adduct
        ),
        adduct_dest = ifelse(
          test = is.na(adduct_dest),
          yes = as.character(Group2),
          no = adduct_dest
        )
      ) |>
      tidytable::distinct(feature_id, adduct, adduct_dest, !!as.name(paste("feature_id", "dest", sep = "_")))
    rm(df_couples_diff, differences)

    df_nl_min <- df_nl |>
      tidytable::distinct(feature_id, loss, mass)

    log_debug("keeping initial and destination feature \n")
    df_add_a <- df_add |>
      tidytable::distinct(feature_id, adduct)

    df_add_b <- df_add |>
      tidytable::distinct(!!as.name(paste("feature_id", "dest", sep = "_")), adduct_dest) |>
      tidytable::select(feature_id := !!as.name(paste("feature_id", "dest", sep = "_")), adduct = adduct_dest)

    ## Always considering [M+H]+ and [M-H]- ions by default
    df_add_enforced <- df_fea_min |>
      tidytable::distinct(feature_id) |>
      tidytable::mutate(adduct = switch(ms_mode,
        "pos" = "[M+H]+",
        "neg" = "[M-H]-"
      ))

    df_add_full <- tidytable::bind_rows(df_add_a, df_add_b, df_add_enforced) |>
      tidytable::distinct()
    rm(df_add_a, df_add_b, df_add_enforced)

    log_debug("joining with initial results (adducts) \n")
    df_adducted <- df_fea_min |>
      tidytable::distinct(feature_id, rt, mz) |>
      tidytable::left_join(df_add_full)
    rm(df_add_full)

    log_debug("joining with initial results (neutral losses) \n")
    df_addlossed <- df_adducted |>
      tidytable::left_join(df_nl_min) |>
      tidytable::bind_rows(df_adducted) |>
      tidytable::distinct() |>
      tidytable::mutate(adduct = ifelse(
        test = !is.na(loss),
        yes = paste0(
          adduct |> gsub(
            pattern = "M(?![a-z]).*",
            replacement = "M",
            perl = TRUE
          ),
          "-",
          loss,
          adduct |> gsub(
            pattern = ".*M(?![a-z])",
            replacement = "",
            perl = TRUE
          )
        ),
        no = adduct
      ))
    rm(df_adducted, df_nl_min)

    df_addlossed_min <- df_addlossed |>
      tidytable::mutate_rowwise(mass = calculate_mass_of_m(adduct_string = adduct, mz = mz))

    ## Safety
    df_addlossed_min_1 <- df_addlossed_min |>
      tidytable::filter(mass != mz) |>
      tidytable::filter(mass != 0)

    df_addlossed_min_2 <- df_addlossed_min |>
      tidytable::filter(mass == mz | mass == 0)

    if (nrow(df_addlossed_min_2) != 0) {
      message("Some adducts were unproperly detected, defaulting to (de)protonated")
      df_addlossed_min_2 <- df_addlossed_min_2 |>
        tidytable::mutate(adduct = switch(ms_mode,
          "pos" = "[M+H]+",
          "neg" = "[M-H]-"
        )) |>
        tidytable::mutate_rowwise(mass = calculate_mass_of_m(adduct_string = adduct, mz = mz))
    }

    df_addlossed_rdy <- df_addlossed_min_1 |>
      tidytable::bind_rows(df_addlossed_min_2) |>
      tidytable::select(-loss) |>
      tidytable::distinct()
    rm(
      df_addlossed,
      df_addlossed_min,
      df_addlossed_min_1,
      df_addlossed_min_2
    )

    log_debug("joining within given mz tol to exact mass library \n")
    df_addlossed_em <- df_addlossed_rdy |>
      dplyr::inner_join(df_add_em, by = dplyr::join_by(mass >= value_min, mass <= value_max)) |>
      tidytable::mutate(error_mz = exact_mass - mass) |>
      tidytable::mutate(library = adduct) |>
      tidytable::select(feature_id, rt, mz, library, error_mz, exact_mass) |>
      tidytable::distinct()

    log_debug("keeping unique exact masses and molecular formulas \n")
    df_em_mf <- structure_organism_pairs_table |>
      tidytable::distinct(structure_exact_mass, structure_molecular_formula)
    df_str_unique <- structure_organism_pairs_table |>
      tidytable::distinct(
        structure_name,
        structure_inchikey_no_stereo,
        structure_smiles_no_stereo,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp
      ) |>
      ## Avoid SMILES redundancy
      tidytable::distinct(
        structure_inchikey_no_stereo,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.numeric), .fns = as.character))

    ## TODO This should be externalized somehow
    forbidden_adducts <- c(
      "[M-H2O (water)+H2O-H]-",
      "[M-H3O4P (phosphoric)+H3O4P-H]-",
      "[M-H3N (ammonia)+C2H7N-H]-",
      "[M-H3N (ammonia)+C2H3N-H]-",
      "[M-H3N (ammonia)+H4N]+",
      "[M-H2O (water)+H2O+H]+",
      "[M-H3O4P (phosphoric)+H3O4P+H]+",
      "[M-H3N (ammonia)+C2H7N+H]+",
      "[M-H3N (ammonia)+C2H3N+H]+"
    )

    log_debug("joining exact masses with single charge adducts \n")
    log_debug("Geting back to M \n")
    df_annotated_1 <- tidytable::left_join(
      x = df_addlossed_em,
      y = df_em_mf,
      by = stats::setNames("structure_exact_mass", "exact_mass")
    ) |>
      tidytable::filter(!(library %in% forbidden_adducts)) |>
      tidytable::distinct()
    rm(df_addlossed_em)

    log_debug("calculating multicharged and in source dimers \n")
    adducts_table_multi <- adducts_table |>
      tidytable::filter(!adduct %in% add_clu_table$adduct) |>
      tidytable::mutate(join = "x")
    rm(adducts_table, add_clu_table)

    if (nrow(adducts_table_multi) != 0) {
      df_multi <- df_fea_min |>
        tidytable::select(-adduct) |>
        tidytable::mutate(join = "x") |>
        tidytable::left_join(adducts_table_multi) |>
        tidytable::mutate_rowwise(value = calculate_mass_of_m(adduct_string = adduct, mz = mz)) |>
        tidytable::mutate(
          mass_min = value - (1E-6 * tolerance_ppm * value),
          mass_max = value + (1E-6 * tolerance_ppm * value),
          rt_min = rt - tolerance_rt,
          rt_max = rt + tolerance_rt
        ) |>
        tidytable::distinct()
    } else {
      df_multi <- tidytable::tidytable(
        "feature_id" = NA_real_,
        "adduct" = NA_character_,
        "rt" = NA_real_,
        "rt_min" = NA_real_,
        "rt_max" = NA_real_,
        "mass_min" = NA_real_,
        "mass_max" = NA_real_,
        "mz" = NA_real_
      )
    }


    rm(adducts_table_multi)

    log_debug("joining within given rt tolerance \n")
    df_multi_nl <- df_multi |>
      dplyr::inner_join(df_addlossed_rdy,
        by = dplyr::join_by(rt_min <= rt, rt_max >= rt, mass_min <= mass, mass_max >= mass)
      )
    rm(df_fea_min, df_multi, neutral_losses, df_addlossed_rdy)

    log_debug("joining within given mz tol and filtering possible adducts \n")
    df_multi_nl_em <- df_multi_nl |>
      dplyr::inner_join(df_add_em, by = dplyr::join_by(mass >= value_min, mass <= value_max)) |>
      tidytable::as_tidytable() |>
      tidytable::mutate(error_mz = exact_mass - mass) |>
      tidytable::rename(
        feature_id = feature_id.x,
        rt = rt.x,
        mz = mz.x,
        library_name = adduct.x
      ) |>
      tidytable::distinct(feature_id, rt, mz, exact_mass, library_name, error_mz)
    rm(df_add_em, df_multi_nl)

    df_annotated_2 <-
      tidytable::left_join(df_multi_nl_em,
        df_em_mf,
        by = stats::setNames("structure_exact_mass", "exact_mass")
      ) |>
      tidytable::select(structure_molecular_formula,
        library = library_name,
        tidyselect::everything(),
        -exact_mass,
      ) |>
      tidytable::filter(!(library %in% forbidden_adducts)) |>
      tidytable::mutate(library = as.character(library)) |>
      tidytable::distinct()
    rm(df_em_mf, df_multi_nl_em)

    log_debug("joining single adducts, in source dimers, and multicharged \n")
    df_annotated_final <- tidytable::bind_rows(df_annotated_1, df_annotated_2) |>
      tidytable::left_join(df_str_unique) |>
      # TODO decide if allowing this or not
      tidytable::filter(!is.na(structure_inchikey_no_stereo)) |>
      tidytable::select(
        feature_id,
        candidate_structure_error_mz = error_mz,
        candidate_structure_name = structure_name,
        candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
        candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
        candidate_structure_molecular_formula = structure_molecular_formula,
        candidate_structure_exact_mass = structure_exact_mass,
        candidate_structure_xlogp = structure_xlogp,
        candidate_library = library
      ) |>
      tidytable::distinct()

    rm(df_annotated_1, df_annotated_2, df_str_unique)

    log_debug("adding chemical classification")
    df_final <- tidytable::left_join(
      df_annotated_final,
      structure_organism_pairs_table |>
        tidytable::distinct(
          candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
          candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
          candidate_structure_tax_npc_01pat = structure_tax_npc_01pat,
          candidate_structure_tax_npc_02sup = structure_tax_npc_02sup,
          candidate_structure_tax_npc_03cla = structure_tax_npc_03cla,
          ## TODO until better
          candidate_structure_tax_cla_chemontid = structure_tax_cla_chemontid,
          candidate_structure_tax_cla_01kin = structure_tax_cla_01kin,
          candidate_structure_tax_cla_02sup = structure_tax_cla_02sup,
          candidate_structure_tax_cla_03cla = structure_tax_cla_03cla,
          candidate_structure_tax_cla_04dirpar = structure_tax_cla_04dirpar
        )
    ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      )) |>
      tidytable::mutate(candidate_adduct = candidate_library) |>
      tidytable::mutate(candidate_library = "TIMA MS1")
    rm(structure_organism_pairs_table, df_annotated_final)

    df_final |>
      decorate_masses()

    edges <- tidytable::bind_rows(
      df_add |>
        tidytable::mutate(label = paste0(adduct, " _ ", adduct_dest)) |>
        tidytable::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        tidytable::distinct(),
      df_nl |>
        tidytable::mutate(label = paste0(loss, " loss")) |>
        tidytable::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        tidytable::distinct()
    )
    rm(df_nl, df_add)

    export_params(
      parameters = get_params(step = "annotate_masses"),
      step = "annotate_masses"
    )
    export_output(x = edges, file = output_edges[[1]])
    export_output(x = df_final, file = output_annotations[[1]])

    rm(edges, df_final)
    return(c("annotations" = output_annotations[[1]], "edges" = output_edges[[1]]))
  }
