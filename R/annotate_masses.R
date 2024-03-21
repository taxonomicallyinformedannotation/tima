#' @title Annotate masses
#'
#' @description This function annotates masses
#'
#' @include decorate_masses.R
#' @include dist_groups.R
#' @include filter_nitrogen_rule.R
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
#' @param adducts_neg Negative adducts to be used
#' @param adducts_pos Positive adducts to be used
#' @param adducts_masses_list Adducts masses
#' @param clusters_list List of clusters to be used
#' @param clusters_neg Negative clusters to be used
#' @param clusters_pos Positive clusters to be used
#' @param neutral_losses_list List of neutral losses to be used
#' @param name Name of the adducts library
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param tolerance_ppm Tolerance to perform annotation. Should be ≤ 10 ppm
#' @param tolerance_rt Tolerance to group adducts. Should be ≤ 0.1min
#'
#' @return A table containing MS1 annotations based on exact mass
#'
#' @export
#'
#' @examples NULL
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
           name = get_params(step = "annotate_masses")$files$libraries$adducts$prepared,
           adducts_list = get_params(step = "annotate_masses")$ms$adducts,
           adducts_neg = get_params(step = "annotate_masses")$files$libraries$adducts$neg,
           adducts_pos = get_params(step = "annotate_masses")$files$libraries$adducts$pos,
           adducts_masses_list = system.file("extdata",
             "adducts.tsv",
             package = "timaR"
           ),
           clusters_neg = get_params(step = "annotate_masses")$ms$clusters$neg,
           clusters_pos = get_params(step = "annotate_masses")$ms$clusters$pos,
           clusters_list = system.file("extdata",
             "clusters.tsv",
             package = "timaR"
           ),
           neutral_losses_list = system.file("extdata",
             "neutral_losses.tsv",
             package = "timaR"
           ),
           ms_mode = get_params(step = "annotate_masses")$ms$polarity,
           tolerance_ppm = get_params(step = "annotate_masses")$ms$tolerances$mass$ppm$ms1,
           tolerance_rt = get_params(step = "annotate_masses")$ms$tolerances$rt$minutes) {
    stopifnot("Your ppm tolerance must be ≤ 20" = tolerance_ppm <= 20)
    stopifnot("Your rt tolerance must be ≤ 0.05" = tolerance_rt <= 0.05)

    features_table <- tidytable::fread(
      file = features,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    neutral_losses <-
      tidytable::fread(
        file = neutral_losses_list,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("mass"),
          .fns = as.numeric
        )
      )

    if (ms_mode == "pos") {
      adduct_file <- adducts_pos
      adduct_db_file <-
        file.path(
          parse_yaml_paths()$data$interim$libraries$adducts$path,
          paste0(name, "_pos.tsv.gz")
        )
      clusters_allowed <- clusters_pos
    }
    if (ms_mode == "neg") {
      adduct_file <- adducts_neg
      adduct_db_file <-
        file.path(
          parse_yaml_paths()$data$interim$libraries$adducts$path,
          paste0(name, "_neg.tsv.gz")
        )
      clusters_allowed <- clusters_neg
    }

    clusters <-
      tidytable::fread(
        file = clusters_list,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::mutate(cluster = stringi::stri_replace_all_regex(
        str = cluster,
        pattern = "\\(.*",
        replacement = ""
      )) |>
      tidytable::mutate(cluster = trimws(cluster)) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("mass"),
          .fns = as.numeric
        )
      ) |>
      tidytable::filter(cluster %in% clusters_allowed)

    log_debug("... single charge adducts table")
    adducts_table <- tidytable::fread(
      file = adduct_file,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("adduct_mass"),
          .fns = as.numeric
        )
      )

    log_debug("... adducts masses for in source dimers and multicharged")
    adducts_mass_table <-
      tidytable::fread(
        file = adducts_masses_list,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("mass"),
          .fns = as.numeric
        )
      )

    log_debug("... neutral lossses")
    add_m <- adducts_mass_table$mass
    names(add_m) <- adducts_mass_table$adduct
    rm(adducts_mass_table)
    clu_m <- clusters$mass
    names(clu_m) <- clusters$cluster

    ## TODO remove this dirty fix
    adduct_db_file <- adduct_db_file |>
      gsub(
        pattern = "NA_pos.tsv.gz",
        replacement = "library_pos.tsv.gz",
        fixed = TRUE
      ) |>
      gsub(
        pattern = "NA_neg.tsv.gz",
        replacement = "library_neg.tsv.gz",
        fixed = TRUE
      )

    log_debug(x = "... exact masses for MS1 annotation")
    structure_exact_mass_table <-
      tidytable::fread(
        file = adduct_db_file,
        na.strings = c("", "NA"),
        colClasses = "character"
      )

    adducts <- unlist(adducts_list[[ms_mode]])

    ## slim it
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
      tidytable::mutate(
        tidytable::across(
          .cols = c("structure_exact_mass"),
          .fns = as.numeric
        )
      ) |>
      round_reals()

    log_debug("filtering desired adducts and adding mz tolerance \n")
    df_add_em <- structure_exact_mass_table |>
      tidytable::filter(!is.na(exact_mass)) |>
      tidytable::filter(!stringi::stri_detect_fixed(
        str = adduct,
        pattern = adducts
      )) |>
      tidytable::mutate(tidytable::across(
        .cols = c("adduct_mass", "exact_mass"),
        .fns = as.numeric
      )) |>
      tidytable::mutate(
        value_min = adduct_mass - (1E-6 * tolerance_ppm * adduct_mass),
        value_max = adduct_mass + (1E-6 * tolerance_ppm * adduct_mass)
      ) |>
      tidytable::filter(!is.na(value_min)) |>
      tidytable::filter(value_min > 0)
    rm(structure_exact_mass_table)

    df_fea_min <- features_table |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("mz"),
          .fns = as.numeric
        )
      ) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)

    if (any(names(features_table) == "rt")) {
      df_fea_min <- df_fea_min |>
        tidytable::mutate(
          tidytable::across(
            .cols = c("rt"),
            .fns = as.numeric
          )
        )
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
      dplyr::inner_join(df_fea_min,
        by = dplyr::join_by(
          rt_min <= rt,
          rt_max >= rt
        )
      ) |>
      tidytable::distinct(
        feature_id = feature_id.x,
        rt = rt.x,
        mz = mz.x,
        feature_id_dest = feature_id.y,
        mz_dest = mz.y
      ) |>
      tidytable::select(
        tidytable::everything(),
        feature_id_dest,
        mz_dest
      ) |>
      tidytable::filter(feature_id != feature_id_dest) |>
      log_pipe("adding delta mz tolerance for single charge adducts \n") |>
      tidytable::filter(mz >= mz_dest) |>
      tidytable::mutate(
        delta_min = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz - (1E-6 * tolerance_ppm * mz) - mz_dest),
          no = abs(mz + (1E-6 * tolerance_ppm * mz) - mz_dest)
        ),
        delta_max = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz + (1E-6 * tolerance_ppm * mz) - mz_dest),
          no = abs(mz - (1E-6 * tolerance_ppm * mz) - mz_dest)
        )
      )
    rm(df_rt_tol)

    log_debug("forming clusters \n")
    add_clu_table <- adducts_table |>
      tidytable::mutate(join = "x") |>
      tidytable::left_join(
        clusters |>
          tidytable::mutate(join = "x")
      ) |>
      tidytable::bind_rows(adducts_table) |>
      tidytable::mutate(
        adduct = ifelse(
          test = !is.na(cluster),
          yes = paste(adduct, "+", paste0("(", cluster, ")1")),
          no = adduct
        ),
        adduct_mass = ifelse(
          test = !is.na(cluster),
          yes = adduct_mass + mass,
          no = adduct_mass
        )
      )
    rm(adducts_table, clusters)

    log_debug("calculating delta mz for single charge adducts and clusters \n")
    differences <-
      dist_groups(
        d = stats::dist(add_clu_table$adduct_mass),
        g = add_clu_table$adduct
      ) |>
      tidytable::select(-Item1, -Item2, -Label) |>
      ## remove redundancy among clusters
      ## Keep proton first
      tidytable::mutate(l = stringi::stri_length(Group1)) |>
      tidytable::arrange(l) |>
      tidytable::select(-l) |>
      tidytable::distinct(Distance, .keep_all = TRUE)

    log_debug("joining within given delta mz tolerance (neutral losses) \n")
    df_nl <- df_couples_diff |>
      dplyr::inner_join(neutral_losses,
        by = dplyr::join_by(
          delta_min <= mass,
          delta_max >= mass
        )
      ) |>
      tidytable::filter(!is.na(loss)) |>
      tidytable::distinct(
        feature_id,
        loss,
        mass,
        feature_id_dest
      )

    log_debug("joining within given delta mz tolerance (adducts) \n")
    df_add <- df_couples_diff |>
      dplyr::inner_join(differences,
        by = dplyr::join_by(
          delta_min <= Distance,
          delta_max >= Distance
        )
      ) |>
      tidytable::filter(!is.na(Group1)) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("rt"),
          .fns = as.character
        )
      ) |>
      tidytable::mutate(
        label = as.character(Group1),
        label_dest = as.character(Group2)
      ) |>
      tidytable::distinct(
        feature_id,
        label,
        label_dest, !!as.name(paste("feature_id", "dest", sep = "_"))
      )
    rm(df_couples_diff, differences)

    df_nl_min <- df_nl |>
      tidytable::distinct(feature_id, loss, mass)

    log_debug("keeping initial and destination feature \n")
    df_add_a <- df_add |>
      tidytable::distinct(feature_id, label)

    df_add_b <- df_add |>
      tidytable::distinct(
        !!as.name(paste("feature_id", "dest", sep = "_")),
        label_dest
      ) |>
      tidytable::select(
        feature_id := !!as.name(paste("feature_id",
          "dest",
          sep = "_"
        )),
        label = label_dest
      )

    ## Always considering [1M+H]+ and [1M-H]- ions by default
    df_add_enforced <- df_fea_min |>
      tidytable::distinct(feature_id) |>
      tidytable::mutate(label = switch(ms_mode,
        "pos" = "[1M+(H)1]1+",
        "neg" = "[1M-(H)1]1-"
      ))

    df_add_full <- tidytable::bind_rows(
      df_add_a,
      df_add_b,
      df_add_enforced
    ) |>
      tidytable::distinct()
    rm(df_add_a, df_add_b, df_add_enforced)

    log_debug("joining with initial results (adducts) \n")
    df_adducted <- df_fea_min |>
      tidytable::distinct(
        feature_id,
        rt,
        mz
      ) |>
      tidytable::left_join(
        df_add_full
      )
    rm(df_add_full)

    log_debug("joining with initial results (neutral losses) \n")
    df_addlossed <- df_adducted |>
      tidytable::left_join(df_nl_min) |>
      tidytable::mutate(mz_1 = ifelse(
        test = !is.na(loss),
        yes = mz + mass,
        no = mz
      )) |>
      tidytable::filter(!stringi::stri_endswith_fixed(
        str = label, pattern =
          loss
      ) | is.na(loss))
    rm(df_adducted, df_nl_min)

    log_debug("joining within given mz tol to exact mass library \n")
    df_addlossed_em <- df_addlossed |>
      dplyr::inner_join(df_add_em,
        by = dplyr::join_by(
          mz_1 >= value_min,
          mz_1 <= value_max
        )
      ) |>
      ## Otherwise matching non-detected adducts
      ## TODO Think how to integrate them the same, as some can be interesting
      ## Eventually a parameter
      tidytable::filter(adduct == label) |>
      tidytable::mutate(error_mz = adduct_mass - mz_1) |>
      tidytable::mutate(library = ifelse(
        test = !is.na(loss),
        yes = paste0(adduct, " - ", loss),
        no = adduct
      )) |>
      tidytable::select(
        feature_id,
        rt,
        mz,
        library,
        error_mz,
        exact_mass,
        adduct,
        adduct_mass,
        loss
      ) |>
      tidytable::distinct()
    rm(df_addlossed)

    log_debug("keeping unique exact masses and molecular formulas \n")
    df_em_mf <- structure_organism_pairs_table |>
      tidytable::distinct(
        structure_exact_mass,
        structure_molecular_formula
      )
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
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.numeric),
          .fns = as.character
        )
      )

    ## TODO This will then be externalized somehow
    forbidden_adducts <- c(
      "[1M-(H)1]1- + (H2O)1 - H2O (water)",
      "[1M-(H)1]1- + (H2PO4)1 - H3O4P (phosphoric)",
      "[1M-(H)1]1- + (C2H7N)1 - H3N (ammonia)",
      "[1M-(H)1]1- + (C2H3N)1 - H3N (ammonia)",
      "[1M+(H)4(N)1]1+ - H3N (ammonia)",
      "[1M+(H)1]1+ + (H2O)1 - H2O (water)",
      "[1M+(H)1]1+ + (H2PO4)1 - H3O4P (phosphoric)",
      "[1M+(H)1]1+ + (C2H7N)1 - H3N (ammonia)",
      "[1M+(H)1]1+ + (C2H3N)1 - H3N (ammonia)",
      "[2M+(H)4(N)1]1+ - H3N (ammonia)"
    )

    log_debug("joining exact masses with single charge adducts \n")
    log_debug("Geting back to [1M] \n")
    df_annotated_1 <- tidytable::left_join(
      x = df_addlossed_em,
      y = df_em_mf,
      by = stats::setNames("structure_exact_mass", "exact_mass")
    ) |>
      tidytable::filter(!(library %in% forbidden_adducts)) |>
      tidytable::distinct()
    rm(df_addlossed_em)

    log_debug("calculating multicharged and in source dimers + mz tol \n")
    if (ms_mode == "pos") {
      df_multi <- df_annotated_1 |>
        tidytable::select(
          feature_id,
          rt,
          mz,
          exact_mass
        ) |>
        tidytable::distinct() |>
        tidytable::mutate(
          `[1M+(H)3]3+` = (exact_mass + 3 * add_m["H+ (proton)"]) / 3,
          `[1M+(H)2(Na)1]3+` = (exact_mass + 2 * add_m["H+ (proton)"] + add_m["Na+ (sodium)"]) / 3,
          `[1M+(H)1(Na)2]3+` = (exact_mass + add_m["H+ (proton)"] + 2 * add_m["Na+ (sodium)"]) / 3,
          `[1M+(Na)3]3+` = (exact_mass + 3 * add_m["Na+ (sodium)"]) / 3,
          `[1M+(H)2]2+` = (exact_mass + 2 * add_m["H+ (proton)"]) / 2,
          `[1M+(H)2]2+ + (HN3)1` = (exact_mass + 2 * add_m["H+ (proton)"] + clu_m["HN3"]) / 2,
          `[1M+(H)1(Na)1]2+` = (exact_mass + add_m["H+ (proton)"] + add_m["Na+ (sodium)"]) / 2,
          `[1M+(Mg)1]2+` = (exact_mass + add_m["Mg++ (magnesium)"]) / 2,
          `[1M+(H)1(K)1]2+` = (exact_mass + add_m["H+ (proton)"] + add_m["K+ (potassium)"]) / 2,
          `[1M+(Ca)1]2+` = (exact_mass + add_m["Ca++ (calcium)"]) / 2,
          `[1M+(H)2]2+ + (C2H3N)1` = (exact_mass + 2 * add_m["H+ (proton)"] + clu_m["C2H3N"]) / 2,
          `[1M+(Na)2]2+` = (exact_mass + 2 * add_m["Na+ (sodium)"]) / 2,
          `[1M+(Fe)1]2+` = (exact_mass + add_m["Fe++ (iron)"]) / 2,
          `[1M+(H)2]2+ + (C2H3N)2` = (exact_mass + 2 * add_m["H+ (proton)"] + 2 * clu_m["C2H3N"]) / 2,
          `[1M+(H)]2+ + (C2H3N)3` = (exact_mass + 2 * add_m["H+ (proton)"] + 3 * clu_m["C2H3N"]) / 2,
          `[2M+(Mg)1]2+` = (2 * (exact_mass) + add_m["Mg++ (magnesium)"]) / 2,
          `[2M+(Ca)1]2+` = (2 * (exact_mass) + add_m["Ca++ (calcium)"]) / 2,
          `[2M+(Fe)1]2+` = (2 * (exact_mass) + add_m["Fe++ (iron)"]) / 2,
          `[2M+(H)1]1+` = 2 * (exact_mass) + add_m["H+ (proton)"],
          `[2M+(H)1]1+ + (HN3)1` = 2 * (exact_mass) + add_m["H+ (proton)"] + clu_m["HN3"],
          `[2M+(Na)1]1+` = 2 * (exact_mass) + add_m["Na+ (sodium)"],
          `[2M+(K)1]1+` = 2 * (mz - add_m["H+ (proton)"]) + add_m["K+ (potassium)"],
          `[2M+(H)1]1+ + (C2H3N)1` = 2 * (exact_mass) + add_m["H+ (proton)"] + clu_m["C2H3N"],
          `[2M+(Na)1]1+ + (C2H3N)1` = 2 * (exact_mass) + add_m["Na+ (sodium)"] + clu_m["C2H3N"]
        ) |>
        tidytable::select(-exact_mass) |>
        tidytable::ungroup()

      cols <- ncol(df_multi)

      df_multi <- df_multi |>
        tidytable::pivot_longer(cols = tidytable::all_of(4:cols))
    }
    if (ms_mode == "neg") {
      df_multi <- df_annotated_1 |>
        tidytable::select(
          feature_id,
          rt,
          mz,
          exact_mass
        ) |>
        tidytable::mutate(
          `[1M-(H)3]3-` = (exact_mass - 3 * add_m["H+ (proton)"]) / 3,
          `[1M-(H)2]2-` = (exact_mass - 2 * add_m["H+ (proton)"]) / 2,
          `[2M-(H)1]1-` = 2 * (exact_mass) - add_m["H+ (proton)"],
          `[2M-(H)1]1- + (CH2O2)1` = 2 * (exact_mass) + clu_m["CH2O2"] - add_m["H+ (proton)"],
          `[2M-(H)1]1- + (C2H4O2)1` = 2 * (exact_mass) + clu_m["C2H4O2"] - add_m["H+ (proton)"],
          `[3M-(H)1]1-` = 3 * (exact_mass) - add_m["H+ (proton)"]
        ) |>
        tidytable::select(-exact_mass) |>
        tidytable::ungroup()

      cols <- ncol(df_multi)

      df_multi <- df_multi |>
        tidytable::pivot_longer(cols = tidytable::all_of(4:cols))
    }

    df_multi <- df_multi |>
      tidytable::mutate(
        mz_min = value - (1E-6 * tolerance_ppm * value),
        mz_max = value + (1E-6 * tolerance_ppm * value),
        rt_min = rt - tolerance_rt,
        rt_max = rt + tolerance_rt
      ) |>
      tidytable::distinct()
    df_multi_min <- df_multi |>
      tidytable::distinct(feature_id, rt_min, rt_max)

    log_debug("joining within given rt tolerance \n")
    df_multi_rt <- df_multi_min |>
      dplyr::inner_join(df_fea_min,
        by = dplyr::join_by(
          rt_min <= rt,
          rt_max >= rt
        )
      ) |>
      tidytable::inner_join(df_multi) |>
      tidytable::mutate(
        delta_min = mz - mz_max,
        delta_max = mz - mz_min
      ) |>
      tidytable::filter(delta_max > min(neutral_losses$mass) | (mz >= mz_min & mz <= mz_max))
    rm(df_fea_min, df_multi, df_multi_min)

    df_multi_nl <- df_multi_rt |>
      dplyr::inner_join(neutral_losses,
        by = dplyr::join_by(
          delta_min <= mass,
          delta_max >= mass
        )
      ) |>
      tidytable::filter(!is.na(loss) | (mz >= mz_min & mz <= mz_max)) |>
      tidytable::mutate(name = ifelse(test = !is.na(loss),
        yes = paste(name, "-", loss, sep = " "),
        no = name
      )) |>
      tidytable::select(
        feature_id,
        rt,
        mz,
        library_name = name,
        adduct_value = value,
        mz_min,
        mz_max
      )
    rm(df_multi_rt, neutral_losses)

    log_debug("joining within given mz tol and filtering possible adducts \n")
    df_multi_nl_em <- df_multi_nl |>
      dplyr::inner_join(df_add_em,
        by = dplyr::join_by(
          adduct_value >= value_min,
          adduct_value <= value_max
        )
      ) |>
      tidytable::as_tidytable() |>
      tidytable::filter(stringi::stri_detect_fixed(
        pattern = paste(adduct, "", sep = " "),
        str = library_name
      )) |>
      tidytable::mutate(error_mz = adduct_mass - adduct_value) |>
      tidytable::distinct(
        feature_id,
        rt,
        mz,
        exact_mass,
        library_name,
        error_mz,
        adduct_value
      )
    rm(df_add_em, df_multi_nl)

    df_annotated_2 <-
      tidytable::left_join(df_multi_nl_em,
        df_em_mf,
        by = stats::setNames("structure_exact_mass", "exact_mass")
      ) |>
      tidytable::select(
        structure_molecular_formula,
        library = library_name,
        tidytable::everything(),
        -exact_mass,
        -adduct_value
      ) |>
      tidytable::filter(!(library %in% forbidden_adducts)) |>
      tidytable::mutate(library = as.character(library)) |>
      tidytable::distinct()
    rm(df_em_mf, df_multi_nl_em)

    log_debug("joining single adducts, neutral losses, and multicharged \n")
    df_annotated_final <- tidytable::bind_rows(
      df_annotated_1,
      df_annotated_2
    ) |>
      tidytable::left_join(df_str_unique) |>
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

    df_annotated_filtered <- df_annotated_final |>
      filter_nitrogen_rule(features_table = features_table)

    rm(df_annotated_final)

    log_debug("adding chemical classification")
    df_final <- tidytable::left_join(
      df_annotated_filtered,
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
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = function(x) {
            tidytable::na_if(x, "")
          }
        )
      )
    rm(structure_organism_pairs_table)

    df_final |>
      decorate_masses()

    edges <- tidytable::bind_rows(
      df_add |>
        tidytable::mutate(label = paste0(label, " _ ", label_dest)) |>
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
    rm(df_nl)

    export_params(parameters = get_params(step = "annotate_masses"), step = "annotate_masses")
    export_output(x = edges, file = output_edges[[1]])
    export_output(x = df_final, file = output_annotations[[1]])

    rm(edges, df_final)
    return(c(
      "annotations" = output_annotations[[1]],
      "edges" = output_edges[[1]]
    ))
  }
