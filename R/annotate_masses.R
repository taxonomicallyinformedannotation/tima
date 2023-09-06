#' @title Annotate masses
#'
#' @description This function annotates masses
#'
#' @include decorate_masses.R
#' @include dist_groups.R
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
           output_annotations = get_params(step = "annotate_masses")$files$annotations$prepared$structural,
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
    stopifnot("Your ppm tolerance must be lower or equal to 20" = tolerance_ppm <=
      20)
    stopifnot("Your rt tolerance must be lower or equal to 0.05" = tolerance_rt <=
      0.05)

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
    } else {
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
    df2 <- structure_exact_mass_table |>
      tidytable::filter(!is.na(exact_mass)) |>
      tidytable::filter(adduct %in% adducts) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("adduct_mass", "exact_mass"),
          .fns = as.numeric
        )
      ) |>
      tidytable::mutate(
        value_min = adduct_mass - (1E-6 * tolerance_ppm * adduct_mass),
        value_max = adduct_mass + (1E-6 * tolerance_ppm * adduct_mass)
      ) |>
      tidytable::filter(!is.na(value_min)) |>
      tidytable::filter(value_min > 0)

    df3 <- features_table |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("mz"),
          .fns = as.numeric
        )
      ) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)

    if (any(names(features_table) == "rt")) {
      df3 <- df3 |>
        tidytable::mutate(
          tidytable::across(
            .cols = c("rt"),
            .fns = as.numeric
          )
        )
    } else {
      df3[, "rt"] <- df3[, "feature_id"]
    }

    log_debug("adding rt tolerance ... \n")
    df4 <- df3 |>
      tidytable::mutate(
        rt_min = as.numeric(rt - tolerance_rt),
        rt_max = as.numeric(rt + tolerance_rt)
      )

    log_debug("joining within given rt tolerance \n")
    df5 <- df4 |>
      dplyr::inner_join(df3,
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
          yes = abs(mz -
            (1E-6 *
              tolerance_ppm *
              mz) -
            mz_dest),
          no = abs(mz + (1E-6 *
            tolerance_ppm *
            mz) - mz_dest)
        ),
        delta_max = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz + (1E-6 *
            tolerance_ppm *
            mz) - mz_dest),
          no = abs(mz -
            (1E-6 *
              tolerance_ppm *
              mz) -
            mz_dest)
        )
      )

    log_debug("forming clusters \n")
    df6 <- adducts_table |>
      tidytable::bind_rows(
        clusters |>
          tidytable::rename(
            adduct = cluster,
            adduct_mass = mass
          ) |>
          tidytable::mutate(
            adduct = paste("[1M+(H)1]1+", "+", adduct),
            adduct_mass = 1.00728 + adduct_mass
          )
      )

    df7 <- df2 |>
      tidytable::filter(adduct == "[1M+(H)1]1+") |>
      tidytable::mutate(join = "x") |>
      tidytable::left_join(clusters |>
        tidytable::mutate(join = "x")) |>
      tidytable::mutate(
        adduct = paste("[1M+(H)1]1+", "+", cluster),
        adduct_mass = adduct_mass + mass,
        value_min = value_min + mass,
        value_max = value_max + mass
      ) |>
      tidytable::select(colnames(df2)) |>
      tidytable::bind_rows(df2)

    log_debug("calculating delta mz for single charge adducts \n")
    df8 <-
      dist_groups(
        d = stats::dist(df6$adduct_mass),
        g = df6$adduct
      ) |>
      tidytable::select(-Item1, -Item2, -Label)

    log_debug("joining within given delta mz tolerance (neutral losses) \n")
    df9_d <- df5 |>
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

    df9_e <- df9_d |>
      tidytable::distinct(feature_id, loss, mass)

    log_debug("joining within given delta mz tolerance (adducts) \n")
    df9 <- df5 |>
      dplyr::inner_join(df8,
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

    log_debug("keeping initial and destination feature \n")
    df9_a <- df9 |>
      tidytable::distinct(feature_id, label)

    df9_b <- df9 |>
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
    df9_ion <- df3 |>
      tidytable::distinct(feature_id) |>
      tidytable::mutate(label = switch(ms_mode,
        "pos" = "[1M+(H)1]1+",
        "neg" = "[1M-(H)1]1-"
      ))

    df9_c <- tidytable::bind_rows(
      df9_a,
      df9_b,
      df9_ion
    ) |>
      tidytable::distinct()

    log_debug("joining with initial results (adducts) \n")
    df10 <- df3 |>
      tidytable::distinct(
        feature_id,
        rt,
        mz
      ) |>
      tidytable::left_join(
        df9_c
      )

    log_debug("joining with initial results (neutral losses) \n")
    df10_a <- tidytable::left_join(df10, df9_e) |>
      tidytable::mutate(mz_1 = ifelse(
        test = !is.na(loss),
        yes = mz + mass,
        no = mz
      ))

    log_debug("joining within given mz tol and filtering single charges \n")
    df11 <- df10_a |>
      dplyr::inner_join(df7,
        by = dplyr::join_by(
          mz_1 >= value_min,
          mz_1 <= value_max
        )
      ) |>
      tidytable::mutate(error_mz = adduct_mass - mz_1) |>
      tidytable::select(
        feature_id,
        rt,
        mz,
        error_mz,
        exact_mass,
        adduct,
        adduct_mass,
        loss
      ) |>
      tidytable::mutate(library = ifelse(
        test = !is.na(loss),
        yes = paste0(adduct, " - ", loss),
        no = adduct
      )) |>
      tidytable::distinct() |>
      tidytable::filter(!is.na(library))

    log_debug("cleaning results \n")
    df12 <- df11 |>
      tidytable::select(
        feature_id,
        rt,
        mz,
        library,
        error_mz,
        exact_mass
      ) |>
      tidytable::filter(!is.na(exact_mass))

    log_debug("keeping unique adducts per exact mass \n")
    df13 <- structure_organism_pairs_table |>
      tidytable::distinct(
        structure_exact_mass,
        structure_molecular_formula
      )
    df13_b <- structure_organism_pairs_table |>
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
      "[1M-(H)1]1- + NH3 (ammonia) - H3N (ammonia)",
      "[1M-(H)1]1- + H2O (water) - H2O (water)",
      "[1M-(H)1]1- + H2PO4 (phosphoric) - H3O4P (phosphoric)",
      "[1M-(H)1]1- + C2H7N (ethylamine) - H3N (ammonia)",
      "[1M-(H)1]1- + C2H3N (acetonitrile) - H3N (ammonia)",
      "[1M+(H)1]1+ + NH3 (ammonia) - H3N (ammonia)",
      "[1M+(H)1]1+ + H2O (water) - H2O (water)",
      "[1M+(H)1]1+ + H2PO4 (phosphoric) - H3O4P (phosphoric)",
      "[1M+(H)1]1+ + C2H7N (ethylamine) - H3N (ammonia)",
      "[1M+(H)1]1+ + C2H3N (acetonitrile) - H3N (ammonia)"
    )

    log_debug("joining exact masses with single charge adducts \n")
    df14 <- tidytable::left_join(
      x = df12,
      y = df13,
      by = stats::setNames("structure_exact_mass", "exact_mass")
    ) |>
      tidytable::select(
        structure_molecular_formula,
        library,
        tidytable::everything(),
        -exact_mass
      ) |>
      tidytable::filter(!(library %in% forbidden_adducts)) |>
      tidytable::distinct()

    log_debug("adding adduct mass to get back to [1M] \n")
    df15 <-
      tidytable::left_join(df14,
        adducts_table,
        by = stats::setNames("adduct", "library")
      ) |>
      tidytable::distinct(feature_id, .keep_all = TRUE) |>
      tidytable::select(
        feature_id,
        adduct_mass
      ) |>
      tidytable::filter(!is.na(adduct_mass))

    log_debug("keeping ions for exploration starting from [1M] \n")
    df16 <- tidytable::inner_join(df3, df15)

    log_debug("calculating multicharged and in source dimers + mz tol \n")
    if (ms_mode == "pos") {
      df17 <- df16 |>
        tidytable::select(
          feature_id,
          rt,
          mz,
          adduct_mass
        ) |>
        tidytable::rowwise() |>
        tidytable::mutate(
          `[1M+(H)3]3+` = (mz - adduct_mass + 3 * add_m["H (proton)"]) / 3,
          `[1M+(H)2(Na)1]3+` = (mz - adduct_mass +
            2 * add_m["H (proton)"] +
            add_m["Na (sodium)"]) / 3,
          `[1M+(H)1(Na)2]3+` = (mz - adduct_mass +
            add_m["H (proton)"] +
            2 * add_m["Na (sodium)"]) / 3,
          `[1M+(Na)3]3+` = (mz - adduct_mass + 3 * add_m["Na (sodium)"]) / 3,
          `[1M+(H)2]2+` = (mz - adduct_mass + 2 * add_m["H (proton)"]) / 2,
          `[1M+(H)2(NH3)1]2+` = (mz - adduct_mass +
            2 * add_m["H (proton)"] +
            add_m["NH4 (ammonium)"]) / 2,
          `[1M+(H)1(Na)1]2+` = (mz - adduct_mass +
            add_m["H (proton)"] +
            add_m["Na (sodium)"]) / 2,
          `[1M+(Mg)1]2+` = (mz - adduct_mass +
            add_m["Mg (magnesium)"]) / 2,
          `[1M+(H)1(K)1]2+` = (mz - adduct_mass +
            add_m["H (proton)"] +
            add_m["K (potassium)"]) / 2,
          `[1M+(Ca)1]2+` = (mz - adduct_mass +
            add_m["Ca (calcium)"]) / 2,
          `[1M+(H)2(ACN)1]2+` = (mz - adduct_mass +
            2 * add_m["H (proton)"] +
            add_m["C2H3N (acetonitrile)"]) / 2,
          `[1M+(Na)2]2+` = (mz - adduct_mass +
            2 * add_m["Na (sodium)"]) / 2,
          `[1M+(Fe)1]2+` = (mz - adduct_mass +
            add_m["Fe (iron)"]) / 2,
          `[1M+(H)2(ACN)2]2+` = (mz - adduct_mass +
            2 * add_m["H (proton)"] +
            2 * add_m["C2H3N (acetonitrile)"]) / 2,
          `[1M+(H)2(ACN)3]2+` = (mz - adduct_mass +
            2 * add_m["H (proton)"] +
            3 * add_m["C2H3N (acetonitrile)"]) / 2,
          `[2M+(Mg)1]2+` = (2 * (mz - adduct_mass) +
            add_m["Mg (magnesium)"]) / 2,
          `[2M+(Ca)1]2+` = (2 * (mz - adduct_mass) +
            add_m["Ca (calcium)"]) / 2,
          `[2M+(Fe)1]2+` = (2 * (mz - adduct_mass) +
            add_m["Fe (iron)"]) / 2,
          `[2M+(H)1]1+` = 2 * (mz - adduct_mass) +
            add_m["H (proton)"],
          `[2M+(H)1(NH3)1]1+` = 2 * (mz - adduct_mass) +
            add_m["H (proton)"] +
            add_m["NH4 (ammonium)"],
          `[2M+(Na)1]1+` = 2 * (mz - adduct_mass) +
            add_m["Na (sodium)"],
          `[2M+(K)1]1+` = 2 * (mz - add_m["H (proton)"]) +
            add_m["K (potassium)"],
          `[2M+(H)1(ACN)1]1+` = 2 * (mz - adduct_mass) +
            add_m["H (proton)"] +
            add_m["C2H3N (acetonitrile)"],
          `[2M+(Na)1(ACN)1]1+` = 2 * (mz - adduct_mass) +
            add_m["C2H3N (acetonitrile)"] +
            add_m["Na (sodium)"]
        ) |>
        tidytable::select(-adduct_mass) |>
        tidytable::ungroup()

      cols <- ncol(df17)

      df17 <- df17 |>
        tidytable::pivot_longer(cols = tidytable::all_of(4:cols))
    } else {
      df17 <- df16 |>
        tidytable::select(
          feature_id,
          rt,
          mz,
          adduct_mass
        ) |>
        tidytable::rowwise() |>
        tidytable::mutate(
          `[1M-(H)3]3-` = (mz + adduct_mass -
            3 * add_m["H (proton)"]) / 3,
          `[1M-(H)2]2-` = (mz + adduct_mass -
            2 * add_m["H (proton)"]) / 2,
          `[2M-(H)1]1-` = 2 * (mz + adduct_mass) -
            add_m["H (proton)"],
          `[2M+(FA)1-(H)1]1-` = 2 * (mz + adduct_mass) +
            add_m["CH2O2 (formic)"] - add_m["H (proton)"],
          `[2M+(Hac)1-(H)1]1-` = 2 * (mz + adduct_mass) +
            add_m["C2H4O2 (acetic)"] - add_m["H (proton)"],
          `[3M-(H)1]1-` = 3 * (mz + adduct_mass) -
            add_m["H (proton)"]
        ) |>
        tidytable::select(-adduct_mass) |>
        tidytable::ungroup()

      cols <- ncol(df17)

      df17 <- df17 |>
        tidytable::pivot_longer(cols = tidytable::all_of(4:cols))
    }

    df17 <- df17 |>
      tidytable::mutate(
        mz_min = value - (1E-6 * tolerance_ppm * value),
        mz_max = value + (1E-6 * tolerance_ppm * value),
        rt_min = rt - tolerance_rt,
        rt_max = rt + tolerance_rt
      )

    log_debug("joining within given rt tolerance \n")
    df20 <- df17 |>
      dplyr::inner_join(df3,
        by = dplyr::join_by(
          rt_min <= rt,
          rt_max >= rt
        )
      ) |>
      tidytable::as_tidytable() |>
      tidytable::mutate(
        delta_min = mz.x - mz_max,
        delta_max = mz.x - mz_min
      ) |>
      tidytable::filter(delta_max > min(neutral_losses$mass) |
        (mz.x >= mz_min & mz.x <= mz_max))

    df20_a <- df20 |>
      dplyr::inner_join(neutral_losses,
        by = dplyr::join_by(
          delta_min <= mass,
          delta_max >= mass
        )
      ) |>
      tidytable::filter(!is.na(loss) |
        (mz.x >= mz_min &
          mz.x <= mz_max)) |>
      tidytable::mutate(name = ifelse(
        test = !is.na(loss),
        yes = paste(name, "-", loss, sep = " "),
        no = name
      )) |>
      tidytable::select(
        feature_id = feature_id.x,
        rt = rt.x,
        mz = mz.x,
        library_name = name,
        adduct_value = value,
        mz_min,
        mz_max
      )

    log_debug("joining within given mz tol and filtering possible adducts \n")
    df21 <- df20_a |>
      dplyr::inner_join(df2,
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

    df22 <-
      tidytable::left_join(df21,
        df13,
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

    log_debug("joining single adducts, neutral losses, and multicharged \n")
    df24 <- tidytable::bind_rows(
      df14,
      df22
    ) |>
      tidytable::left_join(df13_b) |>
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

    log_debug("adding chemical classification")
    df25 <- tidytable::left_join(
      df24,
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

    df25 |>
      decorate_masses()

    edges <- tidytable::bind_rows(
      df9 |>
        tidytable::mutate(label = paste0(label, " _ ", label_dest)) |>
        tidytable::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        tidytable::distinct(),
      df9_d |>
        tidytable::mutate(label = paste0(loss, " loss")) |>
        tidytable::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        tidytable::distinct()
    )

    export_params(parameters = get_params(step = "annotate_masses"), step = "annotate_masses")
    export_output(x = edges, file = output_edges[[1]])
    export_output(x = df25, file = output_annotations[[1]])

    rm(edges, df25)
    return(c(
      "annotations" = output_annotations[[1]],
      "edges" = output_edges[[1]]
    ))
  }
