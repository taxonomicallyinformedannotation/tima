#' @title Annotate masses
#'
#' @description This function annotates masses
#'
#' @param features Table containing your previous annotation to complement
#' @param output_annotations Output for mass based structural annotations
#' @param output_edges Output for mass based edges
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param library Library containing the keys
#' @param str_2D_3D File containing 2D and 3D structures
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param adducts_list List of adducts to be used
#' @param adducts_masses_list Adducts masses
#' @param neutral_losses_list List of neutral losses to be used
#' @param name Name of the adducts library
#' @param msMode Ionization mode. Must be 'pos' or 'neg'
#' @param tolerancePpm Tolerance to perform annotation. Should be lower than 10 ppm
#' @param toleranceRt Tolerance to perform adduct attribution. Should be lower than 0.1min
#' @param parameters params
#'
#' @return A table containing MS1 annotations based on exact mass
#'
#' @export
#'
#' @examples NULL
annotate_masses <-
  function(features = params$files$features$prepared,
           output_annotations = params$files$annotations$prepared,
           output_edges = params$files$networks$spectral$edges$raw,
           name_source = params$names$source,
           name_target = params$names$target,
           library = paths$data$interim$libraries$sop$merged$keys,
           str_2D_3D = params$files$libraries$sop$merged$structures$dd_ddd,
           str_met = params$files$libraries$sop$merged$structures$metadata,
           str_nam = params$files$libraries$sop$merged$structures$names,
           str_tax_cla = params$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = params$files$libraries$sop$merged$structures$taxonomies$npc,
           name = params$files$libraries$adducts$prepared,
           adducts_list = params$ms$adducts,
           adducts_masses_list = paths$inst$extdata$adducts,
           neutral_losses_list = paths$inst$extdata$neutral_losses,
           msMode = params$ms$polarity,
           tolerancePpm = params$ms$tolerances$mass$ppm$ms1,
           toleranceRt = params$ms$tolerances$rt$minutes,
           parameters = params) {
    stopifnot("Your ppm tolerance must be lower or equal to 20" = tolerancePpm <=
      20)
    stopifnot("Your rt tolerance must be lower or equal to 0.1" = toleranceRt <=
      0.1)

    paths <<- parse_yaml_paths()
    params <<- parameters

    featuresTable <- readr::read_delim(
      file = features,
      col_types = readr::cols(.default = "c")
    )

    neutralLosses <-
      readr::read_delim(file = neutral_losses_list)

    log_debug("... single charge adducts table")
    if (msMode == "pos") {
      adduct_file <- paths$data$interim$libraries$adducts$pos
    } else {
      adduct_file <- paths$data$interim$libraries$adducts$neg
    }

    adductsTable <- readr::read_delim(file = adduct_file)

    log_debug("... adducts masses for in source dimers and multicharged")
    adductsMassTable <-
      readr::read_delim(file = adducts_masses_list)

    log_debug("... neutral lossses")

    adductsM <- adductsMassTable$mass
    names(adductsM) <- adductsMassTable$adduct

    if (msMode == "pos") {
      adduct_db_file <-
        file.path(
          paths$data$interim$libraries$adducts$path,
          paste0(name, "_pos.tsv.gz")
        )
    } else {
      adduct_db_file <-
        file.path(
          paths$data$interim$libraries$adducts$path,
          paste0(name, "_neg.tsv.gz")
        )
    }

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
    structureExactMassTable <-
      readr::read_delim(file = adduct_db_file)

    adducts <- unlist(adducts_list[[msMode]])
    # |>
    #   dplyr::filter(exact_mass %in% structure_organism_pairs_table[["structure_exact_mass"]])


    ## slim it
    structureOrganismPairsTable <-
      readr::read_delim(
        file = library,
        col_types = readr::cols(.default = "c")
      ) |>
      dplyr::left_join(readr::read_delim(
        file = str_2D_3D,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_met,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_nam,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_tax_cla,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_tax_npc,
        col_types = readr::cols(.default = "c")
      )) |>
      # dplyr::left_join(readr::read_delim(file = org_tax_ott,
      #                                    col_types = readr::cols(.default = "c"))) |>
      dplyr::filter(!is.na(structure_exact_mass)) |>
      dplyr::mutate(dplyr::across(c(
        "structure_exact_mass",
        "structure_xlogp"
      ), as.numeric)) |>
      round_reals() |>
      dplyr::mutate(dplyr::across(
        dplyr::matches("taxonomy.*_0"),
        ~ tidyr::replace_na(.x, "notClassified")
      ))

    log_debug("filtering desired adducts and adding mz tolerance \n")
    df2 <- structureExactMassTable |>
      dplyr::filter(!is.na(exact_mass)) |>
      dplyr::filter(adduct %in% adducts) |>
      dplyr::mutate(
        value_min = adduct_mass - (1E-6 * tolerancePpm * adduct_mass),
        value_max = adduct_mass + (1E-6 * tolerancePpm * adduct_mass)
      ) |>
      dplyr::filter(!is.na(value_min)) |>
      dplyr::filter(value_min > 0) |>
      data.table::data.table()

    log_debug("setting to data.table format for faster performance \n")
    df3 <- featuresTable |>
      dplyr::mutate(dplyr::across(
        c(mz),
        as.numeric
      )) |>
      dplyr::mutate(mz_2 = mz) |>
      dplyr::distinct(feature_id, .keep_all = TRUE) |>
      data.table::data.table()

    if (any(names(featuresTable) == "rt")) {
      df3 <- df3 |>
        dplyr::mutate(dplyr::across(
          c(rt),
          as.numeric
        ))
    } else {
      df3[, "rt"] <- df3[, "feature_id"]
    }

    log_debug("adding rt tolerance ... \n")
    df4 <- df3 |>
      dplyr::mutate(dplyr::across(rt, as.numeric)) |>
      dplyr::mutate(
        rt_min = as.numeric(rt - toleranceRt),
        rt_max = as.numeric(rt + toleranceRt)
      )

    log_debug("... on the other side (without tolerance) \n")
    df5 <- df3 |>
      dplyr::mutate(dplyr::across(rt, as.numeric)) |>
      dplyr::mutate(
        rt_1 = as.numeric(rt),
        rt_2 = as.numeric(rt)
      ) |>
      data.table::data.table()

    log_debug("setting joining keys \n")
    data.table::setkey(df4, rt_min, rt_max)
    data.table::setkey(df5, rt_1, rt_2)

    log_debug("joining within given rt tolerance \n")
    df7 <- data.table::foverlaps(df4, df5) |>
      dplyr::distinct(
        feature_id,
        rt,
        mz,
        i.feature_id,
        i.mz
      ) |>
      dplyr::select(dplyr::everything(),
        feature_id_dest = i.feature_id,
        mz_dest = i.mz
      ) |>
      log_pipe("adding delta mz tolerance for single charge adducts \n") |>
      dplyr::filter(mz >= mz_dest) |>
      dplyr::mutate(
        delta_min = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz -
            (1E-6 *
              tolerancePpm *
              mz) -
            mz_dest),
          no = abs(mz + (1E-6 *
            tolerancePpm *
            mz) - mz_dest)
        ),
        delta_max = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz + (1E-6 *
            tolerancePpm *
            mz) - mz_dest),
          no = abs(mz -
            (1E-6 *
              tolerancePpm *
              mz) -
            mz_dest)
        )
      )

    log_debug("calculating delta mz for single charge adducts \n")
    df8 <-
      dist_groups(
        d = stats::dist(adductsTable$adduct_mass),
        g = adductsTable$adduct
      ) |>
      dplyr::mutate(Distance_2 = Distance) |>
      dplyr::select(-Item1, -Item2, -Label) |>
      data.table::data.table()

    log_debug("setting joining keys \n")
    data.table::setkey(df7, delta_min, delta_max)
    data.table::setkey(df8, Distance, Distance_2)

    log_debug("neutral losses \n")
    df8_a <- neutralLosses |>
      dplyr::mutate(mass_2 = mass) |>
      data.table::data.table()

    log_debug("setting joining keys \n")
    data.table::setkey(df8_a, mass, mass_2)

    log_debug("joining within given delta mz tolerance (neutral losses) \n")
    df9_d <- data.table::foverlaps(df7, df8_a) |>
      dplyr::filter(!is.na(loss)) |>
      dplyr::distinct(feature_id, loss, mass, feature_id_dest)

    df9_e <- df9_d |>
      dplyr::distinct(feature_id, loss, mass)

    log_debug("joining within given delta mz tolerance (adducts) \n")
    df9 <- data.table::foverlaps(df7, df8) |>
      dplyr::mutate(mz_2 = mz) |>
      dplyr::filter(!is.na(Group1)) |>
      dplyr::mutate(dplyr::across(rt, as.character)) |>
      dplyr::mutate(
        label = as.character(Group1),
        label_dest = as.character(Group2)
      ) |>
      dplyr::distinct(
        feature_id,
        label,
        label_dest, !!as.name(paste("feature_id", "dest", sep = "_"))
      )

    log_debug("keeping initial and destination feature \n")
    df9_a <- df9 |>
      dplyr::distinct(feature_id, label)

    df9_b <- df9 |>
      dplyr::distinct(!!as.name(paste("feature_id", "dest", sep = "_")), label_dest) |>
      dplyr::select(feature_id := !!as.name(paste("feature_id", "dest", sep = "_")),
        label = label_dest
      )

    ## Always considering [M+H]+ and [M-H]- ions by default
    df9_ion <- df3 |>
      dplyr::distinct(feature_id) |>
      dplyr::mutate(label = switch(msMode,
        "pos" = "pos_1_1proton",
        "neg" = "neg_1_1proton"
      ))

    df9_c <- dplyr::bind_rows(
      df9_a,
      df9_b,
      df9_ion
    ) |>
      dplyr::distinct()

    log_debug("joining with initial results (adducts) \n")
    df10 <- dplyr::left_join(
      df3 |>
        dplyr::distinct(
          feature_id,
          rt,
          mz,
          mz_2
        ),
      df9_c
    ) |>
      dplyr::mutate(
        mz_1 = mz,
        score_input = 0
      ) |>
      data.table::data.table()

    log_debug("joining with initial results (neutral losses) \n")
    df10_a <- dplyr::left_join(df10, df9_e) |>
      dplyr::mutate(
        mz_1 = ifelse(
          test = !is.na(loss),
          yes = mz_1 + mass,
          no = mz_1
        ),
        mz_2 = ifelse(
          test = !is.na(loss),
          yes = mz_2 + mass,
          no = mz_2
        )
      )

    log_debug("setting joining keys \n")
    data.table::setkey(df2, value_min, value_max)
    data.table::setkey(df10_a, mz_1, mz_2)

    log_debug("joining within given mz tolerance and filtering possible single charge adducts only \n")
    df11 <- data.table::foverlaps(
      df10_a,
      df2
    ) |>
      dplyr::mutate(
        mz_error = adduct_mass - mz_2,
        rt_error = NA_real_
      ) |>
      dplyr::select(
        feature_id,
        rt,
        mz,
        score_input,
        mz_error,
        rt_error,
        exact_mass,
        adduct,
        adduct_mass,
        loss
      ) |>
      dplyr::mutate(library = ifelse(
        test = !is.na(loss),
        yes = paste0(adduct, " - ", loss),
        no = adduct
      )) |>
      dplyr::distinct() |>
      dplyr::mutate_all(~ replace(
        .,
        . == "NA",
        NA
      )) |>
      dplyr::filter(!is.na(library))

    log_debug("cleaning results \n")
    df12 <- df11 |>
      dplyr::select(
        feature_id,
        rt,
        mz,
        score_input,
        library,
        mz_error,
        rt_error,
        exact_mass
      ) |>
      dplyr::filter(!is.na(exact_mass))

    ## HERE

    log_debug("keeping unique adducts per exact mass \n")
    df13 <- structureOrganismPairsTable |>
      dplyr::filter(!is.na(structure_exact_mass)) |>
      dplyr::distinct(
        structure_exact_mass,
        structure_molecular_formula
      )
    df13_b <- structureOrganismPairsTable |>
      dplyr::filter(!is.na(structure_exact_mass)) |>
      dplyr::distinct(
        structure_name,
        # structure_inchikey,
        structure_inchikey_2D,
        # structure_smiles,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp
      ) |>
      # Avoid SMILES redundancy
      dplyr::distinct(
        structure_inchikey_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        .keep_all = TRUE
      ) |>
      dplyr::mutate_all(as.character)

    ## TODO This will then be externalized somehow
    forbidden_adducts <- c(
      "pos_1_1proton1acetonitrile - NH3",
      "pos_1_1proton1ammonium - NH3",
      "pos_1_1proton1ethylamine - NH3",
      "pos_1_1proton1methanol - CH3COOH",
      "pos_1_1proton1methanol - H2O",
      "pos_1_1proton1methanol - O",
      "pos_1_1proton2acetonitrile - NH3",
      "pos_1_1sodium1acetonitrile - NH3",
      "pos_2_2proton1acetonitrile - NH3",
      "pos_2_2proton1ammonium - NH3",
      "pos_2_2proton2acetonitrile - NH3",
      "pos_2_2proton3acetonitrile - NH3"
    )

    "%ni%" <- Negate("%in%")

    log_debug("joining exact masses with single charge adducts \n")
    df14 <- dplyr::left_join(
      x = df12,
      y = df13,
      by = stats::setNames("structure_exact_mass", "exact_mass")
    ) |>
      dplyr::select(
        structure_molecular_formula,
        library,
        dplyr::everything(), -exact_mass
      ) |>
      dplyr::filter(library %ni% forbidden_adducts) |>
      dplyr::distinct()

    log_debug("adding adduct mass to get back to [M] \n")
    df15 <-
      dplyr::left_join(df14, adductsTable, by = stats::setNames("adduct", "library")) |>
      dplyr::distinct(feature_id, .keep_all = TRUE) |>
      dplyr::select(
        feature_id,
        adduct_mass
      ) |>
      dplyr::filter(!is.na(adduct_mass))

    log_debug("keeping these ions for dimers and multicharged exploration starting from [M] \n")
    df16 <- dplyr::inner_join(df3, df15)

    log_debug("calculating multicharged and in source dimers and adding delta mz tolerance \n")
    if (msMode == "pos") {
      df17 <- df16 |>
        dplyr::select(
          feature_id,
          rt,
          mz,
          adduct_mass
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          pos_3_3proton = (mz - adduct_mass + 3 * adductsM["H (proton)"]) / 3,
          pos_3_2proton1sodium = (mz - adduct_mass +
            2 * adductsM["H (proton)"] +
            adductsM["Na (sodium)"]) / 3,
          pos_3_1proton2sodium = (mz - adduct_mass +
            adductsM["H (proton)"] +
            2 * adductsM["Na (sodium)"]) / 3,
          pos_3_3sodium = (mz - adduct_mass + 3 * adductsM["Na (sodium)"]) / 3,
          pos_2_2proton = (mz - adduct_mass + 2 * adductsM["H (proton)"]) / 2,
          pos_2_2proton1ammonium = (mz - adduct_mass +
            2 * adductsM["H (proton)"] +
            adductsM["NH4 (ammonium)"]) / 2,
          pos_2_1proton1sodium = (mz - adduct_mass +
            adductsM["H (proton)"] +
            adductsM["Na (sodium)"]) / 2,
          pos_2_1magnesium = (mz - adduct_mass +
            adductsM["Mg (magnesium)"]) / 2,
          pos_2_1proton1potassium = (mz - adduct_mass +
            adductsM["H (proton)"] +
            adductsM["K (potassium)"]) / 2,
          pos_2_1calcium = (mz - adduct_mass +
            adductsM["Ca (calcium)"]) / 2,
          pos_2_2proton1acetonitrile = (mz - adduct_mass +
            2 * adductsM["H (proton)"] +
            adductsM["C2H3N (acetonitrile)"]) / 2,
          pos_2_2sodium = (mz - adduct_mass + 2 * adductsM["Na (sodium)"]) / 2,
          pos_2_1iron = (mz - adduct_mass +
            adductsM["Fe (iron)"]) / 2,
          pos_2_2proton2acetonitrile = (mz - adduct_mass +
            2 * adductsM["H (proton)"] +
            2 * adductsM["C2H3N (acetonitrile)"]) / 2,
          pos_2_2proton3acetonitrile = (mz - adduct_mass +
            2 * adductsM["H (proton)"] +
            3 * adductsM["C2H3N (acetonitrile)"]) / 2,
          pos_2MMg = (2 * (mz - adduct_mass) + adductsM["Mg (magnesium)"]) / 2,
          pos_2MCa = (2 * (mz - adduct_mass) + adductsM["Ca (calcium)"]) / 2,
          pos_2MFe = (2 * (mz - adduct_mass) + adductsM["Fe (iron)"]) / 2,
          pos_2MH = 2 * (mz - adduct_mass) + adductsM["H (proton)"],
          pos_2MHNH3 = 2 * (mz - adduct_mass) +
            adductsM["H (proton)"] +
            adductsM["NH4 (ammonium)"],
          pos_2MNa = 2 * (mz - adduct_mass) + adductsM["Na (sodium)"],
          pos_2MK = 2 * (mz - -adductsM["H (proton)"]) + adductsM["K (potassium)"],
          pos_2MHCH3CN = 2 * (mz - adduct_mass) +
            adductsM["H (proton)"] +
            adductsM["C2H3N (acetonitrile)"],
          pos_2MCH3CNNa = 2 * (mz - adduct_mass) +
            adductsM["C2H3N (acetonitrile)"] +
            adductsM["Na (sodium)"]
        ) |>
        dplyr::select(-adduct_mass) |>
        dplyr::ungroup()

      cols <- ncol(df17)

      df17 <- df17 |>
        tidyr::pivot_longer(cols = dplyr::all_of(4:cols))
    } else {
      df17 <- df16 |>
        dplyr::select(
          feature_id,
          rt,
          mz,
          adduct_mass
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          neg_3_3proton = (mz + adduct_mass - 3 * adductsM["H (proton)"]) / 3,
          neg_2_2proton = (mz + adduct_mass - 2 * adductsM["H (proton)"]) / 2,
          neg_2MH = 2 * (mz + adduct_mass) - adductsM["H (proton)"],
          neg_2MFAH = 2 * (mz + adduct_mass) + adductsM["CH2O2 (formic)"] - adductsM["H (proton)"],
          neg_2MACH = 2 * (mz + adduct_mass) + adductsM["C2H4O2 (acetic)"] - adductsM["H (proton)"],
          neg_3MH = 3 * (mz + adduct_mass) - adductsM["H (proton)"]
        ) |>
        dplyr::select(-adduct_mass) |>
        dplyr::ungroup()

      cols <- ncol(df17)

      df17 <- df17 |>
        tidyr::pivot_longer(cols = dplyr::all_of(4:cols))
    }

    df17 <- df17 |>
      dplyr::mutate(
        mz_min = value - (1E-6 * tolerancePpm * value),
        mz_max = value + (1E-6 * tolerancePpm * value),
        rt_min = rt - toleranceRt,
        rt_max = rt + toleranceRt
      ) |>
      data.table::data.table()

    df19 <- df5 |>
      dplyr::select(
        feature_id,
        rt,
        mz,
        rt_1,
        rt_2
      )

    log_debug("setting joining keys \n")
    data.table::setkey(df17, rt_min, rt_max)
    data.table::setkey(df19, rt_1, rt_2)

    log_debug("joining within given rt tolerance \n")
    df20 <- data.table::foverlaps(df17, df19) |>
      dplyr::mutate(
        delta_min = mz - mz_max,
        delta_max = mz - mz_min
      ) |>
      dplyr::filter(delta_max > min(df8_a$mass) |
        (mz >= mz_min & mz <= mz_max))

    log_debug("setting joining keys \n")
    data.table::setkey(df20, delta_min, delta_max)

    df20_a <- data.table::foverlaps(df20, df8_a) |>
      dplyr::filter(!is.na(loss) | (mz >= mz_min & mz <= mz_max)) |>
      dplyr::mutate(name = ifelse(
        test = !is.na(loss),
        yes = paste(name, "-", loss, sep = " "),
        no = name
      )) |>
      dplyr::select(
        feature_id,
        rt,
        mz,
        library_name = name,
        adduct_value = value,
        mz_min,
        mz_max
      )

    log_debug("setting joining keys \n")
    data.table::setkey(df20_a, mz_min, mz_max)
    data.table::setkey(df2, value_min, value_max)

    log_debug(
      "joining within given mz tolerance and filtering possible multicharge / dimeric adducts \n"
    )
    df21 <- data.table::foverlaps(df20_a, df2) |>
      dplyr::filter(stringr::str_detect(
        pattern = paste(adduct, "", sep = " "),
        string = library_name
      )) |>
      dplyr::mutate(mz_error = adduct_mass - adduct_value) |>
      dplyr::distinct(
        feature_id,
        rt,
        mz,
        exact_mass,
        library_name,
        mz_error,
        adduct_value
      )

    df22 <-
      dplyr::left_join(df21,
        df13,
        by = stats::setNames("structure_exact_mass", "exact_mass")
      ) |>
      dplyr::mutate(score_input = 0) |>
      dplyr::select(
        structure_molecular_formula,
        library = library_name,
        dplyr::everything(), -exact_mass, -adduct_value
      ) |>
      dplyr::filter(library %ni% forbidden_adducts) |>
      dplyr::mutate(library = as.character(library)) |>
      dplyr::distinct()

    log_debug("joining single adducts, neutral losses, and multicharged / dimers \n")
    df24 <- dplyr::bind_rows(
      df14 |>
        dplyr::left_join(df13_b),
      df22 |>
        dplyr::left_join(df13_b)
    ) |>
      dplyr::filter(!is.na(structure_inchikey_2D)) |>
      dplyr::group_by(feature_id) |>
      dplyr::ungroup() |>
      dplyr::distinct(
        feature_id,
        mz_error,
        rt_error,
        structure_name,
        # structure_inchikey,
        structure_inchikey_2D,
        # structure_smiles,
        structure_smiles_2D,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        library,
        score_input
      )

    log_debug("adding chemical classification")
    df25 <- dplyr::left_join(
      df24,
      structureOrganismPairsTable |>
        dplyr::distinct(
          structure_inchikey_2D,
          structure_smiles_2D,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          ## TODO until better
          structure_taxonomy_classyfire_chemontid,
          structure_taxonomy_classyfire_01kingdom,
          structure_taxonomy_classyfire_02superclass,
          structure_taxonomy_classyfire_03class,
          structure_taxonomy_classyfire_04directparent
        )
    )

    df25 |>
      decorate_masses()

    edges <- dplyr::bind_rows(
      df9 |>
        dplyr::mutate(label = paste0(label, " _ ", label_dest)) |>
        dplyr::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        dplyr::distinct(),
      df9_d |>
        dplyr::mutate(label = paste0(loss, " loss")) |>
        dplyr::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        dplyr::distinct()
    )

    export_params(step = "annotate_masses")
    export_output(x = edges, file = output_edges[[1]])
    export_output(x = df25, file = output_annotations[[1]])

    return(c(
      "annotations" = output_annotations[[1]],
      "edges" = output_edges[[1]]
    ))
  }
