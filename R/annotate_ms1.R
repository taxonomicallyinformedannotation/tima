#' @title Annotate MS1
#'
#' @param annotationTable TODO
#' @param structureExactMassTable TODO
#' @param structureOrganismPairsTable TODO
#' @param adducts TODO
#' @param neutralLosses TODO
#' @param msMode TODO
#' @param tolerancePpm TODO
#' @param toleranceRt TODO
#' @param candidatesInitial TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom data.table data.table foverlaps setkey
#' @importFrom dplyr across bind_rows dense_rank desc distinct everything
#' @importFrom dplyr filter group_by inner_join left_join mutate mutate_all
#' @importFrom dplyr rowwise select ungroup
#' @importFrom stats dist setNames
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#'
#' @examples TODO
annotate_ms1 <-
  function(annotationTable = metadata_table_spectral_annotation,
           structureExactMassTable = structure_exact_mass_table,
           structureOrganismPairsTable = structure_organism_pairs_table,
           adducts = unlist(adducts_list[[ms_mode]]),
           neutralLosses = neutral_losses_table,
           msMode = ms_mode,
           tolerancePpm = tolerance_ppm,
           toleranceRt = tolerance_rt,
           candidatesInitial = candidates_initial) {
    cat("filtering desired adducts and adding mz tolerance \n")
    df2 <- structureExactMassTable |>
      dplyr::filter(!is.na(exact_mass)) |>
      dplyr::filter(adduct %in% adducts) |>
      dplyr::mutate(
        value_min = adduct_mass - (0.000001 * tolerancePpm * adduct_mass),
        value_max = adduct_mass + (0.000001 * tolerancePpm * adduct_mass)
      ) |>
      dplyr::filter(!is.na(value_min)) |>
      dplyr::filter(value_min > 0) |>
      data.table::data.table()

    cat("setting to data.table format for faster performance \n")
    df3 <- annotationTable |>
      dplyr::mutate(dplyr::across(
        c(
          mz,
          component_id,
        ),
        as.numeric
      )) |>
      dplyr::mutate(mz_2 = mz) |>
      dplyr::distinct(feature_id, .keep_all = TRUE) |>
      data.table::data.table()

    if (any(names(annotationTable) == "rt")) {
      df3 <- df3 |>
        dplyr::mutate(dplyr::across(
          c(rt),
          as.numeric
        ))
    } else {
      df3[, "rt"] <- df3[, "feature_id"]
    }

    cat("adding rt tolerance ... \n")
    df4 <- df3 |>
      dplyr::mutate(dplyr::across(rt, as.numeric)) |>
      dplyr::mutate(
        rt_min = as.numeric(rt - toleranceRt),
        rt_max = as.numeric(rt + toleranceRt)
      )

    cat("... on the other side (without tolerance) \n")
    df5 <- df3 |>
      dplyr::mutate(dplyr::across(rt, as.numeric)) |>
      dplyr::mutate(
        rt_1 = as.numeric(rt),
        rt_2 = as.numeric(rt)
      ) |>
      data.table::data.table()

    cat("setting joining keys \n")
    data.table::setkey(df4, rt_min, rt_max)
    data.table::setkey(df5, rt_1, rt_2)

    cat("joining within given rt tolerance \n")
    df6 <- data.table::foverlaps(df4, df5) |>
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
      )

    cat("adding delta mz tolerance for single charge adducts \n")
    df7 <- df6 |>
      dplyr::filter(mz >= mz_dest) |>
      dplyr::mutate(
        delta_min = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz -
            (0.000001 *
              tolerancePpm *
              mz) -
            mz_dest),
          no = abs(mz + (0.000001 *
            tolerancePpm *
            mz) - mz_dest)
        ),
        delta_max = ifelse(
          test = mz >= mz_dest,
          yes = abs(mz + (0.000001 *
            tolerancePpm *
            mz) - mz_dest),
          no = abs(mz -
            (0.000001 *
              tolerancePpm *
              mz) -
            mz_dest)
        )
      )

    cat("calculating delta mz for single charge adducts \n")
    df8 <-
      dist_groups(
        d = stats::dist(adductsTable$adduct_mass),
        g = adductsTable$adduct
      ) |>
      dplyr::mutate(Distance_2 = Distance) |>
      dplyr::select(
        -Item1,
        -Item2,
        -Label
      ) |>
      data.table::data.table()

    cat("setting joining keys \n")
    data.table::setkey(df7, delta_min, delta_max)
    data.table::setkey(df8, Distance, Distance_2)

    cat("neutral losses \n")
    df8_a <- neutralLosses |>
      dplyr::mutate(mass_2 = mass) |>
      data.table::data.table()

    cat("setting joining keys \n")
    data.table::setkey(df8_a, mass, mass_2)

    cat("joining within given delta mz tolerance (neutral losses) \n")
    df9_d <- data.table::foverlaps(df7, df8_a) |>
      dplyr::filter(!is.na(loss)) |>
      dplyr::distinct(feature_id, loss, mass)

    cat("joining within given delta mz tolerance (adducts) \n")
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
        label_dest,
        !!as.name(paste("feature_id", "dest", sep = "_"))
      )

    cat("keeping initial and destination feature \n")
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

    cat("joining with initial results (adducts) \n")
    df10 <- dplyr::left_join(
      df3 |>
        dplyr::distinct(
          feature_id,
          component_id,
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

    cat("joining with initial results (neutral losses) \n")
    df10_a <- dplyr::left_join(df10, df9_d) |>
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

    cat("setting joining keys \n")
    data.table::setkey(df2, value_min, value_max)
    data.table::setkey(df10_a, mz_1, mz_2)

    cat("joining within given mz tolerance and filtering possible single charge adducts only \n")
    df11 <- data.table::foverlaps(
      df10_a,
      df2
    ) |>
      dplyr::mutate(mz_error = adduct_mass - mz_2) |>
      dplyr::select(
        feature_id,
        component_id,
        rt,
        mz,
        score_input,
        mz_error,
        exact_mass,
        adduct,
        adduct_mass,
        loss
      ) |>
      dplyr::mutate(library = adduct) |>
      dplyr::distinct() |>
      dplyr::mutate_all(~ replace(
        .,
        . == "NA",
        NA
      )) |>
      dplyr::filter(!is.na(adduct))

    cat("cleaning results \n")
    df12 <- df11 |>
      dplyr::select(
        feature_id,
        component_id,
        rt,
        mz,
        score_input,
        library,
        mz_error,
        exact_mass
      ) |>
      dplyr::filter(!is.na(exact_mass))

    cat("keeping unique adducts per exact mass \n")
    df13 <- structureOrganismPairsTable |>
      dplyr::filter(!is.na(structure_exact_mass)) |>
      dplyr::distinct(
        structure_exact_mass,
        structure_molecular_formula,
        structure_inchikey_2D,
        structure_smiles_2D
      )

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

    cat("joining exact masses with single charge adducts \n")
    df14 <- dplyr::left_join(
      x = df12,
      y = df13,
      by = stats::setNames("structure_exact_mass", "exact_mass")
    ) |>
      dplyr::select(
        molecular_formula = structure_molecular_formula,
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        library,
        dplyr::everything(),
        -exact_mass
      ) |>
      dplyr::filter(!is.na(inchikey_2D)) |>
      dplyr::filter(library %ni% forbidden_adducts) |>
      dplyr::distinct()

    cat("adding adduct mass to get back to [M] \n")
    df15 <-
      dplyr::left_join(df14, adductsTable, by = stats::setNames("adduct", "library")) |>
      dplyr::distinct(feature_id, .keep_all = TRUE) |>
      dplyr::select(
        feature_id,
        adduct_mass
      ) |>
      dplyr::filter(!is.na(adduct_mass))

    cat("keeping these ions for dimers and multicharged exploration starting from [M] \n")
    df16 <- dplyr::inner_join(df3, df15)

    cat("calculating multicharged and in source dimers and adding delta mz tolerance \n")
    if (msMode == "pos") {
      df17 <- df16 |>
        dplyr::select(
          feature_id,
          component_id,
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
          pos_2_2proton1ammonium = (
            mz - adduct_mass +
              2 * adductsM["H (proton)"] +
              adductsM["NH4 (ammonium)"]
          ) / 2,
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
          pos_2_2proton1acetonitrile = (
            mz - adduct_mass +
              2 * adductsM["H (proton)"] +
              adductsM["C2H3N (acetonitrile)"]
          ) / 2,
          pos_2_2sodium = (mz - adduct_mass + 2 * adductsM["Na (sodium)"]) / 2,
          pos_2_1iron = (mz - adduct_mass +
            adductsM["Fe (iron)"]) / 2,
          pos_2_2proton2acetonitrile = (
            mz - adduct_mass +
              2 * adductsM["H (proton)"] +
              2 * adductsM["C2H3N (acetonitrile)"]
          ) / 2,
          pos_2_2proton3acetonitrile = (
            mz - adduct_mass +
              2 * adductsM["H (proton)"] +
              3 * adductsM["C2H3N (acetonitrile)"]
          ) / 2,
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
        tidyr::pivot_longer(cols = dplyr::all_of(5:cols))
    }

    if (msMode == "neg") {
      df17 <- df16 |>
        dplyr::select(
          feature_id,
          component_id,
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
        tidyr::pivot_longer(cols = dplyr::all_of(5:cols))
    }

    df17 <- df17 |>
      dplyr::mutate(
        mz_min = value - (0.000001 * tolerancePpm * value),
        mz_max = value + (0.000001 * tolerancePpm * value),
        rt_min = rt - toleranceRt,
        rt_max = rt + toleranceRt
      ) |>
      data.table::data.table()

    df19 <- df5 |>
      dplyr::select(
        feature_id,
        component_id,
        rt,
        mz,
        rt_1,
        rt_2
      )

    cat("setting joining keys \n")
    data.table::setkey(df17, rt_min, rt_max)
    data.table::setkey(df19, rt_1, rt_2)

    cat("joining within given rt tolerance \n")
    df20 <- data.table::foverlaps(df17, df19) |>
      dplyr::mutate(
        delta_min = mz - mz_max,
        delta_max = mz - mz_min
      ) |>
      dplyr::filter(delta_max > min(df8_a$mass) |
        (mz >= mz_min & mz <= mz_max))

    cat("setting joining keys \n")
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
        component_id,
        rt,
        mz,
        library_name = name,
        adduct_value = value,
        mz_min,
        mz_max
      )

    cat("setting joining keys \n")
    data.table::setkey(df20_a, mz_min, mz_max)
    data.table::setkey(df2, value_min, value_max)

    cat(
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
        component_id,
        rt,
        mz,
        exact_mass,
        library_name,
        mz_error,
        adduct_value
      )

    df22 <-
      dplyr::left_join(df21, df13, by = stats::setNames("structure_exact_mass", "exact_mass")) |>
      dplyr::mutate(score_input = 0) |>
      dplyr::select(
        molecular_formula = structure_molecular_formula,
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        library = library_name,
        dplyr::everything(),
        -exact_mass,
        -adduct_value
      ) |>
      dplyr::filter(!is.na(inchikey_2D)) |>
      dplyr::filter(library %ni% forbidden_adducts) |>
      dplyr::distinct()

    cat("formatting initial results \n")
    if (any(names(annotationTable) == "rt")) {
      df23 <- annotationTable |>
        dplyr::mutate(dplyr::across(
          c(
            mz_error,
            component_id,
            mz,
            rt,
            score_input
          ),
          as.numeric
        )) |>
        dplyr::distinct()
    } else {
      df23 <- annotationTable |>
        dplyr::mutate(dplyr::across(
          c(
            mz_error,
            component_id,
            mz,
            score_input
          ),
          as.numeric
        )) |>
        dplyr::distinct()
    }

    cat(
      "joining MS2 results, single adducts, neutral losses, and multicharged / dimers and ranking \n"
    )
    df24 <- dplyr::bind_rows(df23, df14, df22) |>
      dplyr::group_by(feature_id) |>
      dplyr::mutate(rank_initial = dplyr::dense_rank(dplyr::desc(score_input))) |>
      dplyr::ungroup() |>
      dplyr::filter(rank_initial <= candidatesInitial) |>
      dplyr::distinct(
        feature_id,
        component_id,
        score_input,
        library,
        mz_error,
        molecular_formula,
        inchikey_2D,
        smiles_2D,
        rank_initial
      )

    if (!any(names(annotationTable) == "rt")) {
      annotationTable[, "rt"] <- 0
    }

    df25 <- annotationTable |>
      dplyr::select(
        feature_id,
        component_id,
        rt,
        mz,
      ) |>
      dplyr::distinct() |>
      dplyr::mutate_all(as.numeric)

    cat("adding \"notAnnotated\" \n")
    df26 <- dplyr::left_join(df25, df24) |>
      dplyr::distinct() |>
      dplyr::mutate(dplyr::across(mz_error, as.numeric)) |>
      data.frame()

    df26["inchikey_2D"][is.na(df26["inchikey_2D"])] <-
      "notAnnotated"
    df26["score_input"][is.na(df26["score_input"])] <-
      0
    df26["library"][is.na(df26["library"])] <-
      "N/A"
    df26["mz_error"][is.na(df26["mz_error"])] <-
      666
    df26["rank_initial"][is.na(df26["rank_initial"])] <-
      candidatesInitial

    df27 <- dplyr::left_join(
      df26,
      structureOrganismPairsTable |>
        dplyr::distinct(
          inchikey_2D = structure_inchikey_2D,
          smiles_2D = structure_smiles_2D,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class
        )
    )

    return(df27)
  }
