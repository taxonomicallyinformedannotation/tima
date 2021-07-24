#######################   Functions - ms1 annotation    #######################

# require(...)

# annotates features with the selected DB base on exact mass match only
#' Title
#'
#' @param annotationTable
#' @param structureExactMassTable
#' @param structureOrganismPairsTable
#' @param adducts
#' @param neutralLosses
#'
#' @return
#' @export
#'
#' @examples
ms1_annotation <-
  function(annotationTable = metadata_table_spectral_annotation,
           structureExactMassTable = structure_exact_mass_table,
           structureOrganismPairsTable = structure_organism_pairs_table,
           adducts = unlist(params$ms$adducts[[params$ms$mode]]),
           neutralLosses = neutral_losses_table) {
    cat("filtering desired adducts and adding mz tolerance \n")
    df2 <- structureExactMassTable %>%
      filter(!is.na(exact_mass)) %>%
      filter(adduct %in% adducts) %>%
      mutate(
        value_min = adduct_mass - (0.000001 * params$ms$tolerance$ppm * adduct_mass),
        value_max = adduct_mass + (0.000001 * params$ms$tolerance$ppm * adduct_mass)
      ) %>%
      filter(!is.na(value_min)) %>%
      filter(value_min > 0) %>%
      data.table()

    cat("setting to data.table format for faster performance \n")
    df3 <- annotationTable %>%
      mutate(across(
        c(
          mz,
          component_id,
          rt
        ),
        as.numeric
      )) %>%
      mutate(mz_2 = mz) %>%
      distinct(feature_id, .keep_all = TRUE) %>%
      data.table()

    if (!any(names(annotationTable) == "rt")) {
      df3[, "rt"] <- df3["feature_id"]
    }

    cat("adding rt tolerance ... \n")
    df4 <- df3 %>%
      mutate(across(rt, as.numeric)) %>%
      mutate(
        rt_min = as.numeric(rt - params$ms$tolerance$rt),
        rt_max = as.numeric(rt + params$ms$tolerance$rt)
      )

    cat("... on the other side (without tolerance) \n")
    df5 <- df3 %>%
      mutate(across(rt, as.numeric)) %>%
      mutate(
        rt_1 = as.numeric(rt),
        rt_2 = as.numeric(rt)
      ) %>%
      data.table()

    cat("setting joining keys \n")
    setkey(df4, rt_min, rt_max)
    setkey(df5, rt_1, rt_2)

    cat("joining within given rt tolerance \n")
    df6 <- foverlaps(df4, df5) %>%
      distinct(
        feature_id,
        rt,
        mz,
        !!as.name(paste0("i.", "feature_id")),
        !!as.name(paste0("i.", "mz")),
      ) %>%
      select(
        everything(),
        !!as.name(paste("feature_id", "dest", sep = "_")) := !!as.name(paste0("i.", "feature_id")),
        !!as.name(paste("mz", "dest", sep = "_")) :=
          !!as.name(paste0("i.", "mz"))
      )

    cat("adding delta mz tolerance for single charge adducts \n")
    df7 <- df6 %>%
      mutate(
        delta_min = ifelse(
          test = mz >= !!as.name(paste("mz", "dest", sep = "_")),
          yes = abs(
            mz -
              (0.000001 *
                params$ms$tolerance$ppm *
                mz) - !!as.name(paste("mz", "dest", sep = "_"))
          ),
          no = abs(
            mz + (0.000001 *
              params$ms$tolerance$ppm *
              mz) - !!as.name(paste("mz", "dest", sep = "_"))
          )
        ),
        delta_max = ifelse(
          test = mz >= !!as.name(paste("mz", "dest", sep = "_")),
          yes = abs(
            mz + (0.000001 *
              params$ms$tolerance$ppm *
              mz) - !!as.name(paste("mz", "dest", sep = "_"))
          ),
          no = abs(
            mz -
              (0.000001 *
                params$ms$tolerance$ppm *
                mz) - !!as.name(paste("mz", "dest", sep = "_"))
          )
        )
      )

    cat("calculating delta mz for single charge adducts \n")
    df8 <-
      dist_groups(
        d = dist(adductsTable$adduct_mass),
        g = adductsTable$adduct
      ) %>%
      mutate(Distance_2 = Distance) %>%
      select(
        -Item1,
        -Item2,
        -Label
      ) %>%
      data.table()

    cat("setting joining keys \n")
    setkey(df7, delta_min, delta_max)
    setkey(df8, Distance, Distance_2)

    cat("neutral losses \n")
    df8_a <- neutralLosses %>%
      mutate(mass_2 = mass) %>%
      data.table()

    cat("setting joining keys \n")
    setkey(df8_a, mass, mass_2)

    cat("joining within given delta mz tolerance (neutral losses) \n")
    df9_d <- foverlaps(df7, df8_a) %>%
      filter(!is.na(loss)) %>%
      distinct(feature_id, loss, mass)

    cat("joining within given delta mz tolerance (adducts) \n")
    df9 <- foverlaps(df7, df8) %>%
      mutate(mz_2 = mz) %>%
      filter(!is.na(Group1)) %>%
      mutate(across(rt, as.character)) %>%
      mutate(
        label = as.character(Group1),
        label_dest = as.character(Group2)
      ) %>%
      distinct(
        feature_id,
        label,
        label_dest,
        !!as.name(paste("feature_id", "dest", sep = "_"))
      )

    cat("keeping initial and destination feature \n")
    df9_a <- df9 %>%
      distinct(feature_id, label)

    df9_b <- df9 %>%
      distinct(!!as.name(paste("feature_id", "dest", sep = "_")), label_dest) %>%
      select(feature_id := !!as.name(paste("feature_id", "dest", sep = "_")),
        label = label_dest
      )

    ## always considering [M+H]+ and [M-H]- ions
    df9_ion <- df3 %>%
      distinct(feature_id) %>%
      mutate(label = switch(params$ms$mode,
        "pos" = "pos_1_1proton",
        "neg" = "neg_1_1proton"
      ))

    df9_c <- bind_rows(df9_a, df9_b, df9_ion) %>% distinct()

    cat("joining with initial results (adducts) \n")
    df10 <- left_join(df3, df9_c) %>%
      mutate(mz_1 = mz) %>%
      data.table()

    cat("joining with initial results (neutral losses) \n")
    df10_a <- left_join(df10, df9_d) %>%
      mutate(
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
    setkey(df2, value_min, value_max)
    setkey(df10_a, mz_1, mz_2)

    cat("joining within given mz tolerance and filtering possible single charge adducts only \n")
    df11 <- foverlaps(
      df10_a,
      df2
    ) %>%
      select(
        feature_id,
        component_id,
        rt,
        mz,
        score_input,
        library,
        mz_error,
        exact_mass,
        adduct,
        adduct_mass,
        loss
      ) %>%
      mutate_all(~ replace(
        .,
        . == "NA",
        NA
      )) %>%
      filter(!is.na(adduct))

    cat("cleaning results \n")
    df12 <- left_join(df3, df11) %>%
      mutate(
        score_input = as.numeric(0),
        library = ifelse(
          test = !is.na(loss),
          yes = paste(adduct, "-", loss, sep = " "),
          no = adduct
        ),
        mz_error = as.numeric(adduct_mass - mz)
      ) %>%
      select(
        feature_id,
        component_id,
        rt,
        mz,
        score_input,
        library,
        mz_error,
        exact_mass,
      ) %>%
      filter(!is.na(exact_mass))

    cat("keeping unique adducts per exact mass \n")
    df13 <- structureOrganismPairsTable %>%
      filter(!is.na(structure_exact_mass)) %>%
      distinct(
        structure_exact_mass,
        structure_inchikey_2D,
        structure_smiles_2D
      )

    # this will then be externalized somehow
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
    df14 <- left_join(
      x = df12,
      y = df13,
      by = setNames("structure_exact_mass", "exact_mass")
    ) %>%
      select(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        library,
        everything(),
        -exact_mass
      ) %>%
      filter(!is.na(inchikey_2D)) %>%
      filter(library %ni% forbidden_adducts) %>%
      distinct()

    cat("adding adduct mass to get back to [M] \n")
    df15 <-
      left_join(df14, adductsTable, by = setNames("adduct", "library")) %>%
      distinct(feature_id, .keep_all = TRUE) %>%
      select(
        feature_id,
        adduct_mass
      ) %>%
      filter(!is.na(adduct_mass))

    cat("keeping these ions for dimers and multicharged exploration starting from [M] \n")
    df16 <- inner_join(df3, df15)

    cat("calculating multicharged and in source dimers and adding delta mz tolerance \n")
    if (params$ms$mode == "pos") {
      df17 <- df16 %>%
        select(
          feature_id,
          component_id,
          rt,
          mz,
          adduct_mass
        ) %>%
        rowwise() %>%
        mutate(
          pos_3_3proton = (mz - adduct_mass + 3 * adductsM["proton"]) / 3,
          pos_3_2proton1sodium = (mz - adduct_mass +
            2 * adductsM["proton"] +
            adductsM["sodium"]) / 3,
          pos_3_1proton2sodium = (mz - adduct_mass +
            adductsM["proton"] +
            2 * adductsM["sodium"]) / 3,
          pos_3_3sodium = (mz - adduct_mass + 3 * adductsM["sodium"]) / 3,
          pos_2_2proton = ((mz - adduct_mass + 2 * adductsM["proton"]) / 2),
          pos_2_2proton1ammonium = ((
            mz - adduct_mass +
              2 * adductsM["proton"] +
              adductsM["ammonium"]
          ) / 2),
          pos_2_1proton1sodium = ((mz - adduct_mass +
            adductsM["proton"] +
            adductsM["sodium"]) / 2),
          pos_2_1proton1potassium = ((mz - adduct_mass +
            adductsM["proton"] +
            adductsM["potassium"]) / 2),
          pos_2_2proton1acetonitrile = ((
            mz - adduct_mass +
              2 * adductsM["proton"] +
              adductsM["acetonitrile"]
          ) / 2),
          pos_2_2sodium = ((mz - adduct_mass + 2 * adductsM["sodium"]) / 2),
          pos_2_2proton2acetonitrile = ((
            mz - adduct_mass +
              2 * adductsM["proton"] +
              2 * adductsM["acetonitrile"]
          ) / 2),
          pos_2_2proton3acetonitrile = ((
            mz - adduct_mass +
              2 * adductsM["proton"] +
              3 * adductsM["acetonitrile"]
          ) / 2),
          pos_2MH = 2 * (mz - adduct_mass) + adductsM["proton"],
          pos_2MHNH3 = 2 * (mz - adduct_mass) +
            adductsM["proton"] +
            adductsM["ammonium"],
          pos_2MNa = 2 * (mz - adduct_mass) + adductsM["sodium"],
          pos_2MK = 2 * (mz - -adductsM["proton"]) + adductsM["potassium"],
          pos_2MHCH3CN = 2 * (mz - adduct_mass) +
            adductsM["proton"] +
            adductsM["acetonitrile"],
          pos_2MCH3CNNa = 2 * (mz - adduct_mass) +
            adductsM["acetonitrile"] +
            adductsM["sodium"]
        ) %>%
        select(-adduct_mass) %>%
        ungroup() %>%
        pivot_longer(cols = 5:ncol(.))
    }

    if (params$ms$mode == "neg") {
      df17 <- df16 %>%
        select(
          feature_id,
          component_id,
          rt,
          mz,
          adduct_mass
        ) %>%
        rowwise() %>%
        mutate(
          neg_3_3proton = (mz + adduct_mass - 3 * adductsM["proton"]) / 3,
          neg_2_2proton = ((mz + adduct_mass - 2 * adductsM["proton"]) / 2),
          neg_2MH = 2 * (mz + adduct_mass) - adductsM["proton"],
          neg_2MFAH = 2 * (mz + adduct_mass) + adductsM["formic"] - adductsM["proton"],
          neg_2MACH = 2 * (mz + adduct_mass) + adductsM["acetic"] - adductsM["proton"],
          neg_3MH = 3 * (mz + adduct_mass) - adductsM["proton"]
        ) %>%
        select(-adduct_mass) %>%
        ungroup() %>%
        pivot_longer(cols = 5:ncol(.))
    }

    df17 <- df17 %>%
      mutate(
        mz_min = value - (0.000001 * params$ms$tolerance$ppm * value),
        mz_max = value + (0.000001 * params$ms$tolerance$ppm * value),
        rt_min = rt - params$ms$tolerance$rt,
        rt_max = rt + params$ms$tolerance$rt
      ) %>%
      data.table()

    df19 <- df5 %>% select(
      feature_id,
      component_id,
      rt,
      mz,
      rt_1,
      rt_2
    )

    cat("setting joining keys \n")
    setkey(df17, rt_min, rt_max)
    setkey(df19, rt_1, rt_2)

    cat("joining within given rt tolerance \n")
    df20 <- foverlaps(df17, df19) %>%
      mutate(
        delta_min = mz - mz_max,
        delta_max = mz - mz_min
      ) %>%
      filter(delta_max > min(df8_a$mass) |
        (mz >= mz_min & mz <= mz_max))

    cat("setting joining keys \n")
    setkey(df20, delta_min, delta_max)

    df20_a <- foverlaps(df20, df8_a) %>%
      filter(!is.na(loss) | (mz >= mz_min & mz <= mz_max)) %>%
      mutate(name = ifelse(
        test = !is.na(loss),
        yes = paste(name, "-", loss, sep = " "),
        no = name
      )) %>%
      select(
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
    setkey(df20_a, mz_min, mz_max)
    setkey(df2, value_min, value_max)

    cat(
      "joining within given mz tolerance and filtering possible multicharge / dimeric adducts \n"
    )
    df21 <- foverlaps(df20_a, df2) %>%
      filter(str_detect(
        pattern = paste(adduct, "", sep = " "),
        string = library_name
      )) %>%
      mutate(mz_error = adduct_mass - adduct_value) %>%
      select(
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
      left_join(df21, df13, by = setNames("structure_exact_mass", "exact_mass")) %>%
      mutate(score_input = 0) %>%
      select(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        library = library_name,
        everything(),
        -exact_mass,
        -adduct_value
      ) %>%
      filter(!is.na(inchikey_2D)) %>%
      filter(library %ni% forbidden_adducts) %>%
      distinct()

    cat("formatting initial results \n")
    df23 <- annotationTable %>%
      mutate(across(
        c(
          mz_error,
          component_id,
          mz,
          rt,
          score_input
        ),
        as.numeric
      )) %>%
      distinct()

    cat(
      "joining MS2 results, single adducts, neutral losses, and multicharged / dimers and ranking \n"
    )
    df24 <- bind_rows(df23, df14, df22) %>%
      group_by(feature_id) %>%
      mutate(rank_initial = dense_rank(desc(score_input))) %>%
      ungroup() %>%
      filter(rank_initial <= params$top_k$initial) %>%
      distinct(
        feature_id,
        component_id,
        score_input,
        library,
        mz_error,
        inchikey_2D,
        smiles_2D,
        rank_initial
      )

    if (!any(names(annotationTable) == "rt")) {
      annotationTable[, "rt"] <- 0
    }

    df25 <- annotationTable %>%
      select(
        feature_id,
        component_id,
        rt,
        mz,
      ) %>%
      mutate_all(as.numeric)

    cat("adding \"notAnnotated\" \n")
    df26 <- left_join(df25, df24) %>%
      distinct() |> 
      mutate(across(mz, as.numeric)) |>
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
      params$top_k$initial

    df27 <- dplyr::left_join(
      df26,
      structureOrganismPairsTable |> distinct(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class
      )
    )

    return(df27)
  }

###############################################################################

#' Title
#'
#' @param annotationTable
#'
#' @return
#' @export
#'
#' @examples
non_ms1_annotation <-
  function(annotationTable = metadata_table_spectral_annotation) {
    cat("formatting \n")
    df15 <- annotationTable %>%
      mutate(across(
        c(
          mz_error,
          component_id,
          mz,
          rt,
          score_input
        ),
        as.numeric
      )) %>%
      distinct()

    cat("ranking \n")
    df16 <- df15 %>%
      group_by(feature_id) %>%
      mutate(rank_initial = dense_rank(desc(score_input))) %>%
      ungroup() %>%
      filter(rank_initial <= params$top_k$initial) %>%
      select(
        -rt,
        -mz
      )

    if (!any(names(annotationTable) == "rt")) {
      annotationTable[, "rt"] <- 0
    }

    df17 <- annotationTable %>%
      select(
        feature_id,
        component_id,
        mz,
        rt,
      )

    cat("adding \"notAnnotated\" \n")
    df18 <- left_join(df17, df16) |>
      distinct() |> 
      mutate(across(mz_error, as.numeric)) |>
      data.frame()

    df18["inchikey_2D"][is.na(df18["inchikey_2D"])] <-
      "notAnnotated"
    df18["score_input"][is.na(df18["score_input"])] <-
      0
    df18["library"][is.na(df18["library"])] <-
      "N/A"
    df18["mz_error"][is.na(df18["mz_error"])] <-
      666
    df18["rank_initial"][is.na(df18["rank_initial"])] <-
      params$top_k$initial

    df19 <- dplyr::left_join(
      df18,
      structure_organism_pairs_table |> distinct(
        inchikey_2D = structure_inchikey_2D,
        smiles_2D = structure_smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class
      )
    )

    return(df19)
  }

###############################################################################
