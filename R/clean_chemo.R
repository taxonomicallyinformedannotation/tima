utils::globalVariables(
  c(
    "best_candidate",
    "best_candidate_organism",
    "best_candidate_structure",
    "candidates_final",
    "component_id",
    "consensus_structure_cla_cla",
    "consensus_structure_cla_kin",
    "consensus_structure_cla_par",
    "consensus_structure_cla_sup",
    "consensus_structure_npc_cla",
    "consensus_structure_npc_pat",
    "consensus_structure_npc_sup",
    "consistency_structure_cla_cla",
    "consistency_structure_cla_kin",
    "consistency_structure_cla_par",
    "consistency_structure_cla_sup",
    "consistency_structure_npc_cla",
    "consistency_structure_npc_pat",
    "consistency_structure_npc_sup",
    "count_peaks_explained",
    "count_peaks_matched",
    "error_mz",
    "error_rt",
    "feature_id",
    "features_table",
    "minimal_ms1_bio",
    "minimal_ms1_chemo",
    "mz",
    "organism_name",
    "rank_final",
    "rank_initial",
    "reference_doi",
    "rt",
    "score_biological",
    "score_chemical",
    "score_final",
    "score_input",
    # "score_input_normalized",
    "score_interim",
    "score_pondered_bio",
    "score_pondered_chemo",
    "structure_01_cla_kingdom",
    "structure_01_npc_pathway",
    "structure_02_cla_superclass",
    "structure_02_npc_superclass",
    "structure_03_cla_class",
    "structure_03_npc_class",
    "structure_04_cla_parent",
    "structure_exact_mass",
    "structure_inchikey_2D",
    "structure_molecular_formula",
    "structure_name",
    "structure_organism_pairs_table",
    "structure_smiles_2D",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_xlogp",
    "summarise",
    "value"
  )
)

#' @title Clean chemo
#'
#' @description This function cleans the results obtained after chemical weighting
#'
#' @param annotationTableWeightedChemo Table containing your chemically weighted annotation
#' @param componentsTable Prepared components file
#' @param featuresTable Prepared features file
#' @param structureOrganismPairsTable Table containing the structure - organism pairs
#' @param candidatesFinal Number of final candidates to keep
#' @param minimalMs1Bio Minimal biological score to keep MS1 based annotation
#' @param minimalMs1Chemo Minimal chemical score to keep MS1 based annotation
#' @param summarize Boolean. Summarize results (1 row per feature)
#'
#' @return A table containing the chemically weighted annotation where only a given number of initial candidates are kept
#'
#' @export
#'
#' @seealso weight_chemo
#'
#' @examples NULL
clean_chemo <-
  function(annotationTableWeightedChemo = annotation_table_weighted_chemo,
           componentsTable = components_table,
           featuresTable = features_table,
           structureOrganismPairsTable = structure_organism_pairs_table,
           candidatesFinal = candidates_final,
           minimalMs1Bio = minimal_ms1_bio,
           minimalMs1Chemo = minimal_ms1_chemo,
           summarize = summarise) {
    log_debug(
      "filtering top ",
      candidatesFinal,
      " candidates and keeping only MS1 candidates with minimum \n",
      minimalMs1Bio,
      " biological score or \n",
      minimalMs1Chemo,
      "chemical score \n"
    )
    df1 <- annotationTableWeightedChemo |>
      tidytable::filter(
        score_input > 0 |
          # Those lines are to keep ms1 annotation
          score_biological >= minimalMs1Bio |
          # Only if a good biological
          score_chemical >= minimalMs1Chemo
        # Or chemical consistency score is obtained
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(rank_final = (tidytable::dense_rank(-score_pondered_chemo)), .by = c(feature_id)) |>
      tidytable::filter(rank_final <= candidatesFinal, .by = c(feature_id))

    log_debug("adding initial metadata (RT, etc.) and simplifying columns \n")
    df2 <- featuresTable |>
      tidytable::left_join(df1) |>
      tidytable::left_join(componentsTable) |>
      tidytable::mutate(
        best_candidate_structure = paste(
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          sep = "\u00a7"
        )
      ) |>
      tidytable::distinct(
        feature_id,
        component_id,
        rt,
        mz,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_name,
        structure_01_cla_kingdom = structure_taxonomy_classyfire_01kingdom,
        structure_01_npc_pathway = structure_taxonomy_npclassifier_01pathway,
        structure_02_cla_superclass = structure_taxonomy_classyfire_02superclass,
        structure_02_npc_superclass = structure_taxonomy_npclassifier_02superclass,
        structure_03_cla_class = structure_taxonomy_classyfire_03class,
        structure_03_npc_class = structure_taxonomy_npclassifier_03class,
        structure_04_cla_parent = structure_taxonomy_classyfire_04directparent,
        library,
        error_mz,
        error_rt,
        rank_initial,
        rank_final,
        score_input,
        # score_input_normalized,
        score_biological,
        score_interim = score_pondered_bio,
        score_chemical,
        score_final = score_pondered_chemo,
        count_peaks_explained,
        count_peaks_matched,
        best_candidate_organism = best_candidate,
        best_candidate_structure,
        consensus_structure_cla_kin,
        consistency_structure_cla_kin,
        consensus_structure_npc_pat,
        consistency_structure_npc_pat,
        consensus_structure_cla_sup,
        consistency_structure_cla_sup,
        consensus_structure_npc_sup,
        consistency_structure_npc_sup,
        consensus_structure_cla_cla,
        consistency_structure_cla_cla,
        consensus_structure_npc_cla,
        consistency_structure_npc_cla,
        consensus_structure_cla_par,
        consistency_structure_cla_par
      )

    references <- structureOrganismPairsTable |>
      tidytable::select(
        structure_inchikey_2D,
        reference_doi,
        organism_name,
        tidytable::contains("organism_taxonomy_")
      ) |>
      tidytable::distinct() |>
      tidytable::pivot_longer(tidytable::contains("organism_taxonomy_")) |>
      tidytable::filter(!is.na(value)) |>
      tidytable::filter(value != "notClassified") |>
      tidytable::distinct(structure_inchikey_2D,
        best_candidate_organism = value,
        reference_doi
      )

    log_debug("adding references \n")
    df3 <- df2 |>
      tidytable::left_join(references) |>
      dplyr::group_by(dplyr::across(c(-reference_doi))) |>
      dplyr::reframe(dplyr::across(
        .cols = c(reference_doi),
        .fns = function(x) {
          gsub(
            pattern = "\\bNA\\b",
            replacement = "",
            x = paste(unique(x), collapse = " $ ")
          )
        }
      )) |>
      dplyr::ungroup() |>
      tidytable::select(
        feature_id,
        component_id,
        rt,
        mz,
        structure_molecular_formula,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_name,
        structure_exact_mass,
        structure_xlogp,
        structure_01_cla_kingdom,
        structure_01_npc_pathway,
        structure_02_cla_superclass,
        structure_02_npc_superclass,
        structure_03_cla_class,
        structure_03_npc_class,
        structure_04_cla_parent,
        score_input,
        # score_input_normalized,
        score_biological,
        score_chemical,
        count_peaks_matched,
        count_peaks_explained,
        library,
        error_mz,
        error_rt,
        score_interim,
        score_final,
        rank_initial,
        rank_final,
        best_candidate_organism,
        best_candidate_structure,
        reference_doi,
        tidytable::everything()
      ) |>
      tidytable::arrange(rank_final)

    if (summarize == TRUE) {
      log_debug("summarizing results \n")
      df4 <- df3 |>
        dplyr::group_by(feature_id) |>
        dplyr::reframe(dplyr::across(
          .cols = colnames(df3)[5:32],
          .fns = function(x) {
            gsub(
              pattern = "\\bNA\\b",
              replacement = "",
              x = paste(x, collapse = "|")
            )
          }
        )) |>
        dplyr::ungroup()

      df5 <- df4 |>
        tidytable::left_join(
          df3 |>
            tidytable::select("feature_id", !colnames(df4)) |>
            tidytable::distinct()
        )
    } else {
      df5 <- df3
    }

    log_debug("selecting columns to export \n")
    df6 <- df5 |>
      tidytable::mutate(tidytable::across(tidytable::everything(), as.character)) |>
      tidytable::mutate(tidytable::across(tidytable::everything(), .fns = function(x) {
        tidytable::na_if(x, "")
      })) |>
      tidytable::select(tidytable::any_of(
        c(
          "feature_id",
          "component_id",
          "mz",
          "rt",
          "structure_name",
          "structure_molecular_formula",
          "structure_xlogp",
          "structure_smiles_2D",
          "structure_inchikey_2D",
          "library",
          "error_mz",
          "error_rt",
          "score_input",
          # "score_input_normalized",
          "score_biological",
          "score_chemical",
          "score_final",
          "count_peaks_matched",
          "count_peaks_explained",
          "rank_initial",
          "rank_final",
          "best_candidate_organism",
          "best_candidate_structure",
          "consensus_structure_cla_kin",
          "consistency_structure_cla_kin",
          "consensus_structure_npc_pat",
          "consistency_structure_npc_pat",
          "consensus_structure_cla_sup",
          "consistency_structure_cla_sup",
          "consensus_structure_npc_sup",
          "consistency_structure_npc_sup",
          "consensus_structure_cla_cla",
          "consistency_structure_cla_cla",
          "consensus_structure_npc_cla",
          "consistency_structure_npc_cla",
          "consensus_structure_cla_par",
          "consistency_structure_cla_par",
          "reference_doi"
        )
      ))

    log_debug("adding consensus again to droped candidates \n")
    df8 <- df6 |>
      tidytable::filter(!is.na(structure_inchikey_2D))

    df9 <- df6 |>
      tidytable::filter(is.na(structure_inchikey_2D))

    df10 <- tidytable::left_join(
      df9,
      annotationTableWeightedChemo |>
        tidytable::mutate(tidytable::across(tidytable::everything(), as.character))
    ) |>
      tidytable::select(tidytable::any_of(
        c(
          "feature_id",
          "component_id",
          "mz",
          "rt",
          "consensus_structure_cla_kin",
          "consistency_structure_cla_kin",
          "consensus_structure_npc_pat",
          "consistency_structure_npc_pat",
          "consensus_structure_cla_sup",
          "consistency_structure_cla_sup",
          "consensus_structure_npc_sup",
          "consistency_structure_npc_sup",
          "consensus_structure_cla_cla",
          "consistency_structure_cla_cla",
          "consensus_structure_npc_cla",
          "consistency_structure_npc_cla",
          "consensus_structure_cla_par",
          "consistency_structure_cla_par"
        )
      )) |>
      tidytable::distinct()

    df11 <- tidytable::bind_rows(df8, df10) |>
      tidytable::arrange(as.numeric(feature_id))

    ## Because cytoscape import fails otherwise
    colnames(df11) <-
      stringi::stri_replace_all_fixed(
        str = colnames(df11),
        pattern = "_structure",
        replacement = "",
        vectorize_all = FALSE
      )

    return(df11)
  }
