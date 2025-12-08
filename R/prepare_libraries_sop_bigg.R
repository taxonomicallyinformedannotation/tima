#' @title Prepare libraries of structure organism pairs BiGG
#'
#' @description This function prepares BiGG (Biochemical, Genetic and
#'     Genomic) structure-organism pairs by querying BiGG models and PubChem
#'     for metabolite information, extracting chemical structures, and
#'     formatting for TIMA annotation workflows.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param output Character string path for prepared BiGG library output
#'
#' @return Character string path to prepared BiGG structure-organism pairs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_bigg()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_bigg <- function(
  output = get_params(
    step = "prepare_libraries_sop_bigg"
  )$files$libraries$sop$prepared$bigg
) {
  # Validate inputs
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  log_info("Preparing BiGG structure-organism pairs")

  if (!file.exists(output) || file.size(output) < 100000) {
    log_debug("Processing BiGG from online resources")
    bigg_prepared <- tryCatch(
      expr = {
        # Constants
        BIGG_DOI <- "10.1093/nar/gkv1049"
        default_models <- list(
          "Escherichia coli" = c(
            "model_id" = "iML1515",
            "doi" = "10.1038/nbt.3956"
          ),
          "Saccharomyces cerevisiae" = c(
            "model_id" = "iMM904",
            "doi" = "10.1186/1752-0509-3-37"
          ),
          "Homo sapiens" = c(
            "model_id" = "Recon3D",
            "doi" = "10.1038/nbt.4072"
          )
        )

        n_models <- length(default_models)

        # Helper functions
        get_bigg_model <- function(model_id) {
          BIGG_URL <- "http://bigg.ucsd.edu/static/models/"
          jsonlite::fromJSON(
            txt = paste0(BIGG_URL, model_id["model_id"], ".json"),
            flatten = TRUE
          )
        }

        get_metabolites <- function(model) {
          model$metabolites |>
            tidytable::select(
              "name",
              "inchikey" = "annotation.inchi_key"
            ) |>
            tidytable::mutate(
              tidytable::across(tidyselect::everything(), as.character)
            ) |>
            tidytable::filter(inchikey != "NULL") |>
            tidytable::distinct()
        }

        query_pubchem_batch <- function(batch) {
          ik_string <- paste(batch, collapse = ",")

          url <- paste0(
            "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/inchikey/",
            ik_string,
            "/property/inchikey,connectivitysmiles,smiles,",
            "molecularformula,title/json"
          )

          resp <- httr2::request(url) |>
            httr2::req_timeout(60) |>
            httr2::req_error(is_error = function(x) FALSE) |>
            httr2::req_perform()

          txt <- httr2::resp_body_string(resp)

          # if PubChem returns "Not Found", skip
          if (grepl("NotFound", txt)) {
            return(data.frame(InChIKey = batch, SMILES = NA_character_))
          }

          json <- jsonlite::fromJSON(txt)
          json$PropertyTable$Properties
        }

        inchikey_to_smiles <- function(inchikeys, batch_size = 130L) {
          batches <- split(
            inchikeys,
            ceiling(seq_along(inchikeys) / batch_size)
          )
          purrr::map_dfr(.x = batches, .f = query_pubchem_batch)
        }

        # Main processing
        models <- default_models |>
          purrr::map(.f = get_bigg_model)

        metabolites <- models |>
          purrr::map(.f = get_metabolites) |>
          tidytable::bind_rows(.id = "organism_name") |>
          tidytable::arrange(name) |>
          tidytable::distinct(organism_name, inchikey, .keep_all = TRUE)

        inchikeys <- metabolites$inchikey |>
          unique()

        map_1 <- metabolites |>
          tidytable::distinct(organism_name, inchikey)

        df <- inchikey_to_smiles(inchikeys)

        map_2 <- df |>
          process_smiles("SMILES") |>
          tidytable::inner_join(
            df |>
              tidytable::distinct(InChIKey, SMILES)
          ) |>
          tidytable::distinct(InChIKey, structure_inchikey)

        inchikeys_2 <- map_2 |>
          tidytable::pull(structure_inchikey) |>
          unique()

        df_2 <- inchikey_to_smiles(inchikeys_2)

        metabolites_cleaned <- map_1 |>
          tidytable::inner_join(df, by = c("inchikey" = "InChIKey")) |>
          tidytable::inner_join(map_2, by = c("inchikey" = "InChIKey")) |>
          tidytable::select(organism_name, structure_inchikey) |>
          tidytable::distinct() |>
          tidytable::inner_join(
            df_2,
            by = c("structure_inchikey" = "InChIKey")
          ) |>
          tidytable::mutate(length = stringi::stri_length(Title)) |>
          tidytable::arrange(length) |>
          tidytable::distinct(organism_name, structure_inchikey,
            .keep_all = TRUE
          ) |>
          tidytable::select(-length)

        metabolites_processed <- metabolites_cleaned |>
          process_smiles("SMILES")

        # Organism taxonomy tables
        e_coli <- tidytable::tidytable() |>
          tidytable::mutate(
            organism_name = "Escherichia coli",
            organism_taxonomy_ottid = 474506,
            organism_taxonomy_01domain = "Bacteria",
            organism_taxonomy_02kingdom = NA_character_,
            organism_taxonomy_03phylum = "Proteobacteria",
            organism_taxonomy_04class = "Gammaproteobacteria",
            organism_taxonomy_05order = "Enterobacteriales",
            organism_taxonomy_06family = "Enterobacteriaceae",
            organism_taxonomy_07tribe = NA_character_,
            organism_taxonomy_08genus = NA_character_,
            organism_taxonomy_09species = "Escherichia coli",
            organism_taxonomy_10varietas = NA_character_
          ) |>
          tidytable::mutate(
            reference_doi = default_models$`Escherichia coli`["doi"]
          )

        h_sapiens <- tidytable::tidytable() |>
          tidytable::mutate(
            organism_name = "Homo sapiens",
            organism_taxonomy_ottid = 770315,
            organism_taxonomy_01domain = "Eukaryota",
            organism_taxonomy_02kingdom = "Metazoa",
            organism_taxonomy_03phylum = "Chordata",
            organism_taxonomy_04class = "Mammalia",
            organism_taxonomy_05order = "Primates",
            organism_taxonomy_06family = "Hominidae",
            organism_taxonomy_07tribe = NA_character_,
            organism_taxonomy_08genus = "Homo",
            organism_taxonomy_09species = "Homo sapiens",
            organism_taxonomy_10varietas = NA_character_
          ) |>
          tidytable::mutate(
            reference_doi = default_models$`Homo sapiens`["doi"]
          )

        s_cerevisiae <- tidytable::tidytable() |>
          tidytable::mutate(
            organism_name = "Saccharomyces cerevisiae",
            organism_taxonomy_ottid = 356221,
            organism_taxonomy_01domain = "Eukaryota",
            organism_taxonomy_02kingdom = "Fungi",
            organism_taxonomy_03phylum = "Ascomycota",
            organism_taxonomy_04class = "Saccharomycetes",
            organism_taxonomy_05order = NA_character_,
            organism_taxonomy_06family = "Saccharomycetaceae",
            organism_taxonomy_07tribe = NA_character_,
            organism_taxonomy_08genus = "Saccharomyces",
            organism_taxonomy_09species = "Saccharomyces cerevisiae",
            organism_taxonomy_10varietas = NA_character_
          ) |>
          tidytable::mutate(
            reference_doi = default_models$`Saccharomyces cerevisiae`["doi"]
          )

        biota <- tidytable::tidytable() |>
          tidytable::mutate(
            organism_name = "Biota",
            organism_taxonomy_ottid = 0,
            organism_taxonomy_01domain = "Biota",
            organism_taxonomy_02kingdom = NA_character_,
            organism_taxonomy_03phylum = NA_character_,
            organism_taxonomy_04class = NA_character_,
            organism_taxonomy_05order = NA_character_,
            organism_taxonomy_06family = NA_character_,
            organism_taxonomy_07tribe = NA_character_,
            organism_taxonomy_08genus = NA_character_,
            organism_taxonomy_09species = NA_character_,
            organism_taxonomy_10varietas = NA_character_
          ) |>
          tidytable::mutate(reference_doi = BIGG_DOI)

        metabolites_prefinal <- metabolites_processed |>
          tidytable::inner_join(metabolites_cleaned) |>
          tidytable::select(-SMILES) |>
          tidytable::distinct() |>
          tidytable::rename(
            structure_name = Title,
            structure_smiles_2D = structure_smiles_no_stereo
          ) |>
          tidytable::mutate(
            structure_inchikey_2D = stringi::stri_sub(
              str = structure_inchikey,
              from = 1,
              to = 14
            )
          ) |>
          tidytable::mutate(
            structure_taxonomy_npclassifier_01pathway = NA_character_,
            structure_taxonomy_npclassifier_02superclass = NA_character_,
            structure_taxonomy_npclassifier_03class = NA_character_,
            structure_taxonomy_classyfire_chemontid = NA_character_,
            structure_taxonomy_classyfire_01kingdom = NA_character_,
            structure_taxonomy_classyfire_02superclass = NA_character_,
            structure_taxonomy_classyfire_03class = NA_character_,
            structure_taxonomy_classyfire_04directparent = NA_character_
          )

        metabolites_ecoli <- metabolites_prefinal |>
          tidytable::inner_join(e_coli) |>
          select_sop_columns() |>
          round_reals() |>
          tidytable::distinct()

        metabolites_hsapiens <- metabolites_prefinal |>
          tidytable::inner_join(h_sapiens) |>
          select_sop_columns() |>
          round_reals() |>
          tidytable::distinct()

        metabolites_scerevisiae <- metabolites_prefinal |>
          tidytable::inner_join(s_cerevisiae) |>
          select_sop_columns() |>
          round_reals() |>
          tidytable::distinct()

        metabolites_biota <- metabolites_prefinal |>
          tidytable::group_by(structure_inchikey) |>
          tidytable::add_count() |>
          tidytable::filter(n == n_models) |>
          tidytable::select(-n) |>
          tidytable::mutate(organism_name = "Biota") |>
          tidytable::distinct() |>
          tidytable::inner_join(biota) |>
          select_sop_columns() |>
          round_reals() |>
          tidytable::distinct()

        metabolites_biota |>
          tidytable::bind_rows(
            metabolites_ecoli,
            metabolites_hsapiens,
            metabolites_scerevisiae
          ) |>
          tidytable::arrange(structure_name)
      },
      error = function(e) {
        log_error(
          "Something went wrong with BiGG processing: %s",
          conditionMessage(e)
        )
        fake_sop_columns()
      }
    )

    export_output(x = bigg_prepared, file = output)
  } else {
    # log_trace("BiGG library already exists and is valid")
  }

  return(output)
}

