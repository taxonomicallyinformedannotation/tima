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
#' @param bigg_doi Character string DOI for BiGG database reference
#' @param bigg_models Named list of BiGG models with organism names as keys
#'     and named character vectors containing "model_id" and "doi" as values
#' @param bigg_url Character string base URL for BiGG models API
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
  bigg_doi = "10.1093/nar/gkv1049",
  bigg_models = list(
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
  ),
  bigg_url = "http://bigg.ucsd.edu/static/models/",
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
        n_models <- length(bigg_models)

        # Helper functions
        get_bigg_model <- function(model_id) {
          jsonlite::fromJSON(
            txt = paste0(bigg_url, model_id["model_id"], ".json"),
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
        models <- bigg_models |>
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
          tidytable::distinct(
            organism_name,
            structure_inchikey,
            .keep_all = TRUE
          ) |>
          tidytable::select(-length)

        metabolites_processed <- metabolites_cleaned |>
          process_smiles("SMILES")

        # Organism taxonomy tables - created conditionally based on bigg_models
        taxonomy_templates <- list(
          "Escherichia coli" = list(
            ottid = 474506L,
            domain = "Bacteria",
            kingdom = NA_character_,
            phylum = "Proteobacteria",
            class = "Gammaproteobacteria",
            order = "Enterobacteriales",
            family = "Enterobacteriaceae",
            tribe = NA_character_,
            genus = NA_character_,
            species = "Escherichia coli",
            varietas = NA_character_
          ),
          "Homo sapiens" = list(
            ottid = 770315L,
            domain = "Eukaryota",
            kingdom = "Metazoa",
            phylum = "Chordata",
            class = "Mammalia",
            order = "Primates",
            family = "Hominidae",
            tribe = NA_character_,
            genus = "Homo",
            species = "Homo sapiens",
            varietas = NA_character_
          ),
          "Saccharomyces cerevisiae" = list(
            ottid = 356221L,
            domain = "Eukaryota",
            kingdom = "Fungi",
            phylum = "Ascomycota",
            class = "Saccharomycetes",
            order = NA_character_,
            family = "Saccharomycetaceae",
            tribe = NA_character_,
            genus = "Saccharomyces",
            species = "Saccharomyces cerevisiae",
            varietas = NA_character_
          )
        )

        # Helper to create organism table
        create_organism_table <- function(organism_name, taxonomy_template, doi) {
          if (is.null(doi)) {
            return(NULL)
          }
          tidytable::tidytable(
            organism_name = organism_name,
            organism_taxonomy_ottid = taxonomy_template$ottid,
            organism_taxonomy_01domain = taxonomy_template$domain,
            organism_taxonomy_02kingdom = taxonomy_template$kingdom,
            organism_taxonomy_03phylum = taxonomy_template$phylum,
            organism_taxonomy_04class = taxonomy_template$class,
            organism_taxonomy_05order = taxonomy_template$order,
            organism_taxonomy_06family = taxonomy_template$family,
            organism_taxonomy_07tribe = taxonomy_template$tribe,
            organism_taxonomy_08genus = taxonomy_template$genus,
            organism_taxonomy_09species = taxonomy_template$species,
            organism_taxonomy_10varietas = taxonomy_template$varietas,
            reference_doi = doi
          )
        }

        # Create tables only for organisms in bigg_models
        organism_tables <- list()
        for (org_name in names(bigg_models)) {
          doi <- bigg_models[[org_name]]["doi"]
          taxonomy <- taxonomy_templates[[org_name]]
          if (!is.null(taxonomy)) {
            organism_tables[[org_name]] <- create_organism_table(
              org_name,
              taxonomy,
              doi
            )
          }
        }

        # Create Biota table (universal)
        biota <- tidytable::tidytable(
          organism_name = "Biota",
          organism_taxonomy_ottid = 0L,
          organism_taxonomy_01domain = "Biota",
          organism_taxonomy_02kingdom = NA_character_,
          organism_taxonomy_03phylum = NA_character_,
          organism_taxonomy_04class = NA_character_,
          organism_taxonomy_05order = NA_character_,
          organism_taxonomy_06family = NA_character_,
          organism_taxonomy_07tribe = NA_character_,
          organism_taxonomy_08genus = NA_character_,
          organism_taxonomy_09species = NA_character_,
          organism_taxonomy_10varietas = NA_character_,
          reference_doi = bigg_doi
        )

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

        # Process metabolites for each organism
        metabolites_by_organism <- purrr::map_dfr(
          .x = organism_tables,
          .f = function(org_table) {
            metabolites_prefinal |>
              tidytable::inner_join(org_table) |>
              select_sop_columns() |>
              round_reals() |>
              tidytable::distinct()
          }
        )

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

        # Combine organism tables with Biota
        metabolites_by_organism |>
          tidytable::bind_rows(metabolites_biota) |>
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
