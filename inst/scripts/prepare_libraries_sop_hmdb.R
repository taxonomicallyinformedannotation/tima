start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("prepares hmdb")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

#' @title Prepare HMDB
#'
#' @param input Input file
#' @param output Output file
#' @param output_minimal Output of the minimal file
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_sop_hmdb <- function(
    input = paths$data$source$libraries$sop$hmdb,
    output = paths$data$interim$libraries$sop$hmdb,
    output_minimal = paths$data$interim$libraries$sop$hmdb_minimal) {
  log_debug("Unzipping HMDB (6.5GB)")
  unzip(
    zipfile = input,
    exdir = dirname(input)
  )
  hmdb_structures <-
    gsub(
      pattern = ".zip",
      replacement = ".xml",
      x = input,
      fixed = TRUE
    )

  log_debug("Loading HMDB (takes waaaaayyyy tooooo long)")
  hmdb_xml <- XML::xmlParse(file = hmdb_structures)

  extract_value_from_hmdb_xml <-
    function(value) {
      extracted <- XML::xpathSApply(
        doc = hmdb_xml,
        path = paste0("//*/ns:metabolite/ns:", value),
        ## Because of insecure link detected otherwise
        namespaces = c(ns = "https://www.hmdb.ca" |>
          gsub(
            pattern = "https",
            replacement = "http",
            fixed = TRUE
          )),
        fun = XML::xmlValue
      ) |>
        list()

      names(extracted) <- value

      return(extracted)
    }

  values <- list(
    "accession",
    "name",
    "chemical_formula",
    "monisotopic_molecular_weight",
    "iupac_name",
    "smiles",
    "inchi",
    "inchikey"
  )

  log_debug("Extracting values from HMDB (takes waaaaayyyy tooooo long)")
  hmdb_df <-
    lapply(X = values, FUN = extract_value_from_hmdb_xml) |>
    data.frame() |>
    tidytable::distinct()

  log_debug(x = "Formatting HMDB")
  hmdb_prepared <- hmdb_df |>
    tidytable::mutate_all(tidytable::na_if, "") |>
    tidytable::filter(!is.na(inchikey)) |>
    tidytable::mutate(
      structure_inchikey_2D = stringi::stri_sub(
        str = inchikey,
        from = 1,
        to = 14
      ),
      ## TODO compute it
      structure_smiles_2D = NA_character_,
      monisotopic_molecular_weight = as.numeric(monisotopic_molecular_weight)
    ) |>
    tidytable::select(
      structure_name = name,
      structure_inchikey = inchikey,
      structure_smiles = smiles,
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_molecular_formula = chemical_formula,
      structure_exact_mass = monisotopic_molecular_weight
    ) |>
    tidytable::mutate(
      structure_xlogp = NA_integer_,
      structure_taxonomy_npclassifier_01pathway = NA_character_,
      structure_taxonomy_npclassifier_02superclass = NA_character_,
      structure_taxonomy_npclassifier_03class = NA_character_,
      structure_taxonomy_classyfire_chemontid = NA_character_,
      structure_taxonomy_classyfire_01kingdom = NA_character_,
      structure_taxonomy_classyfire_02superclass = NA_character_,
      structure_taxonomy_classyfire_03class = NA_character_,
      structure_taxonomy_classyfire_04directparent = NA_character_,
    ) |>
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
      organism_taxonomy_10varietas = NA_character_,
      reference_doi = NA_character_
    ) |>
    round_reals() |>
    tidytable::distinct()

  log_debug(x = "Exporting ...")
  export_output(x = hmdb_df, file = output_minimal)
  export_output(x = hmdb_prepared, file = output)

  log_debug("Deleting unzipped HMDB")
  file.remove(input)
}

prepare_libraries_sop_hmdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
