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
#' @importFrom dplyr distinct filter mutate mutate_all na_if select
#' @importFrom readr read_tsv write_tsv
#' @importFrom XML xmlParse xmlValue xpathSApply
#'
#' @examples NULL
prepare_hmdb <- function(input = paths$data$source$libraries$hmdb,
                         output = paths$data$interim$libraries$hmdb,
                         output_minimal = paths$data$interim$libraries$hmdb_minimal) {
  log_debug("Unzipping HMDB (6.5GB)")
  unzip(
    zipfile = input,
    exdir = dirname(input)
  )
  hmdb_structures <-
    gsub(
      pattern = ".zip",
      replacement = ".xml",
      x = input
    )

  log_debug("Loading HMDB (takes waaaaayyyy tooooo long)")
  hmdb_xml <- XML::xmlParse(file = hmdb_structures)

  extract_value_from_hmdb_xml <-
    function(value) {
      extracted <- XML::xpathSApply(
        doc = hmdb_xml,
        path = paste0("//*/ns:metabolite/ns:", value),
        namespaces = c(ns = "http://www.hmdb.ca"),
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
    dplyr::distinct()

  log_debug(x = "Formatting HMDB")
  hmdb_prepared <- hmdb_df |>
    dplyr::mutate_all(dplyr::na_if, "") |>
    dplyr::filter(!is.na(inchikey)) |>
    dplyr::mutate(structure_inchikey_2D = substring(
      text = inchikey,
      first = 1,
      last = 14
    )) |>
    dplyr::select(
      structure_nameTraditional = name,
      structure_inchikey_2D,
      structure_smiles_2D = smiles,
      ## COMMENT (AR): To improve!
      structure_molecular_formula = chemical_formula,
      structure_exact_mass = monisotopic_molecular_weight
    ) |>
    dplyr::mutate(
      ## COMMENT (AR): To improve!
      structure_xlogp = NA_integer_,
      structure_taxonomy_npclassifier_01pathway = NA_character_,
      structure_taxonomy_npclassifier_02superclass = NA_character_,
      structure_taxonomy_npclassifier_03class = NA_character_
    ) |>
    dplyr::mutate(
      organism_name = "Homo sapiens",
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
    # Round to 5 digits to avoid small discrepancies
    dplyr::mutate(
      structure_exact_mass = round(structure_exact_mass, digits = 5),
      structure_xlogp = round(structure_xlogp, digits = 5)
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(reference_doi = NA)

  log_debug(x = "Exporting ...")
  export_output(x = hmdb_df, file = output_minimal)
  export_output(x = hmdb_prepared, file = output)

  log_debug("Deleting unzipped HMDB")
  file.remove(input)
}

prepare_hmdb()

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
