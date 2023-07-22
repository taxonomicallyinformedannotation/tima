## not putting this into R/ as more elaborated dependencies

## replacing the original rinchi::parse.inchi
parse.inchi <- function(inchis) {
  ## Not working?
  ## OKAY <- rJava::.jcall(
  ## "net/sf/jniinchi/INCHI_RET",
  ## "Lnet/sf/jniinchi/INCHI_RET;",
  ## "getValue",
  ## as.integer(0))
  dcob <-
    rJava::.jcall(
      "org/openscience/cdk/DefaultChemObjectBuilder",
      "Lorg/openscience/cdk/interfaces/IChemObjectBuilder;",
      "getInstance"
    )
  igf <-
    rJava::.jcall(
      "org/openscience/cdk/inchi/InChIGeneratorFactory",
      "Lorg/openscience/cdk/inchi/InChIGeneratorFactory;",
      "getInstance"
    )
  mols <- lapply(inchis, function(inchi) {
    i2s <-
      rJava::.jcall(
        igf,
        "Lorg/openscience/cdk/inchi/InChIToStructure;",
        "getInChIToStructure",
        inchi,
        dcob
      )
    status <- i2s$getReturnStatus()

    # if (status == OKAY)
    if ("OKAY" == "OKAY") {
      rJava::.jcast(
        i2s$getAtomContainer(),
        "org/openscience/cdk/interfaces/IAtomContainer"
      )
    } else {
      warning(paste0(
        "InChI parsing error for ",
        inchi,
        ": ",
        status$toString()
      ))
      return(NULL)
    }
  })
  return(mols)
}

standardize_mol <- function(mol) {
  if (mol$isEmpty()) {
    list <-
      list(
        smiles_2D = NA_character_,
        inchi_2D = NA_character_,
        inchikey_2D = NA_character_
      )
  } else {
    mol <- mol |>
      rcdk::remove.hydrogens()
    smiles_2D <- mol |>
      rcdk::get.smiles(
        flavor = rcdk::smiles.flavors(
          c(
            "Canonical",
            "UseAromaticSymbols"
          )
        )
      )
    inchi_2D <- mol |>
      rinchi::get.inchi()
    inchikey_2D <- mol |>
      rinchi::get.inchi.key() |>
      gsub(pattern = "-.*", replacement = "")

    return(list(
      smiles_2D = ifelse(
        test = smiles_2D == "",
        yes = NA_character_,
        no = smiles_2D
      ),
      inchi_2D = ifelse(
        test = is.null(inchi_2D),
        yes = NA_character_,
        no = inchi_2D
      ),
      inchikey_2D = ifelse(
        test = identical(x = inchikey_2D, y = character(0)),
        yes = NA_character_,
        no = inchikey_2D
      )
    ))
  }
}

standardize_smiles <- function(smiles) {
  list <- rcdk::parse.smiles(
    smiles = smiles,
    kekulise = TRUE
  )[[1]]
  if (length(list) > 0) {
    list <- list |>
      standardize_mol()
  } else {
    list <- list(
      smiles_2D = NA_character_,
      inchi_2D = NA_character_,
      inchikey_2D = NA_character_
    )
  }

  return(data.frame(smiles, list))
}

standardize_inchi <- function(inchi) {
  list <- parse.inchi(inchi)[[1]]
  if (length(list) > 0) {
    list <- list |>
      standardize_mol()
  } else {
    list <- list(
      smiles_2D = NA_character_,
      inchi_2D = NA_character_,
      inchikey_2D = NA_character_
    )
  }

  return(data.frame(inchi, list))
}
