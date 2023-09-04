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
        smiles_no_stereo = NA_character_,
        inchi_no_stereo = NA_character_,
        inchikey_no_stereo = NA_character_
      )
  } else {
    mol <- mol |>
      rcdk::remove.hydrogens()
    smiles_no_stereo <- mol |>
      rcdk::get.smiles(
        flavor = rcdk::smiles.flavors(
          c(
            "Canonical",
            "UseAromaticSymbols"
          )
        )
      )
    inchi_no_stereo <- mol |>
      rinchi::get.inchi()
    inchikey_no_stereo <- mol |>
      rinchi::get.inchi.key() |>
      gsub(pattern = "-.*", replacement = "")

    return(list(
      smiles_no_stereo = ifelse(
        test = smiles_no_stereo == "",
        yes = NA_character_,
        no = smiles_no_stereo
      ),
      inchi_no_stereo = ifelse(
        test = is.null(inchi_no_stereo),
        yes = NA_character_,
        no = inchi_no_stereo
      ),
      inchikey_no_stereo = ifelse(
        test = identical(x = inchikey_no_stereo, y = character(0)),
        yes = NA_character_,
        no = inchikey_no_stereo
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
      smiles_no_stereo = NA_character_,
      inchi_no_stereo = NA_character_,
      inchikey_no_stereo = NA_character_
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
      smiles_no_stereo = NA_character_,
      inchi_no_stereo = NA_character_,
      inchikey_no_stereo = NA_character_
    )
  }

  return(data.frame(inchi, list))
}
