adducts_forbidden <- c(
  "[M-H2O+H2O-H]-",
  "[M-H3O4P+H3O4P-H]-",
  "[M-H3N+C2H7N-H]-",
  "[M-H3N+C2H3N-H]-",
  "[M-H3N+H4N]+",
  "[M-H2O+H2O+H]+",
  "[M-H3O4P+H3O4P+H]+",
  "[M-H3N+C2H7N+H]+",
  "[M-H3N+C2H3N+H]+"
)

adducts_translations <-
  c(
    "-2H" = "-H2",
    # cliqueMS
    "-3H" = "-H3",
    # cliqueMS
    "-2H2O" = "-H4O2",
    # mzmine
    "-3H2O" = "-H6O3",
    # mzmine
    "-4H2O" = "-H8O4",
    # mzmine
    "-5H2O" = "-H10O5",
    # mzmine
    "[M+H-H2O]+" = "[M-H2O+H]+",
    # mzmine (reorder: loss before addition)
    "[M+H-2H2O]+" = "[M-H4O2+H]+",
    # mzmine
    "[M+H-3H2O]+" = "[M-H6O3+H]+",
    # mzmine
    "[M+Na-H2O]+" = "[M-H2O+Na]+",
    # mzmine
    "[M+K-H2O]+" = "[M-H2O+K]+",
    # mzmine
    "[M-H-H2O]-" = "[M-H2O-H]-",
    # mzmine
    "[M+NH4-H2O]+" = "[M-H2O+H4N]+",
    # mzmine
    "[M+NH4]+" = "[M+H4N]+",
    # mzmine
    "[M+2NH4]2+" = "[M+2H4N]2+",
    # mzmine
    "-NH3" = "+H3N",
    # mzmine
    "+2H" = "+H2",
    # mzmine
    "+2K" = "+K2",
    # cliqueMS
    "+2Na" = "+Na2",
    # mzmine
    "+3K" = "+K3",
    # cliqueMS
    "+3Na" = "+Na3",
    # cliqueMS
    "+Acetate" = "+C2H3O2",
    # mzmine
    "+ACN" = "+C2H3N",
    # mzmine
    "+CH3COO" = "+C2H3O2",
    # GNPS
    "+FA" = "+CHO2",
    # mzmine
    "+HAc" = "+C2H4O2",
    # mzmine
    "+Hac" = "+C2H4O2",
    # GNPS
    "+HFA" = "+CH2O2",
    # mzmine
    "+IsoProp" = "+C3H8O",
    # mzmine
    "+MeOH" = "+CH4O",
    # mzmine
    "+NH4" = "+H4N",
    # mzmine
    "+TFA" = "+C2HF3O2",
    # MassBank
    "[M+CH3COO]-/[M-CH3]-" = "[M+CH3COO]-",
    # additional
    "[M+2H]+2" = "[M+2H]2+"
  )
