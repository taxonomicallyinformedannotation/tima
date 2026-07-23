#' @title Label mandatory
#'
#' @description This function adds an asterisk to mandatory inputs
#'
#' @param label Label
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
.label_mandatory <- function(label) {
  shiny::tagList(label, shiny::span("*", class = "mandatory_star"))
}

app_css <-
  ".mandatory_star { color: red; }
     .shiny-input-container { margin-top: 25px; }
     #submit_msg { margin-left: 15px; }
     #error { color: red; }
     body { background: #fcfcfc; }
     #header { margin: -20px -15px 0; padding: 15px 15px 10px; }
    "

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(rules = app_css),
  title = "Taxonomically Informed Metabolite Annotation",
  shiny::div(
    id = "header",
    shiny::h1("Taxonomically Informed Metabolite Annotation"),
    shiny::h4(
      "This app helps performing TIMA as described in the",
      shiny::a(
        href = "https://taxonomicallyinformedannotation.github.io/tima",
        "following documentation"
      )
    ),
    shiny::strong(
      shiny::span("Created by "),
      shiny::a("Adriano Rutz", href = "https://adafede.github.io/"),
      shiny::HTML("&bull;"),
      shiny::a(
        "Main publication",
        href = "https://doi.org/10.3389/fpls.2019.01329"
      ),
      shiny::HTML("&bull;"),
      shiny::span("Code"),
      shiny::a(
        "on GitHub",
        href = "https://github.com/taxonomicallyinformedannotation/tima"
      )
    )
  ),
  shiny::fluidPage(
    shiny::div(
      id = "params",
      shiny::navlistPanel(
        widths = c(4, 5),
        "Parameters",
        shiny::tabPanel(
          title = "Files",
          shiny::h3("Required files"),
          shiny::div(
            shiny::fileInput(
              inputId = "fil_spe_raw",
              label = "MGF file (optional if mzTab is provided)",
              accept = ".mgf"
            ),
            shiny::downloadButton(
              outputId = "demo_spe",
              "Download example spectra"
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "The MGF file containing your spectra.",
                "Should be located in `data/source`.",
                "Reason therefore is to find it in the future."
                # "If you have a GNPS job ID, the spectra will be stored there."
              )
            ),
          shiny::div(
            shiny::fileInput(
              inputId = "fil_fea_raw",
              label = "Features table (optional if mzTab is provided)",
              accept = c(
                ".csv",
                ".tsv",
                ".csv.gz",
                ".tsv.gz",
                ".csv.zip",
                ".tsv.zip"
              )
            ),
            shiny::downloadButton(
              outputId = "demo_fea",
              "Download example features"
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "The csv or tsv file containing your features",
                "Should be located in `data/source`.",
                "Reason therefore is to find it in the future."
                # "If you have a GNPS job ID, the spectra will be stored there."
              )
            ),
          shiny::div(
            shiny::fileInput(
              inputId = "fil_met_raw",
              label = "Metadata table (mandatory if no taxon name)",
              accept = c(
                ".csv",
                ".tsv",
                ".csv.gz",
                ".tsv.gz",
                ".csv.zip",
                ".tsv.zip"
              )
            ),
            shiny::downloadButton(
              outputId = "demo_met",
              "Download example metadata"
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "The csv or tsv file containing the metadata of your experiment.",
                "Should be located in `data/source`.",
                "Reason therefore is to find it in the future.",
                # "If you have a GNPS job ID, the spectra will be stored there.",
                "Do not forget to change the `name` in the corresponding tab."
              )
            ),
          shiny::fileInput(
            inputId = "fil_ann_raw_mzm",
            label = "mzmine annotations (optional)",
            accept = ".csv"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Optional mzmine annotation file",
                "Should be located in `data/interim`.",
                "Reason therefore is to find it in the future."
              )
            ),
          shiny::fileInput(
            inputId = "fil_ann_raw_sir",
            label = "SIRIUS project space (optional)",
            accept = ".zip"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "The compressed directory containing the SIRIUS project space.",
                "Should be located in `data/interim`.",
                "Reason therefore is to find it in the future."
              )
            ),
          shiny::fileInput(
            inputId = "fil_mzt_raw",
            label = "mzTab-M file (optional - replaces features, spectra, and metadata)",
            accept = c(".mztab", ".json")
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "An mzTab-M (v2.0.0) file with quantification and/or annotation data.",
                "Accepted formats: plain-text .mztab and JSON (.json for rmzTabM,",
                "Progenesis QI, and MetaboScape exports).",
                "When provided, the feature table, spectra, and metadata are",
                "automatically derived from this file.",
                "Additional files uploaded above override the corresponding",
                "mzTab-M-derived output (e.g. a custom MGF overrides proxy spectra).",
                "Should be placed in `data/source` for reproducibility."
              )
            ),
          shiny::selectInput(
            inputId = "too_sir_ver",
            label = "SIRIUS version",
            choices = c(5, 6),
            selected = 6
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "The SIRIUS version used.",
                "We highly recommend 6, the default."
              )
            ),
          shiny::numericInput(
            inputId = "too_sir_max_analog_abs_mz_error",
            label = "Max abs m/z error for SIRIUS analog hits (Da)",
            value = 0.01,
            min = 0,
            step = 0.001
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Filters SIRIUS spectral analog candidates by absolute precursor m/z deviation.",
                "Only analogs with |mz_error| <= this threshold are kept.",
                "Set a larger value to keep more analog candidates."
              )
            ),
          shiny::textInput(
            inputId = "fil_pat",
            label = .label_mandatory("Pattern to identify your job locally"),
            value = "example"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of your job.",
                "All intermediate steps will use this pattern.",
                "For example, the corresponding SIRIUS results will be named:",
                "`yourPattern_sirius`."
              )
            )
        ),
        shiny::tabPanel(
          title = "Annotations",
          shiny::h3("Annotations-related parameters"),
          shiny::sliderInput(
            inputId = "ann_can_bes",
            label = "Best percentile threshold for candidates",
            min = 0.0,
            max = 1.0,
            value = 0.0,
            step = 0.05,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Percentile threshold for selecting top candidates within each feature.",
                "Keeps candidates with scores >= percentile * max_score.",
                "0.9 keeps candidates with scores >= 90% of the maximum score (top 10%).",
                "This ensures mini and filtered outputs have the same row counts.",
                "Lower values (e.g., 0.8) = more candidates, higher values (e.g., 0.95) = fewer candidates."
              )
            ),
          shiny::sliderInput(
            inputId = "ann_can_fin",
            label = "Number of final candidates",
            min = 1L,
            max = 500L,
            value = 1L,
            step = 1L,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Number of final candidates to consider.",
                "For 12 candidates, with 10, only the first 10 will be kept.",
                "This can end to more than n candidates, if some are ex aequo."
              )
            ),
          shiny::sliderInput(
            inputId = "ann_can_nei",
            label = "Number of neighbors to keep per feature",
            min = 1L,
            max = 100L,
            value = 16L,
            step = 1L,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Number of neighbors to keep per feature.",
                "This affects chemical consistency calculation.",
                "In case you have many edges, helps reducing the memory load."
              )
            ),
          shiny::sliderInput(
            inputId = "ann_can_sam",
            label = "Number of samples to keep per feature",
            min = 1L,
            max = 5L,
            value = 1L,
            step = 1L,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Only has impact if you provided a metadata file in the
              `Files` panel.",
                "In this case,
              it will take the intensity matrix of your features",
                "and attribute the n samples where the highest intensity",
                "was observed as source to your features.",
                "Useful when your experiment contains
              different biological sources.",
                "As the samples where the measured intensity
              is the highest are also the ones",
                "with higher likelihood of the
              corresponding compounds being isolated,",
                "the number of samples can be kept low."
              )
            ),
          shiny::checkboxInput(
            inputId = "ann_ms1only",
            label = "Erase MS2 results and keep MS1 only",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Option to erase MS2 results.",
                "Usually not needed, for benchmarking mainly."
              )
            ),
          shiny::sliderInput(
            inputId = "ann_thr_con",
            label = "Minimal consistency score (chemical) to consider a class",
            min = 0,
            max = 1,
            value = 0.0,
            step = 0.05,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimal consistency score (chemical) to consider a class.",
                "Everything below will be considered as not consistent."
              )
            ),
          shiny::sliderInput(
            inputId = "ann_thr_ms1_bio",
            label = "Minimal biological score to keep MS1 only annotation",
            min = 0,
            max = 1,
            value = 0.0,
            step = 0.05,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimal biological score to keep an MS1 candaidate.",
                "Everything below will be discarded (see below).",
                "Check the sub-scores and weights in the `weights` panel."
              )
            ),
          shiny::sliderInput(
            inputId = "ann_thr_ms1_che",
            label = "Minimal chemical score to keep MS1 only annotation",
            min = 0,
            max = 1,
            value = 0.0,
            step = 0.1,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimal chemical score to keep an MS1 candaidate.",
                "Everything below will be discarded (see below).",
                "Check the sub-scores and weights in the `weights` panel."
              )
            ),
          shiny::selectInput(
            inputId = "ann_thr_ms1_con",
            label = "Condition to be used to retain candidates",
            choices = c("AND", "OR"),
            selected = "OR"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "If an MS1 candidate has a bio score of 1, a chem score of 0.25",
                "the above thresholds set at 0.3 and 0.3 respectively",
                "keeps it if the condition is `OR` but discard it if `AND`."
              )
            ),
          shiny::checkboxInput(
            inputId = "ann_ms2_app",
            label = "Perform matching without precursor? (can be very long)",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Corresponds to variable dereplication.",
                "Then, precurors masses are disregarded during matching.",
                "Allows to find (incorrect) analogs.",
                "Takes a lot of time."
              )
            ),
          shiny::selectInput(
            inputId = "sim_met_ann",
            label = "Similarity method (annotation)",
            choices = c("entropy", "gnps"),
            selected = "gnps"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Similarity method to be used.",
                "Entropy is faster."
              )
            ),
          shiny::sliderInput(
            inputId = "sim_thr_ann",
            label = "Minimal similarity score (annotation)",
            min = 0,
            max = 1,
            step = 0.05,
            value = 0.0,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimum similarity score between spectra.",
                "To keep MS2 annotation."
              )
            ),
          shiny::selectInput(
            inputId = "sim_met_edg",
            label = "Similarity method (edges)",
            choices = c("entropy", "gnps"),
            selected = "gnps"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Similarity method to be used.",
                "Entropy is faster."
              )
            ),
          shiny::sliderInput(
            inputId = "sim_thr_edg",
            label = "Minimal similarity score (edges)",
            min = 0.5,
            max = 1,
            step = 0.05,
            value = 0.7,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimum similarity score between spectra.",
                "To keep MS2 similarity edge."
              )
            ),
          shiny::sliderInput(
            inputId = "sim_thr_mat",
            label = "Minimal matched peaks (edges)",
            min = 0,
            max = 100,
            step = 1,
            value = 6,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimum number of matched peaks between spectra.",
                "To keep MS2 similarity edge."
              )
            )
        ),
        shiny::tabPanel(
          title = "MS",
          shiny::h3("MS-related parameters"),
          shiny::selectInput(
            inputId = "ms_pol",
            label = "Polarity used",
            choices = c("pos", "neg"),
            selected = "pos"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Polarity of the MS experiment."
            ),
          shiny::sliderInput(
            inputId = "ms_thr_ms2_int",
            label = "Intensity threshold for MS2",
            min = 0,
            max = 1E4,
            value = 0,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "MS2 intensity threshold.",
                "Fragments below this threshold will be removed from spectra."
              )
            ),
          shiny::numericInput(
            inputId = "ms_thr_ms2_min_fragments",
            label = "Minimum number of fragments",
            min = 1,
            max = 10,
            value = 2,
            step = 1
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Minimum number of fragment peaks a spectrum must have",
                "after cleaning to be retained.",
                "Spectra with fewer peaks are discarded."
              )
            ),
          shiny::sliderInput(
            inputId = "ms_tol_mas_ppm_ms1",
            label = "Relative mass tolerance for MS1 in ppm",
            min = 0.1,
            max = 20,
            value = 10,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Mass tolerance (in ppm) used for MS1 annotation."
            ),
          shiny::sliderInput(
            inputId = "ms_tol_mas_dal_ms1",
            label = "Absolute mass tolerance for MS1 in Dalton",
            min = 0.005,
            max = 0.02,
            value = 0.01,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Mass tolerance (in Da) used for MS1 annotation."
            ),
          shiny::sliderInput(
            inputId = "ms_tol_mas_ppm_ms2",
            label = "Relative mass tolerance for MS2 in ppm",
            min = 0.1,
            max = 20,
            value = 10,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Mass tolerance (in ppm) used for MS2 annotation."
            ),
          shiny::sliderInput(
            inputId = "ms_tol_mas_dal_ms2",
            label = "Absolute mass tolerance for MS2 in Dalton",
            min = 0.005,
            max = 0.02,
            value = 0.01,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Mass tolerance (in Da) used for MS2 annotation."
            ),
          shiny::sliderInput(
            inputId = "ms_tol_rt_add",
            label = "Retention time tolerance for adducts annotation in minutes",
            min = 0.005,
            max = 0.1,
            value = 0.05,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Retention time tolerance (minutes) used for adducts annotation."
            ),
          shiny::sliderInput(
            inputId = "ms_tol_rt_lib",
            label = "Retention time tolerance for library annotation in minutes",
            min = 0.005,
            max = 0.2,
            value = 0.1,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Retention time tolerance (minutes) used for library annotation.",
                "If no experimental library is given, does not impact results."
              )
            ),
          shiny::sliderInput(
            inputId = "ms_tol_int_add",
            label = "Minimum intensity co-variance for adduct edges",
            min = 0.0,
            max = 1.0,
            value = 0.7,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Pearson correlation threshold for intensity co-variance validation.",
                "Adduct edges with |correlation| < threshold are rejected.",
                "Higher values (0.8-0.9) require stronger intensity co-variance."
              )
            ),
          shiny::checkboxGroupInput(
            inputId = "ms_add_pos",
            label = "List of adducts to be used in positive",
            choices = c(
              "[M+H3]3+",
              "[M+Fe]3+",
              "[M+H2]2+",
              "[M+H8N2]2+",
              "[M+Na2]2+",
              "[M+K2]2+",
              "[M+Cu2]2+",
              "[M+Mg]2+",
              "[M+Ca]2+",
              "[M+Fe]2+",
              "[M]+",
              "[M+H]+",
              "[M+H4N]+",
              "[M+Na]+",
              "[M+K]+",
              "[M+Cu]+",
              "[2M+Mg]2+",
              "[2M+Ca]2+",
              "[2M+Fe]2+",
              "[2M+H]+",
              "[2M+H4N]+",
              "[2M+Na]+",
              "[2M+K]+",
              "[2M+Cu]+"
            ),
            selected = c(
              "[M+H2]2+",
              "[M+H]+",
              "[M+H4N]+",
              "[M+Na]+",
              "[M+K]+",
              "[2M+H]+"
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Choose wisely.",
                "If a very important adduct everyone should have is missing,",
                "please open an issue at:",
                as.character(
                  shiny::tags$a(
                    "https://github.com/taxonomicallyinformedannotation/tima/issues",
                    href = "https://github.com/taxonomicallyinformedannotation/tima/issues"
                  )
                )
              )
            ),
          shiny::checkboxGroupInput(
            inputId = "ms_add_neg",
            label = "List of adducts to be used in negative",
            choices = c(
              "[M-H3]3-",
              "[M-H2]2-",
              "[M]-",
              "[M-H]-",
              "[M+F]-",
              "[M+Cl]-",
              "[M+Br]-",
              "[2M-H]-",
              "[3M-H]-"
            ),
            selected = c(
              "[M-H2]2-",
              "[M-H]-",
              "[2M-H]-"
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Choose wisely.",
                "If a very important adduct everyone should have is missing,",
                "please open an issue at:",
                as.character(
                  shiny::tags$a(
                    "https://github.com/taxonomicallyinformedannotation/tima/issues",
                    href = "https://github.com/taxonomicallyinformedannotation/tima/issues"
                  )
                )
              )
            ),
          shiny::checkboxGroupInput(
            inputId = "ms_clu",
            label = "List of clusters to be used",
            choices = c(
              "H4ClN", # (ammonium chloride)            ~53.003
              "NaCl", # (sodium chloride)              ~57.959
              "ClK", # (potassium chloride)           ~73.933
              "CH2O2", # (formic acid)                  ~46.006
              "CH5NO2", # (ammonium formate)             ~63.032
              "CHNaO2", # (sodium formate)               ~67.987
              "CHKO2", # (potassium formate)            ~83.961
              "C2H4O2", # (acetic acid)                  ~60.021
              "C2H7NO2", # (ammonium acetate)             ~77.046
              "C2H3NaO2", # (sodium acetate)               ~82.003
              "C2H3KO2", # (potassium acetate)            ~97.977
              "H2PO4", # (phosphoric acid)              ~96.969
              "C2HF3O2" # (trifluoroacetic acid)         ~78.014
            ),
            selected = c(
              "H4ClN", # (ammonium chloride)            ~53.003
              "NaCl", # (sodium chloride)              ~57.959
              "ClK", # (potassium chloride)           ~73.933
              "CH2O2", # (formic acid)                  ~46.006
              "CH5NO2", # (ammonium formate)             ~63.032
              "CHNaO2", # (sodium formate)               ~67.987
              "CHKO2", # (potassium formate)            ~83.961
              "C2HF3O2" # (trifluoroacetic acid)         ~78.014
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Choose wisely.",
                "If a very important cluster everyone should have is missing,",
                "please open an issue at:",
                as.character(
                  shiny::tags$a(
                    "https://github.com/taxonomicallyinformedannotation/tima/issues",
                    href = "https://github.com/taxonomicallyinformedannotation/tima/issues"
                  )
                )
              )
            ),
          shiny::checkboxGroupInput(
            inputId = "ms_sol",
            label = "List of solvents to be used",
            choices = c(
              "H2O", # (water)                        ~18.011
              "CH4O", # (methanol)                     ~32.026
              "C2H3N", # (acetonitrile)                 ~41.053
              "C2H6O", # (ethanol)                      ~46.042
              "C3H8O", # (isopropanol)                  ~60.058
              "C2H6OS" # (dmso)                         ~78.014
            ),
            selected = c(
              "C2H3N" # (acetonitrile)                 ~41.053
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Solvent modifiers are configured separately from cluster modifiers.",
                "Keep this list aligned with the annotate_masses YAML defaults."
              )
            ),
          shiny::checkboxGroupInput(
            inputId = "ms_neu",
            label = "List of neutral losses to be used",
            choices = c(
              "H2 (dihydrogen)", # ~2.016
              "CH2 (methylene)", # ~14.016
              "O", # ~15.995
              "H3N (ammonia)", # ~17.027
              "H2O (water)", # ~18.011
              "HF (hydrogen fluoride)", # ~20.006
              "CO", # ~27.995
              "C2H4 (ethene)", # ~28.031
              "C2H5 (ethyl radical)", # ~29.039
              "NO (nitric oxide)", # ~29.998
              "CH2O (formaldehyde)", # ~30.011
              "H5ON (H2O-H3N)", # ~35.037
              "H4O2 (2xH2O)", # ~36.021
              "C2H2O (ethenone)", # ~42.011
              "CO2", # ~43.990
              "C2H4O (acetaldehyde)", # ~44.026
              "CHO2", # ~44.998
              "H6O3 (3xH2O)", # ~54.032
              "C2O2 (2xCO)", # ~55.990
              "C3H4O (acrolein)", # ~56.026
              "SO2 (sulfur dioxide)", # ~63.962
              "CF2O (carbonyl difluoride)", # ~65.992
              "CH6O3 (combination)", # ~66.032
              "C4H4O (furan)", # ~68.026
              "H8O4 (4xH2O)", # ~72.042
              "C3H5O2 (glycerol backbone)", # ~73.029
              "SO3 (sulfur trioxide)", # ~79.957
              "HPO3 (metaphosphoric)", # ~79.966
              "C3H6O3 (sugar)", # ~90.032
              "C3H8O3 (glycerol)", # ~92.047
              "H2O4S (sulfuric)", # ~97.967
              "H3O4P (phosphoric)", # ~97.977
              "C4H8O4 (sugar)", # ~120.042
              "C6H6O3 (HRF)", # ~126.032
              "C5H8O4 (pentose-H2O)", # ~132.042
              "C8H8O2 (RDA-1)", # ~136.052
              "C6H8O4 (HRF)", # ~144.042
              "C6H10O4 (methylpentose/desoxyhexose-H2O)", # ~146.058
              "C8H8O3 (RDA-2)", # ~152.047
              "C6H10O5 (hexose-H2O)", # ~162.053
              "C8H8O4 (RDA-3)", # ~168.042
              "C8H10O4 (RDA-2-H2O)", # ~170.058
              "C6H8O6 (glucuronic-H2O)", # ~176.032
              "C6H12O6 (hexose)", # ~180.063
              "C5H14NO4P (phosphocholine-head)", # ~183.066
              "C11H10O4 (sinapoyl)", # ~206.058
              "C14H28O (myristoyl)", # ~212.214
              "C16H30O (palmitoyl)", # ~238.230
              "C16H32O (palmitoyl-sat)", # ~240.245
              "C18H32O (linoleoyl)", # ~264.245
              "C18H34O (oleoyl)", # ~266.261
              "C18H36O (stearoyl)", # ~268.277
              "C20H32O (arachidonyl)", # ~288.245
              "C20H34O (eicosadienoyl)", # ~290.261
              "C12H20O8 (2xmethylpentose/desoxyhexose-H2O)", # ~292.116
              "C22H32O (DHA acyl)", # ~312.245
              "C22H34O (EPA acyl)", # ~314.261
              "C22H36O (docosadienoyl)", # ~316.277
              "C12H20O10 (2xhexose-H2O)", # ~324.106
              "C18H30O15 (3xhexose-H2O)", # ~486.169
              # less common ones
              "CH3N (methanimine)", # ~29.027
              "H2S (dihydrosulphur)", # ~33.988
              "H2O2", # ~33.997
              "C3H6 (propene)", # ~42.047
              "CHON", # ~43.006
              "CHN", # ~27.011
              "HN", # ~15.011
              "C3H6O2 (propionic)", # ~74.037
              "C2H2O3 (CH2O-CO2)", # ~74.000
              "H2O3S (sulfate)", # ~81.972
              "C3O3 (3xCO)", # ~83.985
              "C4H9NO", # ~87.068
              "C2O4 (2xCO2)", # ~87.980
              "C3H4O3 (pyruvic)", # ~88.016
              "C4H8O2 (butyric)", # ~88.052
              "C3H7NO2 (alanine)", # ~89.048
              "C5H10O2 (valeric)", # ~102.068
              "C3H4O4 (malonic)", # ~104.011
              "C2H2O5 (CO2-CO2-H2O)", # ~105.986
              "C3H8O4 (combination)", # ~108.042
              "C5H9NO2 (proline)", # ~115.063
              "C4O4H6 (CO2-CO2-CH3-CH3)", # ~118.027
              "C2H4O6 (CO2-CO2-H2O-H2O)", # ~123.981
              # Plants-related
              "C9H6O2 (coumaroyl)", # ~146.037
              "C9H8O2 (cinnamoyl)", # ~148.052
              "C7H4O4 (galloyl)", # ~152.011
              "C9H6O3 (caffeoyl)", # ~162.032
              "C10H8O3 (feruloyl)", # ~176.047
              "C6H13NO5 (hexose-H2N)", # ~179.079
              "C7H12O6 (quinoyl)", # ~192.063
              "C6H15NO6 (hexose-H2N-H2O)", # ~197.090
              "C6H14O7 (hexose-H2O)", # ~198.074
              "C8H12O6 (acetylhexose-H2O)", # ~204.074
              "C9H12O8 (malonylhexose)", # ~264.058
              "C13H14O6 (benzoylhexose)", # ~266.079
              "C15H16O7 (coumaroylhexose)", # ~324.085
              "C13H14O9 (galloylhexose)", # ~330.058
              "C15H16O8 (caffeoylhexose)", # ~340.080
              "C16H18O8 (feruloylhexose)", # ~354.095
              "C17H20O9 (sinapoylhexose)", # ~384.106
              # PFAS-related
              "SO2F (sulfonyl fluoride)", # ~82.960
              "CF2 (perfluoroalkyl)", # ~49.997
              "CHF2 (difluoromethyl)", # ~51.005
              "C2F2 (fluorocarbon)", # ~61.997
              "CF3 (trifluoromethyl)", # ~68.995
              "C2F4 (perfluoroalkyl)", # ~99.994
              "C3F6 (perfluoroalkyl)", # ~149.990
              "C4F8 (perfluoroalkyl)", # ~199.987
              "C5F10 (perfluoroalkyl)", # ~249.984
              "C6F12 (perfluoroalkyl)", # ~299.981
              # Animal-related
              "C5H12N (choline)", # ~86.097
              "C3H8O4P (glycerophosphate)", # ~139.016
              "C2H5NO (sphingosine)", # ~59.037
              "C8H14O (cholesterol side chain)", # ~126.104
              "C9H16O (cholesterol side chain + CH2)" # ~140.120
            ),
            selected = c(
              # Matches annotate_masses.yaml enabled losses
              "CH2 (methylene)", # ~14.016
              "O", # ~15.995
              "H2O (water)", # ~18.011
              "HF (hydrogen fluoride)", # ~20.006
              "CO", # ~27.995
              "C2H4 (ethene)", # ~28.031
              "C2H5 (ethyl radical)", # ~29.039
              "NO (nitric oxide)", # ~29.998
              "CH2O (formaldehyde)", # ~30.011
              "H5ON (H2O-H3N)", # ~35.037
              "H4O2 (2xH2O)", # ~36.021
              "C2H2O (ethenone)", # ~42.011
              "CO2", # ~43.990
              "C2H4O (acetaldehyde)", # ~44.026
              "CHO2", # ~44.998
              "H6O3 (3xH2O)", # ~54.032
              "C2O2 (2xCO)", # ~55.990
              "C3H4O (acrolein)", # ~56.026
              "SO2 (sulfur dioxide)", # ~63.962
              "CF2O (carbonyl difluoride)", # ~65.992
              "CH6O3 (combination)", # ~66.032
              "C4H4O (furan)", # ~68.026
              "H8O4 (4xH2O)", # ~72.042
              "C3H5O2 (glycerol backbone)", # ~73.029
              "SO3 (sulfur trioxide)", # ~79.957
              "HPO3 (metaphosphoric)", # ~79.966
              "C3H6O3 (sugar)", # ~90.032
              "C3H8O3 (glycerol)", # ~92.047
              "H2O4S (sulfuric)", # ~97.967
              "H3O4P (phosphoric)", # ~97.977
              "C4H8O4 (sugar)", # ~120.042
              "C6H6O3 (HRF)", # ~126.032
              "C5H8O4 (pentose-H2O)", # ~132.042
              "C8H8O2 (RDA-1)", # ~136.052
              "C6H8O4 (HRF)", # ~144.042
              "C6H10O4 (methylpentose/desoxyhexose-H2O)", # ~146.058
              "C8H8O3 (RDA-2)", # ~152.047
              "C6H10O5 (hexose-H2O)", # ~162.053
              "C8H8O4 (RDA-3)", # ~168.042
              "C8H10O4 (RDA-2-H2O)", # ~170.058
              "C6H8O6 (glucuronic-H2O)", # ~176.032
              "C6H12O6 (hexose)", # ~180.063
              "C5H14NO4P (phosphocholine-head)", # ~183.066
              "C11H10O4 (sinapoyl)", # ~206.058
              "C14H28O (myristoyl)", # ~212.214
              "C16H30O (palmitoyl)", # ~238.230
              "C16H32O (palmitoyl-sat)", # ~240.245
              "C18H32O (linoleoyl)", # ~264.245
              "C18H34O (oleoyl)", # ~266.261
              "C18H36O (stearoyl)", # ~268.277
              "C20H32O (arachidonyl)", # ~288.245
              "C20H34O (eicosadienoyl)", # ~290.261
              "C12H20O8 (2xmethylpentose/desoxyhexose-H2O)", # ~292.116
              "C22H32O (DHA acyl)", # ~312.245
              "C22H34O (EPA acyl)", # ~314.261
              "C22H36O (docosadienoyl)", # ~316.277
              "C12H20O10 (2xhexose-H2O)", # ~324.106
              "C18H30O15 (3xhexose-H2O)", # ~486.169
              # Plants-related
              "C9H6O2 (coumaroyl)", # ~146.037
              "C9H8O2 (cinnamoyl)", # ~148.052
              "C7H4O4 (galloyl)", # ~152.011
              "C9H6O3 (caffeoyl)", # ~162.032
              "C10H8O3 (feruloyl)", # ~176.047
              "C6H13NO5 (hexose-H2N)", # ~179.079
              "C7H12O6 (quinoyl)", # ~192.063
              "C6H15NO6 (hexose-H2N-H2O)", # ~197.090
              "C6H14O7 (hexose-H2O)", # ~198.074
              "C8H12O6 (acetylhexose-H2O)", # ~204.074
              "C9H12O8 (malonylhexose)", # ~264.058
              "C13H14O6 (benzoylhexose)", # ~266.079
              "C15H16O7 (coumaroylhexose)", # ~324.085
              "C13H14O9 (galloylhexose)", # ~330.058
              "C15H16O8 (caffeoylhexose)", # ~340.080
              "C16H18O8 (feruloylhexose)", # ~354.095
              "C17H20O9 (sinapoylhexose)", # ~384.106
              # PFAS-related
              "SO2F (sulfonyl fluoride)", # ~82.960
              "CF2 (perfluoroalkyl)", # ~49.997
              "CHF2 (difluoromethyl)", # ~51.005
              "C2F2 (fluorocarbon)", # ~61.997
              "CF3 (trifluoromethyl)", # ~68.995
              "C2F4 (perfluoroalkyl)", # ~99.994
              "C3F6 (perfluoroalkyl)", # ~149.990
              "C4F8 (perfluoroalkyl)", # ~199.987
              "C5F10 (perfluoroalkyl)", # ~249.984
              "C6F12 (perfluoroalkyl)", # ~299.981
              # Animal-related
              "C5H12N (choline)", # ~86.097
              "C3H8O4P (glycerophosphate)", # ~139.016
              "C2H5NO (sphingosine)", # ~59.037
              "C8H14O (cholesterol side chain)", # ~126.104
              "C9H16O (cholesterol side chain + CH2)" # ~140.120
            )
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Choose wisely.",
                "If a very important neutral loss everyone should have is missing,",
                "please open an issue at:",
                as.character(
                  shiny::tags$a(
                    "https://github.com/taxonomicallyinformedannotation/tima/issues",
                    href = "https://github.com/taxonomicallyinformedannotation/tima/issues"
                  )
                )
              )
            )
        ),
        shiny::tabPanel(
          title = "Organisms",
          shiny::h3("Organisms-related parameters"),
          shiny::textInput(
            inputId = "org_tax",
            label = "OPTIONAL. Force all features to be attributed to a taxon
          (e.g. Gentiana lutea)",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "You have to input the scientific name of your taxon.",
                "Will be used for the weighting of your annotations",
                "Example: Gentiana lutea",
                "Example for multiple ones: Gentiana lutea|Swertia chirayta",
                "All features will be attributed to this source.",
                "For finer attribution,
              you need to provide a metadata file in the `Files` panel."
              )
            ),
          shiny::checkboxInput(
            inputId = "org_fil_mod",
            label = "Filter library to restrict it to a
          portion of organisms only",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "If you want, you can restrict your
              structure-organism pairs library",
                "to given taxa only.",
                "We do not advise doing so, but you are free to."
              )
            ),
          shiny::selectInput(
            inputId = "org_fil_lev",
            label = "Level at which the library will be filtered",
            choices = c(
              "domain",
              "phylum",
              "class",
              "order",
              "family",
              "tribe",
              "genus",
              "species",
              "varietas"
            ),
            selected = "phylum"
          ),
          shiny::textInput(
            inputId = "org_fil_val",
            label = "Value to be applied for filtering",
            value = "Streptophyta"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "You can also provide multiple values.",
                "Example, if you want to get a bitter library:",
                "`Simaroubaceae|Gentianaceae`"
              )
            )
        ),
        shiny::tabPanel(
          title = "Names",
          shiny::h3("Variable names parameters"),
          shiny::textInput(
            inputId = "names_adduct",
            label = "Name of `adduct` variable in the input",
            value = "best ion"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `adduct` column in your features file.",
                "The default corresponds to the default in mzmine.",
                "If using SLAW, please input 'annotation'"
              )
            ),
          shiny::textInput(
            inputId = "names_features",
            label = "Name of `feature IDs` variable in the input",
            value = "row ID"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `feature id` column in your features file.",
                "The default corresponds to the default in mzmine.",
                "If using SLAW, please input 'slaw_id'"
              )
            ),
          shiny::textInput(
            inputId = "names_filename",
            label = "Name of `filename` variable in the input",
            value = "filename"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `file name` column in your metadata file.",
                "The default is filename.",
                "If using Sequencer, please input 'Sample Name'"
              )
            ),
          shiny::checkboxInput(
            inputId = "names_extension",
            label = "The file(s) extension is present in the sample name",
            value = TRUE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Uncheck the box if you removed the files extensions",
                "of your file in the features file",
                "The default corresponds to the default in mzmine."
              )
            ),
          shiny::textInput(
            inputId = "names_compound_name",
            label = "Name of `compound name` variable in the input",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `inchikey` column in your file."
            ),
          shiny::textInput(
            inputId = "names_inchikey",
            label = "Name of `inchikey` variable in the input",
            value = "inchikey"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `inchikey` column in your file."
            ),
          shiny::textInput(
            inputId = "names_precursor",
            label = "Name of `precursor m/z` variable in the input",
            value = "row m/z"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `precursor m/z` column in your features file.",
                "The default corresponds to the default in mzmine.",
                "If using SLAW, please input 'mz'"
              )
            ),
          shiny::textInput(
            inputId = "names_rt",
            label = "Name of `retention time` variable in the feature table",
            value = "row retention time"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `retention time` column in your features file.",
                "The default corresponds to the default in mzmine.",
                "If using SLAW, please input 'rt'",
                "Assumed to be in minutes."
              )
            ),
          shiny::textInput(
            inputId = "names_rt_2",
            label = "Name of `retention time` variable in the rt library",
            value = "rt"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `retention time` column in your rt library file."
            ),
          shiny::textInput(
            inputId = "names_smiles",
            label = "Name of `SMILES` variable in the input",
            value = "smiles"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `SMILES` column in your file."
            ),
          shiny::textInput(
            inputId = "names_source",
            label = "Name of `source IDs` variable in the input",
            value = "CLUSTERID1"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `source id` column in your edges file.",
                "The default corresponds to the default in GNPS."
              )
            ),
          shiny::textInput(
            inputId = "names_target",
            label = "Name of `target IDs` variable in the input",
            value = "CLUSTERID2"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `target id` column in your edges file.",
                "The default corresponds to the default in GNPS."
              )
            ),
          shiny::textInput(
            inputId = "names_taxon",
            label = "Name of `taxon name` variable in the input",
            value = "ATTRIBUTE_species"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Name of the `taxon name` column in your edges file.",
                "The default corresponds to the default in GNPS."
              )
            ),
          shiny::h3("MGF names parameters"),
          shiny::textInput(
            inputId = "names_mgf_ad",
            label = "Name of `adduct` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `adduct` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_ce",
            label = "Name of `collision energy` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `collision energy` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_ci",
            label = "Name of `compound id` variable in the MGF",
            value = "SPECTRUMID"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `compound id` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_in",
            label = "Name of `InChI` variable in the MGF",
            value = "INCHI"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `InChI` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_io",
            label = "Name of `InChI no stereo` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `InChI no stereo` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_ik",
            label = "Name of `InChIKey` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `InChIKey` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_il",
            label = "Name of `InChIKey connectivity layer` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `InChIKey connectivity layer` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_na",
            label = "Name of `name` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `name` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_po",
            label = "Name of `polarity` variable in the MGF",
            value = "IONMODE"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `polarity` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_rt",
            label = "Name of `retention time` variable in the MGF",
            value = "RTINSECONDS"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `retention time` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_sm",
            label = "Name of `SMILES` variable in the MGF",
            value = "SMILES"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `SMILES` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_sn",
            label = "Name of `SMILES no stereo` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `SMILES no stereo` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_si",
            label = "Name of `spectrum ID` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `spectrum ID` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_sp",
            label = "Name of `SPLASH` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `SPLASH` column in your MGF file."
            ),
          shiny::textInput(
            inputId = "names_mgf_sy",
            label = "Name of `synonyms` variable in the MGF",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the `synonyms` column in your MGF file."
            )
        ),
        shiny::tabPanel(
          title = "Weights",
          shiny::h3("Weights-related parameters"),
          shiny::sliderInput(
            inputId = "wei_glo_bio",
            label = "Weight for the biological part",
            min = 0,
            max = 1,
            value = 0.500,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Weight attributed to the biological score.",
                "Depends on the organism you are studying,
              and how specialized its metabolome is.",
                "Also depends if you favor novelty over previous knowledge.",
                "We advise this value to be high."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_glo_che",
            label = "Weight for the chemical part",
            min = 0,
            max = 1,
            value = 0.166,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Weight attributed to the chemical score.",
                "We advise this value to be low.",
                "Current chemical taxonomies do not perform well."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_glo_spe",
            label = "Weight for the spectral part",
            min = 0,
            max = 1,
            value = 0.333,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Weight attributed to the spectral score.",
                "Depends on the quality of your spectra.",
                "We advise this value to be medium to high."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_01",
            label = "Score for a biological domain match",
            min = 0,
            max = 1,
            value = 0.100,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `domain` match.",
                "We advise this value to be the lowest."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_02",
            label = "Score for a biological kingdom match",
            min = 0,
            max = 1,
            value = 0.200,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `kingdom` match.",
                "We advise this value to be higher than the one of `domain`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_03",
            label = "Score for a biological phylum match",
            min = 0,
            max = 1,
            value = 0.300,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `phylum` match.",
                "We advise this value to be higher than the one of `kingdom`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_04",
            label = "Score for a biological class match",
            min = 0,
            max = 1,
            value = 0.400,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `class` match.",
                "We advise this value to be higher than the one of `phylum`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_05",
            label = "Score for a biological order match",
            min = 0,
            max = 1,
            value = 0.500,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `order` match.",
                "We advise this value to be higher than the one of `class`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_06",
            label = "Score for a biological infraorder match",
            min = 0,
            max = 1,
            value = 0.550,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `infraorder` match.",
                "We advise this value to be higher than the one of `order`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_07",
            label = "Score for a biological family match",
            min = 0,
            max = 1,
            value = 0.600,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `family` match.",
                "We advise this value to be higher than the one of `infraorder`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_08",
            label = "Score for a biological subfamily match",
            min = 0,
            max = 1,
            value = 0.650,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `subfamily` match.",
                "We advise this value to be higher than the one of `family`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_09",
            label = "Score for a biological tribe match",
            min = 0,
            max = 1,
            value = 0.700,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `tribe` match.",
                "We advise this value to be higher than the one of `subfamily`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_10",
            label = "Score for a biological subtribe match",
            min = 0,
            max = 1,
            value = 0.750,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `subtribe` match.",
                "We advise this value to be higher than the one of `tribe`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_11",
            label = "Score for a biological genus match",
            min = 0,
            max = 1,
            value = 0.800,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `genus` match.",
                "We advise this value to be higher than the one of `subtribe`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_12",
            label = "Score for a biological subgenus match",
            min = 0,
            max = 1,
            value = 0.850,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `subgenus` match.",
                "We advise this value to be higher than the one of `genus`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_13",
            label = "Score for a biological species match",
            min = 0,
            max = 1,
            value = 0.900,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `species` match.",
                "We advise this value to be higher than the one of `subgenus`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_14",
            label = "Score for a biological subspecies match",
            min = 0,
            max = 1,
            value = 0.950,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `subspecies` match.",
                "We advise this value to be higher than the one of `species`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_15",
            label = "Score for a biological variety match",
            min = 0,
            max = 1,
            value = 1.000,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `variety` match.",
                "We advise this value to be higher than the one of `subspecies`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_bio_16",
            label = "Score for a biological Biota match",
            min = 0,
            max = 1.007276,
            value = 1.007276,
            step = 0.000001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `Biota` match.",
                "This is a special value."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_11",
            label = "Score for a (classyfire) chemical kingdom match",
            min = 0,
            max = 1,
            value = 0.250,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `kingdom (classyfire)` match.",
                "We advise this value to be the lowest."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_12",
            label = "Score for a (classyfire) chemical superclass match",
            min = 0,
            max = 1,
            value = 0.500,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `superclass (classyfire)` match.",
                "We advise this value to be higher
              than the one of `kingdom (classyfire)`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_13",
            label = "Score for a (classyfire) chemical class match",
            min = 0,
            max = 1,
            value = 0.750,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `class (classyfire)` match.",
                "We advise this value to be higher
              than the one of `superclass (classyfire)`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_14",
            label = "Score for a (classyfire) chemical parent match",
            min = 0,
            max = 1,
            value = 1.000,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `parent (classyfire)` match.",
                "We advise this value to be higher
              than the one of `class (classyfire)`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_21",
            label = "Score for a (NPC) chemical pathway match",
            min = 0,
            max = 1,
            value = 0.333,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `pathway (npclassifier)` match.",
                "We advise this value to be the lowest."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_22",
            label = "Score for a (NPC) chemical superclass match",
            min = 0,
            max = 1,
            value = 0.666,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `superclass (npclassifier)` match.",
                "We advise this value to be higher
              than the one of `pathway (npclassifier)`."
              )
            ),
          shiny::sliderInput(
            inputId = "wei_che_23",
            label = "Score for a (NPC) chemical class match",
            min = 0,
            max = 1,
            value = 1.000,
            step = 0.001,
            ticks = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Score for a `class (npclassifier)` match.",
                "We advise this value to be higher
              than the one of `superclass (npclassifier)`."
              )
            )
        ),
        shiny::tabPanel(
          title = "Options",
          shiny::h3("Options parameters"),
          shiny::checkboxInput(
            inputId = "compounds_names",
            label = "Compound names",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "If ticked, compounds names will be reported.",
                "This will require much more memory,
              as they are the longest strings."
              )
            ),
          shiny::checkboxInput(
            inputId = "high_evidence",
            label = "High evidence only",
            value = TRUE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "If ticked, only compounds with high evidence will be reported.",
                "This is an important filter. Do not hesitate removing it for more coverage."
              )
            ),
          shiny::checkboxInput(
            inputId = "remove_ties",
            label = "Remove ties",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "If checked, only one of the candidates with the exact same score will be kept randomly.",
                "Not adivsed, but here for convenience."
              )
            ),
          shiny::checkboxInput(
            inputId = "summarize",
            label = "summarize results to one row per feature",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "If checked, the output will be a Cytoscape-ready table,",
                "with multiple candidates per line, separated by pipes (|).",
                "If unchecked, will output multiple lines per features
              (one per candidate)."
              )
            ),
          shiny::checkboxInput(
            inputId = "force",
            label = "Do not use it!",
            value = FALSE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Really, do not use it.",
                "Corresponds to a `--force` parameter,
              allowing for crazy things that can crash.",
                "Really, do not use it."
              )
            )
        ),
        shiny::tabPanel(
          title = "Libraries (optional)",
          shiny::h3("Additional personal libraries"),
          shiny::h4("Temporal (retention time)"),
          shiny::fileInput(
            inputId = "lib_tmp_exp_csv",
            label = "Experimental (CSV)",
            accept = c(
              ".csv",
              ".tsv",
              ".csv.gz",
              ".tsv.gz",
              ".csv.zip",
              ".tsv.zip"
            ),
            multiple = TRUE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "A TSV of retention times.",
                "The default column names are `rt`, `smiles`, `inchikey`.",
                "Do not forget to adapt the names parameters if needed.",
                "Do not forget to fill down seconds or minutes below."
              )
            ),
          shiny::fileInput(
            inputId = "lib_tmp_is_csv",
            label = "In silico (CSV)",
            accept = c(
              ".csv",
              ".tsv",
              ".csv.gz",
              ".tsv.gz",
              ".csv.zip",
              ".tsv.zip"
            ),
            multiple = TRUE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "A TSV of retention times.",
                "The default column names are `rt`, `smiles`, `inchikey`.",
                "Do not forget to adapt the names parameters if needed.",
                "Do not forget to fill down seconds or minutes below."
              )
            ),
          shiny::h4("Spectral"),
          shiny::fileInput(
            inputId = "lib_spe_mgf",
            label = "MGF",
            accept = ".mgf",
            multiple = TRUE
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "An MGF containing reference spectra.",
                "If it contains retention times, they will also be used.",
                "Do not forget to fill down seconds or minutes below."
              )
            ),
          shiny::textInput(
            inputId = "names_libraries",
            label = "Name of the library",
            value = "internal"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "Name of the library. Related content will be identified with this name."
            ),
          shiny::selectInput(
            inputId = "uni_rt",
            label = "Retention time unit",
            choices = c("seconds", "minutes"),
            selected = "seconds"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Unit of the retention time.",
                "Must be `seconds` or `minutes`."
              )
            ),
        ),
        shiny::tabPanel(
          title = "GNPS (optional)",
          shiny::h3("GNPS parameters"),
          shiny::textInput(
            inputId = "gnps_id",
            label = "GNPS job ID",
            value = NULL
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = c(
                "Your GNPS job ID.",
                "If you provide one, all files will be taken from there.",
                "You still need to provide a path for
              where we will store the files,",
                "in case your GNPS job gets deleted for you to find them back.",
                "Annotations will also be downloaded."
              )
            ),
          shiny::selectInput(
            inputId = "gnps_workflow",
            label = "Workflow used within GNPS",
            choices = c("classical", "fbmn"),
            selected = "fbmn"
          ) |>
            shinyhelper::helper(
              type = "inline",
              content = "We advise `fbmn`, but we also support `classical` jobs."
            )
        )
      )
    )
  ),
  shiny::fluidPage(
    shiny::div(
      id = "form",
      shiny::actionButton(
        inputId = "save",
        label = "Save parameters",
        class = "btn-primary"
      ),
      shinyjs::hidden(
        shiny::span(id = "save_msg", "Saving parameters"),
        shiny::div(
          id = "error",
          shiny::div(
            shiny::br(),
            shiny::tags$b("Error: "),
            shiny::span(id = "error_msg")
          )
        )
      ),
      shiny::actionButton(
        inputId = "launch",
        label = "Launch job",
        class = "btn-primary"
      ),
    ),
    shinyjs::hidden(
      shiny::div(
        id = "thankyou_msg",
        shiny::h3("Thanks, your parameters were saved successfully!")
      )
    ),
    shinyjs::hidden(
      shiny::downloadButton(
        outputId = "results_mini",
        "Download mini results"
      )
    ),
    shinyjs::hidden(
      shiny::downloadButton(
        outputId = "results_filtered",
        "Download filtered results"
      )
    ),
    shinyjs::hidden(
      shiny::downloadButton(
        outputId = "results_full",
        "Download full results"
      )
    ),
    shinyjs::hidden(
      shiny::downloadButton(
        outputId = "results_mztab",
        "Download mzTab"
      )
    ),
    shinyjs::hidden(shiny::actionButton(inputId = "close", label = "Close")),
    shinyjs::hidden(shiny::div(id = "job_msg", shiny::h3("Job is running!"))),
    shinyjs::hidden(shiny::div(id = "job_end", shiny::h3("Job finished!")))
  )
)
