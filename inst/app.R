# Check if runs in Docker environment or not
if (file.exists("/.dockerenv")) {
  options(shiny.host = "0.0.0.0")
} else {
  options(shiny.host = "127.0.0.1")
}
options(shiny.port = 3838)
options(shiny.maxRequestSize = 2000 * 1024^2)

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
      shiny::a(href = "https://taxonomicallyinformedannotation.github.io/tima", "following documentation")
    ),
    shiny::strong(
      shiny::span("Created by "),
      shiny::a("Adriano Rutz", href = "https://adafede.github.io/"),
      shiny::HTML("&bull;"),
      shiny::a("Main publication", href = "https://doi.org/10.3389/fpls.2019.01329"),
      shiny::HTML("&bull;"),
      shiny::span("Code"),
      shiny::a("on GitHub", href = "https://github.com/taxonomicallyinformedannotation/tima")
    )
  ),
  shiny::fluidPage(shiny::div(
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
            label = .label_mandatory("MGF file"),
            accept = ".mgf"
          ),
          shiny::downloadButton(outputId = "demo_spe", "Download example spectra")
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
            label = .label_mandatory("Features table"),
            accept = c(".csv", ".tsv", ".csv.gz", ".tsv.gz", ".csv.zip", ".tsv.zip")
          ),
          shiny::downloadButton(outputId = "demo_fea", "Download example features")
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
            accept = c(".csv", ".tsv", ".csv.gz", ".tsv.gz", ".csv.zip", ".tsv.zip")
          ),
          shiny::downloadButton(outputId = "demo_met", "Download example metadata")
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
          inputId = "fil_ann_raw_sir",
          label = "SIRIUS project space (optional)",
          accept = c(".zip")
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "The compressed directory containing the SIRIUS project space.",
              "Should be located in `data/interim`.",
              "Reason therefore is to find it in the future."
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
            content = c("The SIRIUS version used.", "We highly recommend 6, the default.")
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
          inputId = "ann_can_fin",
          label = "Number of final candidates",
          min = 1,
          max = 500,
          value = 3,
          step = 1,
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
        shiny::sliderInput(
          inputId = "ann_thr_ms2_sim",
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
        shiny::sliderInput(
          inputId = "edg_thr_ms2_sim",
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
            content = c("Polarity of the MS experiment.")
          ),
        shiny::sliderInput(
          inputId = "ms_thr_ms2_int",
          label = "Intensity threshold for MS2",
          min = 0,
          max = 1E4,
          value = 5,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "MS2 intensity threshold.",
              "Fragments below this threshold will be removed from spectra."
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
            content = c("Mass tolerance (in ppm) used for MS1 annotation.")
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
            content = c("Mass tolerance (in Da) used for MS1 annotation.")
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
            content = c("Mass tolerance (in ppm) used for MS2 annotation.")
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
            content = c("Mass tolerance (in Da) used for MS2 annotation.")
          ),
        shiny::sliderInput(
          inputId = "ms_tol_rt_add",
          label = "Retention time tolerance for adducts annotation in minutes",
          min = 0.005,
          max = 0.1,
          value = 0.02,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Retention time tolerance (minutes) used for adducts annotation.")
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
        shiny::checkboxGroupInput(
          inputId = "ms_add_pos",
          label = "List of adducts to be used in positive",
          choices = list(
            "[M+H3]3+",
            "[M+H2Na]3+",
            "[M+HNa2]3+",
            "[M+Na3]3+",
            "[M+H2]2+",
            "[M+HNa]2+",
            "[M+Mg]2+",
            "[M+HK]2+",
            "[M+Ca]2+",
            "[M+Na2]2+",
            "[M+Fe]2+",
            "[M]+",
            "[M+H]+",
            "[M+H4N]+",
            "[M+Na]+",
            "[M+K]+",
            "[M+Fe-H2]+",
            "[M+Fe-H]+",
            "[M+Cu]+",
            "[2M+Mg]2+",
            "[2M+Ca]2+",
            "[2M+Fe]2+",
            "[2M+H]+",
            "[2M+H4N]+",
            "[2M+Na]+",
            "[2M+K]+"
          ),
          selected = list("[M+H2]2+", "[M+H]+", "[M+H4N]+", "[M+Na]+", "[2M+H]+")
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
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima/issues"
                )
              )
            )
          ),
        shiny::checkboxGroupInput(
          inputId = "ms_add_neg",
          label = "List of adducts to be used in negative",
          choices = list(
            "[M-H3]3-",
            "[M-H2]2-",
            "[M]-",
            "[M-H]-",
            "[M+F]-",
            "[M+Na-H2]-",
            "[M+Cl]-",
            "[M+K-H2]-",
            "[M+Br]-",
            "[2M-H]-",
            "[3M-H]-"
          ),
          selected = list("[M-H2]2-", "[M-H]-", "[2M-H]-")
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
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima/issues"
                )
              )
            )
          ),
        shiny::checkboxGroupInput(
          inputId = "ms_clu_pos",
          label = "List of clusters to be used in positive",
          choices = list(
            "H2O", # (water)
            # "CH4O", # (methanol)
            "C2H3N", # (acetonitrile)
            # "C2H7N", # (ethylamine)
            # "C2H6O", # (ethanol)
            "NaCl" # (sodium chloride)
            # "C3H8O", # (isopropanol)
            # "C2H6OS" # (dmso)
          ),
          selected = list(
            "H2O", # (water)
            # "CH4O", # (methanol)
            "C2H3N" # (acetonitrile)
            # "C2H7N", # (ethylamine)
            # "C2H6O", # (ethanol)
            # "NaCl", # (sodium chloride)
            # "C3H8O", # (isopropanol)
            # "C2H6OS" # (dmso)
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
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima/issues"
                )
              )
            )
          ),
        shiny::checkboxGroupInput(
          inputId = "ms_clu_neg",
          label = "List of clusters to be used in negative",
          choices = list(
            "H2O", # (water)
            "CH2O2", # (formic)
            "NaCl", # (sodium chloride)
            "C2H4O2", # (acetic)
            "H2PO4", # (phosphoric)
            "C2HF3O2" # (tfa)
          ),
          selected = list(
            "H2O", # (water)
            "CH2O2", # (formic)
            # "NaCl", # (sodium chloride)
            # "C2H4O2", # (acetic)
            "H2PO4" # (phosphoric)
            # "C2HF3O2" # (tfa)
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
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima/issues"
                )
              )
            )
          ),
        shiny::checkboxGroupInput(
          inputId = "ms_neu",
          label = "List of neutral losses to be used",
          choices = list(
            "HN",
            "CH3",
            "O",
            "H3N (ammonia)",
            "H2O (water)",
            "CHN",
            "CO",
            "C2H4 (ethene)",
            "CH3N (methanimine)",
            "C2H5 (ethyl radical)",
            "H2O2 (dioxygen-dihydrogen)",
            "CH2O",
            "H2S (dihydrosulphur)",
            "H5ON (H2O-H3N)",
            "H4O2 (2xH2O)",
            "C2H2O (ethenone)",
            "C3H6 (propene)",
            "CHON",
            "CO2",
            "CHO2",
            "H6O3 (3xH2O)",
            "C2O2 (2xCO)",
            "CH6O3 (combination)",
            "H8O4 (4xH2O)",
            "C2H2O3 (CH2O-CO2)",
            "C3H6O2 (CH3-CH2-COOH)",
            "C3O3 (3xCO)",
            "C4H9NO (combination)",
            "C2O4 (2xCO2)",
            "C3H4O3 (pyruvic)",
            "C4H8O2 (butyric)",
            "C3H7NO2 (alanine)",
            "C3H6O3 (sugar)",
            "H2O4S (sulfuric)",
            "H3O4P (phosphoric)",
            "C5H10O2 (valeric)",
            "C3H4O4 (malonic)",
            "C2H2O5 (CO2-CO2-H2O)",
            "C3H8O4 (combination)",
            "C5H9NO2 (proline)",
            "C4O4H6 (CO2-CO2-CH3-CH3)",
            "C4H8O4 (sugar)",
            "C2H4O6 (CO2-CO2-H2O-H2O)",
            "C6H6O3 (HRF)",
            "C5H8O4 (pentose-H2O)",
            "C8H8O2 (RDA-1)",
            "C6H8O4 (HRF)",
            "C9H6O2 (coumaroyl)",
            "C6H10O4 (methylpentose/desoxyhexose-H2O)",
            "C9H8O2 (cinnamoyl)",
            "C7H4O4 (galloyl)",
            "C8H8O3 (RDA-2)",
            "C9H6O3 (caffeoyl)",
            "C6H10O5 (hexose-H2O)",
            "C8H8O4 (RDA-3)",
            "C8H10O4 (RDA-2-H2O)",
            "C6H8O6 (glucuronic-H2O)",
            "C10H8O3 (feruloyl)",
            "C6H13NO5 (hexose-H2N)",
            "C6H12O6 (hexose)",
            "C7H12O6 (quinoyl)",
            "C6H15NO6 (hexose-H2N-H2O)",
            "C6H14O7 (hexose-H2O)",
            "C8H12O6 (acetylhexose-H2O)",
            "C11H10O4 (sinapoyl)",
            "C16H30O (pamitoyl)",
            "C9H12O8 (malonylhexose)",
            "C13H14O6 (benzoylhexose)",
            "C12H20O8 (2xmethylpentose/desoxyhexose-H2O)",
            "C15H16O7 (coumaroylhexose)",
            "C13H14O9 (galloylhexose)",
            "C15H16O8 (caffeoylhexose)",
            "C12H20O10 (2xhexose-H2O)",
            "C16H18O8 (feruloylhexose)",
            "C17H20O9 (sinapoylhexose)",
            "C18H30O15 (3xhexose-H2O)"
          ),
          selected = list(
            "O",
            "H3N (ammonia)",
            "H2O (water)",
            "CO",
            "C2H5 (ethyl radical)",
            "H4O2 (2xH2O)",
            "CO2",
            "CHO2",
            "H6O3 (3xH2O)",
            "C2O2 (2xCO)",
            "CH6O3 (combination)",
            "H8O4 (4xH2O)",
            "C3H6O3 (sugar)",
            "H2O4S (sulfuric)",
            "H3O4P (phosphoric)",
            "C4H8O4 (sugar)",
            "C6H6O3 (HRF)",
            "C5H8O4 (pentose-H2O)",
            "C8H8O2 (RDA-1)",
            "C6H8O4 (HRF)",
            "C6H10O4 (methylpentose/desoxyhexose-H2O)",
            "C8H8O3 (RDA-2)",
            "C6H10O5 (hexose-H2O)",
            "C8H8O4 (RDA-3)",
            "C8H10O4 (RDA-2-H2O)",
            "C6H12O6 (hexose)",
            "C11H10O4 (sinapoyl)",
            "C16H30O (pamitoyl)",
            "C12H20O8 (2xmethylpentose/desoxyhexose-H2O)",
            "C12H20O10 (2xhexose-H2O)"
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
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima/issues"
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
        shiny::sliderInput(
          inputId = "org_can",
          label = "Number of organisms to keep per feature",
          min = 1,
          max = 5,
          value = 1,
          step = 1,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Only has impact if you provided a metadata file in the
              `Files` panel.",
              "In this case,
              it will take the intensity matrix of your features",
              "and attribute the n organisms where the highest intensity",
              "was observed as source to your features.",
              "Useful when your experiment contains
              different biological sources.",
              "As the organisms where the measured intensity
              is the highest are also the ones",
              "with higher likelihood of the
              corresponding compounds being isolated,",
              "the number of organisms can be kept low."
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
              "The default corresponds to the default in MZmine.",
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
              "The default corresponds to the default in MZmine.",
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
              "The default corresponds to the default in MZmine."
            )
          ),
        shiny::textInput(
          inputId = "names_inchikey",
          label = "Name of `inchikey` variable in the input",
          value = "inchikey"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `inchikey` column in your file.")
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
              "The default corresponds to the default in MZmine.",
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
              "The default corresponds to the default in MZmine.",
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
            content = c("Name of the `retention time` column in your rt library file.")
          ),
        shiny::textInput(
          inputId = "names_smiles",
          label = "Name of `SMILES` variable in the input",
          value = "smiles"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `SMILES` column in your file.")
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
            content = c("Name of the `adduct` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_ce",
          label = "Name of `collision energy` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `collision energy` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_ci",
          label = "Name of `compound id` variable in the MGF",
          value = "SPECTRUMID"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `compound id` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_em",
          label = "Name of `exact mass` variable in the MGF",
          value = "EXACTMASS"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `exact mass` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_in",
          label = "Name of `InChI` variable in the MGF",
          value = "INCHI"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `InChI` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_io",
          label = "Name of `InChI no stereo` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `InChI no stereo` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_ik",
          label = "Name of `InChIKey` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `InChIKey` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_il",
          label = "Name of `InChIKey no stereo` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `InChIKey no stereo` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_mf",
          label = "Name of `molecular formula` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `molecular formula` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_na",
          label = "Name of `name` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `name` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_po",
          label = "Name of `polarity` variable in the MGF",
          value = "IONMODE"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `polarity` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_rt",
          label = "Name of `retention time` variable in the MGF",
          value = "RTINSECONDS"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `retention time` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_sm",
          label = "Name of `SMILES` variable in the MGF",
          value = "SMILES"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `SMILES` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_sn",
          label = "Name of `SMILES no stereo` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `SMILES no stereo` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_si",
          label = "Name of `spectrum ID` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `spectrum ID` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_sp",
          label = "Name of `SPLASH` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `SPLASH` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_sy",
          label = "Name of `synonyms` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `synonyms` column in your MGF file.")
          ),
        shiny::textInput(
          inputId = "names_mgf_xl",
          label = "Name of `xlogP` variable in the MGF",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Name of the `xlogP` column in your MGF file.")
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
          inputId = "high_confidence",
          label = "High confidence only",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "If ticked, only compounds with high confidence will be reported.",
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
          inputId = "summarise",
          label = "Summarise results to one row per feature",
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
          accept = c(".csv", ".tsv", ".csv.gz", ".tsv.gz", ".csv.zip", ".tsv.zip"),
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
          accept = c(".csv", ".tsv", ".csv.gz", ".tsv.gz", ".csv.zip", ".tsv.zip"),
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
          accept = c(".mgf"),
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
            content = c(
              "Name of the library. Related content will be identified with this name."
            )
          ),
        shiny::selectInput(
          inputId = "uni_rt",
          label = "Retention time unit",
          choices = c("seconds", "minutes"),
          selected = "seconds"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Unit of the retention time.", "Must be `seconds` or `minutes`.")
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
            content = c("We advise `fbmn`,
                        but we also support `classical` jobs.")
          )
      )
    )
  )),
  shiny::fluidPage(
    shiny::div(
      id = "form",
      shiny::actionButton(
        inputId = "save",
        label = "Save parameters",
        class = "btn-primary"
      ),
      shinyjs::hidden(
        shiny::span(id = "save_msg", "Saving parameters..."),
        shiny::div(id = "error", shiny::div(
          shiny::br(),
          shiny::tags$b("Error: "),
          shiny::span(id = "error_msg")
        ))
      ),
      shiny::actionButton(
        inputId = "launch",
        label = "Launch job",
        class = "btn-primary"
      ),
    ),
    shinyjs::hidden(shiny::div(
      id = "thankyou_msg",
      shiny::h3("Thanks, your parameters were saved successfully!")
    )),
    shinyjs::hidden(
      shiny::div(
        id = "targets",
        shiny::mainPanel(
          targets::tar_watch_ui(
            id = "targets-shiny",
            seconds = 10,
            targets_only = TRUE,
            degree_from = 8,
            display = "graph"
          )
        )
      )
    ),
    shinyjs::hidden(shiny::downloadButton(outputId = "results", "Download results")),
    shinyjs::hidden(shiny::actionButton(inputId = "close", label = "Close")),
    shinyjs::hidden(shiny::div(id = "job_msg", shiny::h3("Job is running!"))),
    shinyjs::hidden(shiny::div(id = "job_end", shiny::h3("Job finished!")))
  )
)

# save the results to a file
.save_input <- function(input) {
  paths_data_source <- tima:::get_default_paths()$data$source$path
  paths_data_interim_annotations <-
    tima:::get_default_paths()$data$interim$annotations$path
  ## safety
  tima:::create_dir(paths_data_source)

  list <- tima:::load_yaml_files()

  yamls_params <- list$yamls_params
  yaml_files <- list$yaml_files
  yaml_names <- list$yaml_names

  ## This allows to keep files correctly placed in `data/source` clean
  prefil_fea_raw <- shiny::isolate(input$fil_fea_raw)
  prefil_spe_raw <- shiny::isolate(input$fil_spe_raw)
  prefil_met_raw <- shiny::isolate(input$fil_met_raw)
  prefil_sir_raw <- shiny::isolate(input$fil_ann_raw_sir)

  prefil_fea_raw_1 <- file.path(paths_data_source, prefil_fea_raw[[1]])
  prefil_spe_raw_1 <- file.path(paths_data_source, prefil_spe_raw[[1]])
  if (!is.null(prefil_met_raw)) {
    prefil_met_raw_1 <- file.path(paths_data_source, prefil_met_raw[[1]])
  }
  if (!is.null(prefil_sir_raw)) {
    prefil_sir_raw_1 <-
      file.path(paths_data_interim_annotations, prefil_sir_raw[[1]])
  }

  if (!file.exists(prefil_fea_raw_1)) {
    fs::file_copy(
      path = prefil_fea_raw[[4]],
      new_path = file.path(prefil_fea_raw_1),
      overwrite = TRUE
    )
  }
  if (!file.exists(prefil_spe_raw_1)) {
    fs::file_copy(
      path = prefil_spe_raw[[4]],
      new_path = file.path(prefil_spe_raw_1),
      overwrite = TRUE
    )
  }
  if (!is.null(prefil_met_raw)) {
    if (!file.exists(prefil_met_raw_1)) {
      fs::file_copy(
        path = prefil_met_raw[[4]],
        new_path = file.path(prefil_met_raw_1),
        overwrite = TRUE
      )
    }
  } else {
    prefil_met_raw_1 <- NULL
  }
  if (!is.null(prefil_sir_raw)) {
    if (!file.exists(prefil_sir_raw_1)) {
      ## safety
      tima:::create_dir(paths_data_interim_annotations)
      fs::file_copy(
        path = prefil_sir_raw[[4]],
        new_path = file.path(prefil_sir_raw_1),
        overwrite = TRUE
      )
    }
  } else {
    prefil_sir_raw_1 <- NULL
  }
  if (!is.null(lib_tmp_exp_csv)) {
    if (!file.exists(lib_tmp_exp_csv)) {
      fs::file_copy(
        path = lib_tmp_exp_csv[[4]],
        new_path = file.path(lib_tmp_exp_csv),
        overwrite = TRUE
      )
    }
  } else {
    lib_tmp_exp_csv <- NULL
  }
  if (!is.null(lib_tmp_is_csv)) {
    if (!file.exists(lib_tmp_is_csv)) {
      fs::file_copy(
        path = lib_tmp_is_csv[[4]],
        new_path = file.path(lib_tmp_is_csv),
        overwrite = TRUE
      )
    }
  } else {
    lib_tmp_is_csv <- NULL
  }

  fil_fea_raw <- prefil_fea_raw_1
  fil_spe_raw <- prefil_spe_raw_1
  fil_met_raw <- prefil_met_raw_1
  fil_sir_raw <- prefil_sir_raw_1

  fil_pat <- shiny::isolate(input$fil_pat)

  gnps_job_id <- shiny::isolate(input$gnps_id)
  if (gnps_job_id == "") {
    gnps_job_id <- NULL
  }

  org_tax <- shiny::isolate(input$org_tax)
  if (org_tax == "") {
    org_tax <- NULL
  }
  hig_con <- shiny::isolate(input$high_confidence)
  ms_pol <- shiny::isolate(input$ms_pol)
  summarise <- shiny::isolate(input$summarise)

  message(x = "Changing parameters ...")
  message(x = "... Small")
  yaml_small <- yamls_params[["params/prepare_params"]]
  yaml_small$files$pattern <- fil_pat
  yaml_small$files$features$raw <- fil_fea_raw
  yaml_small$files$metadata$raw <- fil_met_raw
  yaml_small$files$annotations$raw$sirius <- fil_sir_raw
  yaml_small$files$spectral$raw <- fil_spe_raw
  yaml_small$ms$polarity <- ms_pol
  yaml_small$organisms$taxon <- org_tax
  yaml_small$options$high_confidence <- hig_con
  yaml_small$options$summarise <- summarise
  tima:::create_dir("params")
  yaml::write_yaml(x = yaml_small, file = tima:::get_default_paths()$params$prepare_params)

  message(x = "... Advanced")
  yaml_advanced <- yamls_params[["params/prepare_params_advanced"]]
  yaml_advanced$annotations$candidates$final <-
    shiny::isolate(input$ann_can_fin)
  yaml_advanced$annotations$ms1only <-
    shiny::isolate(input$ann_ms1only)
  yaml_advanced$annotations$ms2approx <-
    shiny::isolate(input$ann_ms2_app)
  yaml_advanced$annotations$thresholds$consistency <-
    shiny::isolate(input$ann_thr_con)
  yaml_advanced$annotations$thresholds$ms1$biological <-
    shiny::isolate(input$ann_thr_ms1_bio)
  yaml_advanced$annotations$thresholds$ms1$chemical <-
    shiny::isolate(input$ann_thr_ms1_che)
  yaml_advanced$annotations$thresholds$ms1$condition <-
    shiny::isolate(input$ann_thr_ms1_con)
  yaml_advanced$annotations$thresholds$ms2$similarity$annotation <-
    shiny::isolate(input$ann_thr_ms2_sim)
  yaml_advanced$annotations$thresholds$ms2$similarity$edges <-
    shiny::isolate(input$edg_thr_ms2_sim)
  yaml_advanced$files$pattern <- fil_pat
  yaml_advanced$files$annotations$raw$spectral$gnps <-
    yaml_advanced$files$annotations$raw$spectral$gnps |>
    tima:::replace_id()
  yaml_advanced$files$annotations$raw$spectral$spectral <-
    yaml_advanced$files$annotations$raw$spectral$spectral |>
    tima:::replace_id()
  # yaml_advanced$files$annotations$raw$sirius <-
  #   yaml_advanced$files$annotations$raw$sirius |>
  #   tima:::replace_id()
  yaml_advanced$files$annotations$raw$sirius <- fil_sir_raw
  yaml_advanced$files$annotations$prepared$canopus <-
    yaml_advanced$files$annotations$prepared$canopus |>
    tima:::replace_id()
  yaml_advanced$files$annotations$prepared$formula <-
    yaml_advanced$files$annotations$prepared$formula |>
    tima:::replace_id()
  yaml_advanced$files$annotations$prepared$structural$gnps <-
    yaml_advanced$files$annotations$prepared$structural$gnps |>
    tima:::replace_id()
  yaml_advanced$files$annotations$prepared$structural$ms1 <-
    yaml_advanced$files$annotations$prepared$structural$ms1 |>
    tima:::replace_id()
  yaml_advanced$files$annotations$prepared$structural$sirius <-
    yaml_advanced$files$annotations$prepared$structural$sirius |>
    tima:::replace_id()
  yaml_advanced$files$annotations$prepared$structural$spectral <-
    yaml_advanced$files$annotations$prepared$structural$spectral |>
    tima:::replace_id()
  yaml_advanced$files$annotations$filtered <-
    yaml_advanced$files$annotations$filtered |>
    tima:::replace_id()
  # yaml_advanced$files$annotations$processed <-
  #   yaml_advanced$files$annotations$processed |>
  #   tima:::replace_id()
  yaml_advanced$files$features$raw <-
    fil_fea_raw
  yaml_advanced$files$features$prepared <-
    yaml_advanced$files$features$prepared |>
    tima:::replace_id()
  # TODO
  # yaml_advanced$files$libraries$sop$raw$closed <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$raw$ecmdb <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$raw$hmdb <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$raw$lotus <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$closed <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$ecmdb <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$hmdb <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$lotus <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$rt <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$spectral <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$keys <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$organisms$names <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$organisms$taxonomies$ott <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$stereo <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$metadata <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$names <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$taxonomies$cla <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$taxonomies$npc <-
  #   shiny::isolate(input$todo)
  yaml_advanced$files$libraries$spectral$raw <-
    shiny::isolate(input$lib_spe_mgf)
  yaml_advanced$files$libraries$temporal$exp$csv <-
    shiny::isolate(input$lib_tmp_exp_csv)
  yaml_advanced$files$libraries$temporal$is$csv <-
    shiny::isolate(input$lib_tmp_is_csv)
  # TODO
  # other relative paths, not necessary
  # TODO
  # yaml_advanced$files$libraries$temporal$prepared <-
  #   shiny::isolate(input$todo)
  yaml_advanced$files$networks$spectral$edges$raw$ms1 <-
    yaml_advanced$files$networks$spectral$edges$raw$ms1 |>
    tima:::replace_id()
  yaml_advanced$files$networks$spectral$edges$raw$spectral <-
    yaml_advanced$files$networks$spectral$edges$raw$spectral |>
    tima:::replace_id()
  yaml_advanced$files$networks$spectral$edges$prepared <-
    yaml_advanced$files$networks$spectral$edges$prepared |>
    tima:::replace_id()
  yaml_advanced$files$networks$spectral$components$raw <-
    yaml_advanced$files$networks$spectral$components$raw |>
    tima:::replace_id()
  yaml_advanced$files$networks$spectral$components$prepared <-
    yaml_advanced$files$networks$spectral$components$prepared |>
    tima:::replace_id()
  yaml_advanced$files$metadata$raw <- fil_met_raw
  yaml_advanced$files$metadata$prepared <-
    yaml_advanced$files$metadata$prepared |>
    tima:::replace_id()
  yaml_advanced$files$spectral$raw <- fil_spe_raw
  yaml_advanced$gnps$id <- gnps_job_id
  yaml_advanced$gnps$workflow <-
    shiny::isolate(input$gnps_workflow)
  yaml_advanced$ms$adducts$neg <-
    shiny::isolate(input$ms_add_neg)
  yaml_advanced$ms$adducts$pos <-
    shiny::isolate(input$ms_add_pos)
  yaml_advanced$ms$clusters$neg <-
    shiny::isolate(input$ms_clu_neg)
  yaml_advanced$ms$clusters$pos <-
    shiny::isolate(input$ms_clu_pos)
  yaml_advanced$ms$neutral_losses <-
    shiny::isolate(input$ms_neu)
  yaml_advanced$ms$polarity <- ms_pol
  yaml_advanced$ms$thresholds$ms2$intensity <-
    shiny::isolate(input$ms_thr_ms2_int)
  yaml_advanced$ms$tolerances$mass$ppm$ms1 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms1)
  yaml_advanced$ms$tolerances$mass$ppm$ms2 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms2)
  yaml_advanced$ms$tolerances$mass$dalton$ms1 <-
    shiny::isolate(input$ms_tol_mas_dal_ms1)
  yaml_advanced$ms$tolerances$mass$dalton$ms2 <-
    shiny::isolate(input$ms_tol_mas_dal_ms2)
  yaml_advanced$ms$tolerances$rt$adducts <-
    shiny::isolate(input$ms_tol_rt_add)
  yaml_advanced$ms$tolerances$rt$library <-
    shiny::isolate(input$ms_tol_rt_lib)
  yaml_advanced$names$adduct <-
    shiny::isolate(input$names_adduct)
  yaml_advanced$names$extension <-
    shiny::isolate(input$names_extension)
  yaml_advanced$names$features <-
    shiny::isolate(input$names_features)
  yaml_advanced$names$filename <-
    shiny::isolate(input$names_filename)
  yaml_advanced$names$inchikey <-
    shiny::isolate(input$names_inchikey)
  yaml_advanced$names$libraries <-
    shiny::isolate(input$names_libraries)
  yaml_advanced$names$mgf$adduct <-
    shiny::isolate(input$names_mgf_ad)
  yaml_advanced$names$mgf$collision_energy <-
    shiny::isolate(input$names_mgf_ce)
  yaml_advanced$names$mgf$compound_id <-
    shiny::isolate(input$names_mgf_ci)
  yaml_advanced$names$mgf$exact_mass <-
    shiny::isolate(input$names_mgf_em)
  yaml_advanced$names$mgf$inchi <-
    shiny::isolate(input$names_mgf_in)
  yaml_advanced$names$mgf$inchi_no_stereo <-
    shiny::isolate(input$names_mgf_io)
  yaml_advanced$names$mgf$inchikey <-
    shiny::isolate(input$names_mgf_ik)
  yaml_advanced$names$mgf$inchikey_no_stereo <-
    shiny::isolate(input$names_mgf_il)
  yaml_advanced$names$mgf$molecular_formula <-
    shiny::isolate(input$names_mgf_mf)
  yaml_advanced$names$mgf$name <-
    shiny::isolate(input$names_mgf_na)
  yaml_advanced$names$mgf$polarity <-
    shiny::isolate(input$names_mgf_po)
  yaml_advanced$names$mgf$retention_time <-
    shiny::isolate(input$names_mgf_rt)
  yaml_advanced$names$mgf$smiles <-
    shiny::isolate(input$names_mgf_sm)
  yaml_advanced$names$mgf$smiles_no_stereo <-
    shiny::isolate(input$names_mgf_sn)
  yaml_advanced$names$mgf$spectrum_id <-
    shiny::isolate(input$names_mgf_si)
  yaml_advanced$names$mgf$splash <-
    shiny::isolate(input$names_mgf_sp)
  yaml_advanced$names$mgf$synonyms <-
    shiny::isolate(input$names_mgf_sy)
  yaml_advanced$names$mgf$xlogp <-
    shiny::isolate(input$names_mgf_xl)
  yaml_advanced$names$precursor <-
    shiny::isolate(input$names_precursor)
  yaml_advanced$names$rt$features <-
    shiny::isolate(input$names_rt)
  yaml_advanced$names$rt$library <-
    shiny::isolate(input$names_rt_2)
  yaml_advanced$names$smiles <-
    shiny::isolate(input$names_smiles)
  yaml_advanced$names$source <-
    shiny::isolate(input$names_source)
  yaml_advanced$names$target <-
    shiny::isolate(input$names_target)
  yaml_advanced$names$taxon <-
    shiny::isolate(input$names_taxon)
  yaml_advanced$organisms$candidates <-
    shiny::isolate(input$org_can)
  yaml_advanced$organisms$filter$mode <-
    shiny::isolate(input$org_fil_mod)
  yaml_advanced$organisms$filter$level <-
    shiny::isolate(input$org_fil_lev)
  yaml_advanced$organisms$filter$value <-
    shiny::isolate(input$org_fil_val)
  yaml_advanced$organisms$taxon <-
    org_tax
  # TODO
  # yaml_advanced$tools$metadata <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$networks$spectral$components <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$networks$spectral$edges <-
  #   shiny::isolate(input$too_x)
  yaml_advanced$tools$sirius$version <-
    shiny::isolate(input$too_sir_ver)
  # TODO
  # yaml_advanced$tools$taxonomies$biological <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$taxonomies$chemical <-
  #   shiny::isolate(input$too_x)
  yaml_advanced$units$rt <-
    shiny::isolate(input$uni_rt)
  yaml_advanced$weights$global$biological <-
    shiny::isolate(input$wei_glo_bio)
  yaml_advanced$weights$global$chemical <-
    shiny::isolate(input$wei_glo_che)
  yaml_advanced$weights$global$spectral <-
    shiny::isolate(input$wei_glo_spe)
  yaml_advanced$weights$biological$domain <-
    shiny::isolate(input$wei_bio_01)
  yaml_advanced$weights$biological$kingdom <-
    shiny::isolate(input$wei_bio_02)
  yaml_advanced$weights$biological$phylum <-
    shiny::isolate(input$wei_bio_03)
  yaml_advanced$weights$biological$class <-
    shiny::isolate(input$wei_bio_04)
  yaml_advanced$weights$biological$order <-
    shiny::isolate(input$wei_bio_05)
  yaml_advanced$weights$biological$infraorder <-
    shiny::isolate(input$wei_bio_06)
  yaml_advanced$weights$biological$family <-
    shiny::isolate(input$wei_bio_07)
  yaml_advanced$weights$biological$subfamily <-
    shiny::isolate(input$wei_bio_08)
  yaml_advanced$weights$biological$tribe <-
    shiny::isolate(input$wei_bio_09)
  yaml_advanced$weights$biological$subtribe <-
    shiny::isolate(input$wei_bio_10)
  yaml_advanced$weights$biological$genus <-
    shiny::isolate(input$wei_bio_11)
  yaml_advanced$weights$biological$subgenus <-
    shiny::isolate(input$wei_bio_12)
  yaml_advanced$weights$biological$species <-
    shiny::isolate(input$wei_bio_13)
  yaml_advanced$weights$biological$subspecies <-
    shiny::isolate(input$wei_bio_14)
  yaml_advanced$weights$biological$variety <-
    shiny::isolate(input$wei_bio_15)
  yaml_advanced$weights$chemical$cla$kingdom <-
    shiny::isolate(input$wei_che_11)
  yaml_advanced$weights$chemical$npc$pathway <-
    shiny::isolate(input$wei_che_21)
  yaml_advanced$weights$chemical$cla$superclass <-
    shiny::isolate(input$wei_che_12)
  yaml_advanced$weights$chemical$npc$superclass <-
    shiny::isolate(input$wei_che_22)
  yaml_advanced$weights$chemical$cla$class <-
    shiny::isolate(input$wei_che_13)
  yaml_advanced$weights$chemical$npc$class <-
    shiny::isolate(input$wei_che_23)
  yaml_advanced$weights$chemical$cla$parent <-
    shiny::isolate(input$wei_che_14)
  yaml_advanced$options$compounds_names <-
    shiny::isolate(input$compounds_names)
  yaml_advanced$options$high_confidence <- hig_con
  yaml_advanced$options$force <-
    shiny::isolate(input$force)
  yaml_advanced$options$remove_ties <-
    shiny::isolate(input$remove_ties)
  yaml_advanced$options$summarise <- summarise

  if (!is.null(prefil_met_raw)) {
    yamls_params$prepare_taxa$files$metadata$raw <- fil_met_raw
  }
  if (!is.null(gnps_job_id)) {
    yamls_params$prepare_taxa$files$metadata$raw <-
      file.path(paths_data_source, paste0(gnps_job_id, "_metadata.tsv"))
  }

  yaml::write_yaml(
    x = yaml_advanced,
    file = tima:::get_default_paths()$params$prepare_params_advanced
  )
}

server <- function(input, output, session) {
  ## Observe helpers
  shinyhelper::observe_helpers()

  output$demo_spe <- downloadHandler(
    filename = basename(tima:::get_default_paths()$urls$examples$spectra),
    content = function(file) {
      writeLines(readLines(tima:::get_default_paths()$urls$examples$spectra_mini), file)
    }
  )

  output$demo_fea <- downloadHandler(
    filename = basename(tima:::get_default_paths()$urls$examples$features),
    content = function(file) {
      writeLines(readLines(tima:::get_default_paths()$urls$examples$features), file)
    }
  )

  output$demo_met <- downloadHandler(
    filename = basename(tima:::get_default_paths()$urls$examples$metadata),
    content = function(file) {
      writeLines(readLines(tima:::get_default_paths()$urls$examples$metadata), file)
    }
  )

  ## Mandatory fields
  fields_mandatory <- c("fil_fea_raw", "fil_spe_raw", "fil_pat")

  ## Enable the Submit button when all mandatory fields are filled out
  shiny::observe(x = {
    mandatory_filled <-
      vapply(
        X = fields_mandatory,
        FUN = function(x) {
          ## TODO improve
          suppressWarnings(any(!is.null(input[[x]]), input[[x]] != ""))
        },
        FUN.VALUE = logical(1)
      )
    mandatory_filled <- all(mandatory_filled)

    shinyjs::toggleState(id = "save", condition = mandatory_filled)
    shinyjs::toggleState(id = "launch", condition = input$save >= 1)
  })

  ## Special check for taxon name
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("org_tax", function(taxon) {
    if (!grepl(
      pattern = "^[[:upper:]]",
      x = taxon,
      perl = TRUE
    )) {
      "Please provide your taxon name with capital letter"
    }
  })
  iv$add_rule("org_tax", function(taxon) {
    if (any(is.na(
      stringi::stri_split_fixed(str = taxon, pattern = "|") |>
        lapply(
          FUN = function(taxon) {
            rotl::tnrs_match_names(names = taxon, do_approximate_matching = FALSE)$ott_id
          }
        )
    ))) {
      "Taxon not found in Open Tree of Life"
    }
  })
  iv$enable()

  ## When the Save button is clicked, save the response
  shiny::observeEvent(eventExpr = input$save, handlerExpr = {
    ## User-experience stuff
    shinyjs::show("save_msg")
    shinyjs::enable("launch")
    shinyjs::hide("error")
    shinyjs::hide("job_msg")
    shinyjs::hide("job_end")
    shinyjs::hide("results")
    shinyjs::hide("close")

    ## Save the data (show an error message in case of error)
    tryCatch(
      expr = {
        .save_input(input = input)
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(
          id = "error",
          anim = TRUE,
          animType = "fade"
        )
      },
      finally = {
        shinyjs::enable("save")
        shinyjs::enable("launch")
        shinyjs::hide("save_msg")
        shinyjs::hide("error")
        shinyjs::hide("job_msg")
        shinyjs::hide("job_end")
        shinyjs::hide("results")
        shinyjs::hide("close")
      }
    )
  })

  shiny::observeEvent(eventExpr = input$launch, handlerExpr = {
    shinyjs::show("job_msg")
    shinyjs::show("targets")
    shinyjs::hide("thankyou_msg")
    shinyjs::hide("error")
    shinyjs::hide("params")
    shinyjs::hide("form")
    shinyjs::hide("job_end")
    shinyjs::hide("results")
    shinyjs::hide("close")
    targets::tar_watch_server(
      id = "targets-shiny",
      exclude = c(
        "yaml_paths",
        "benchmark_ann_fil_ms1_neg",
        "benchmark_ann_fil_ms1_pos",
        "benchmark_ann_fil_spe_neg",
        "benchmark_ann_fil_spe_pos",
        "benchmark_ann_fil_spe_ms1_neg",
        "benchmark_ann_fil_spe_ms1_pos",
        "benchmark_ann_ms1_neg",
        "benchmark_ann_ms2_pos",
        "benchmark_ann_ms1_pre_neg",
        "benchmark_ann_ms1_pre_pos",
        "benchmark_ann_pre_ms1_ms2_b_c_neg",
        "benchmark_ann_pre_ms1_ms2_b_c_pos",
        "benchmark_ann_pre_ms1_ms2_b_neg",
        "benchmark_ann_pre_ms1_ms2_b_pos",
        "benchmark_ann_pre_ms2_b_c_neg",
        "benchmark_ann_pre_ms2_b_c_pos",
        "benchmark_ann_pre_ms2_b_neg",
        "benchmark_ann_pre_ms2_b_pos",
        "benchmark_ann_sir_pre",
        "benchmark_ann_sir_pre_can",
        "benchmark_ann_sir_pre_for",
        "benchmark_ann_sir_pre_str",
        "benchmark_ann_spe_neg",
        "benchmark_ann_spe_pos",
        "benchmark_ann_spe_pre_neg",
        "benchmark_ann_spe_pre_pos",
        "benchmark_com_neg",
        "benchmark_com_pos",
        "benchmark_com_pre_neg",
        "benchmark_com_pre_pos",
        "benchmark_converted",
        "benchmark_copy",
        "benchmark_def_ann_mas",
        "benchmark_def_ann_spe",
        "benchmark_def_cre_edg_com",
        "benchmark_def_cre_edg_spe",
        "benchmark_def_fil_ann",
        "benchmark_def_pre_ann_sir",
        "benchmark_def_pre_ann_spe",
        "benchmark_def_pre_fea_com",
        "benchmark_def_pre_fea_edg",
        "benchmark_def_wei_ann",
        "benchmark_edg_pre_neg",
        "benchmark_edg_pre_pos",
        "benchmark_edg_spe_neg",
        "benchmark_edg_spe_pos",
        "benchmark_file",
        "benchmark_files_neg",
        "benchmark_files_pos",
        "benchmark_path_copy",
        "benchmark_path_export",
        "benchmark_path_file",
        "benchmark_path_mgf_neg",
        "benchmark_path_mgf_pos",
        "benchmark_path_url",
        "benchmark_path_zip",
        "benchmark_prepared",
        "benchmark_pre_meta_neg",
        "benchmark_pre_meta_pos",
        "benchmark_pre_mgf_neg",
        "benchmark_pre_mgf_pos",
        "benchmark_taxed_neg",
        "benchmark_taxed_pos",
        "benchmark_wei_par",
        "benchmark_zip",
        "paths",
        "paths_data_interim_libraries_adducts_path",
        "paths_data_source_benchmark_cleaned",
        "paths_data_source_benchmark_copy",
        "paths_data_source_benchmark_mgf_neg",
        "paths_data_source_benchmark_mgf_pos",
        "paths_data_source_benchmark_set",
        "paths_data_source_benchmark_zip",
        "paths_data_source_libraries_sop_ecmdb",
        "paths_data_source_libraries_sop_hmdb",
        "paths_data_source_libraries_sop_lotus",
        "paths_data_source_libraries_spectra_is_lotus_pos",
        "paths_data_source_libraries_spectra_is_lotus_neg",
        "paths_data_source_spectra",
        # "paths_gnps_example_id",
        "paths_interim_a",
        "paths_interim_f",
        "paths_source",
        "paths_test_mode",
        "paths_urls_benchmarking_set",
        "paths_urls_ecmdb_metabolites",
        "paths_urls_hmdb_structures",
        "paths_urls_lotus_doi",
        "paths_urls_lotus_pattern",
        "paths_urls_massbank_file",
        "paths_urls_massbank_url",
        "paths_urls_massbank_version",
        "paths_urls_examples_spectra_mini",
        "paths_urls_examples_spectral_lib_pos",
        "paths_urls_examples_spectral_lib_neg",
        "par_def_ann_mas",
        "par_def_ann_spe",
        "par_def_cre_com",
        "par_def_cre_edg_spe",
        "par_def_fil_ann",
        "par_def_pre_ann_gnp",
        "par_def_pre_ann_sir",
        "par_def_pre_ann_spe",
        "par_def_pre_fea_com",
        "par_def_pre_fea_edg",
        "par_def_pre_fea_tab",
        "par_def_pre_lib_rt",
        "par_def_pre_lib_sop_clo",
        "par_def_pre_lib_sop_ecm",
        "par_def_pre_lib_sop_hmd",
        "par_def_pre_lib_sop_lot",
        "par_def_pre_lib_sop_mer",
        "par_def_pre_lib_spe",
        "par_def_pre_tax",
        "par_def_wei_ann",
        "par_fin_par",
        "par_fin_par2",
        "par_pre_par",
        "par_pre_par2",
        "par_usr_ann_mas",
        "par_usr_ann_spe",
        "par_usr_cre_com",
        "par_usr_cre_edg_spe",
        "par_usr_fil_ann",
        "par_usr_pre_ann_gnp",
        "par_usr_pre_ann_sir",
        "par_usr_pre_ann_spe",
        "par_usr_pre_fea_com",
        "par_usr_pre_fea_edg",
        "par_usr_pre_fea_tab",
        "par_usr_pre_lib_rt",
        "par_usr_pre_lib_sop_clo",
        "par_usr_pre_lib_sop_ecm",
        "par_usr_pre_lib_sop_hmd",
        "par_usr_pre_lib_sop_lot",
        "par_usr_pre_lib_sop_mer",
        "par_usr_pre_lib_spe",
        "par_usr_pre_tax",
        "par_usr_wei_ann",
        "par_ann_mas",
        "par_ann_spe",
        "par_cre_com",
        "par_cre_edg_spe",
        "par_fil_ann",
        "par_pre_ann_gnp",
        "par_pre_ann_sir",
        "par_pre_ann_spe",
        "par_pre_fea_com",
        "par_pre_fea_edg",
        "par_pre_fea_tab",
        "par_pre_lib_rt",
        "par_pre_lib_sop_clo",
        "par_pre_lib_sop_ecm",
        "par_pre_lib_sop_hmd",
        "par_pre_lib_sop_lot",
        "par_pre_lib_sop_mer",
        "par_pre_lib_spe",
        "par_pre_tax",
        "par_wei_ann",
        ".Random.seed"
      )
    )

    ## For shinylive, see
    ## https://github.com/taxonomicallyinformedannotation/tima-shinylive/pull/7
    if (R.Version()$os != "emscripten") {
      targets::tar_make(
        names = tidyselect::matches("^ann_pre$"),
        garbage_collection = TRUE,
        reporter = "verbose_positives"
      )
    } else {
      targets::tar_make(
        names = tidyselect::matches("^ann_pre$"),
        garbage_collection = TRUE,
        reporter = "verbose_positives",
        callr_function = NULL
      )
    }

    results <- tail(
      list.files(
        path = "data/processed",
        full.names = TRUE,
        recursive = TRUE
      ),
      n = 1
    )
    process <- reactiveValues(status = targets::tar_active())
    observe({
      shiny::invalidateLater(millis = 5000)
      process$status <- targets::tar_active()
      shinyjs::hide("job_msg")
      shinyjs::show("job_end")
      shinyjs::show("results")
      output$results <- downloadHandler(
        filename = basename(results),
        content = function(file) {
          writeLines(readLines(results), file)
        }
      )
      shinyjs::show("close")
      shiny::observeEvent(eventExpr = input$close, handlerExpr = {
        shiny::stopApp()
      })
    })
  })
}
url <- "<http://127.0.0.1:3838>"
message("Please, open:", url, " on your favorite browser, but not Edge.")
shiny::shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    tima:::copy_backbone()
    tima:::go_to_cache()
  }
)
