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
        href = "https://taxonomicallyinformedannotation.github.io/tima-r/",
        "following documentation"
      )
    ),
    shiny::strong(
      shiny::span("Created by "),
      shiny::a("Adriano Rutz", href = "https://adafede.github.io/"),
      shiny::HTML("&bull;"),
      shiny::a("Main publication", href = "https://doi.org/10.3389/fpls.2019.01329"),
      shiny::HTML("&bull;"),
      shiny::span("Code"),
      shiny::a(
        "on GitHub",
        href = "https://github.com/taxonomicallyinformedannotation/tima-r/"
      )
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
        shiny::h5("They SHOULD be located in `data/source`"),
        shiny::div(
          shiny::fileInput(
            inputId = "fil_spe_raw",
            label = label_mandatory("MGF file"),
            accept = ".mgf"
          )
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "The MGF file containing your spectra.",
              "Should be located in `data/source`.",
              "Reason therefore is to find it in the future.",
              "If you have a GNPS job ID, the spectra will be stored there."
            )
          ),
        shiny::fileInput(
          inputId = "fil_fea_raw",
          label = label_mandatory("Features table"),
          accept = c(
            ".csv",
            ".tsv",
            ".csv.gz",
            ".tsv.gz",
            ".csv.zip",
            ".tsv.zip"
          )
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "The csv or tsv file containing your features",
              "Should be located in `data/source`.",
              "Reason therefore is to find it in the future.",
              "If you have a GNPS job ID, the spectra will be stored there."
            )
          ),
        shiny::fileInput(
          inputId = "fil_tax_raw",
          label = "Metadata table (mandatory if no taxon name)",
          accept = c(
            ".csv",
            ".tsv",
            ".csv.gz",
            ".tsv.gz",
            ".csv.zip",
            ".tsv.zip"
          )
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "The csv or tsv file containing the metadata of your experiment.",
              "Should be located in `data/source`.",
              "Reason therefore is to find it in the future.",
              "If you have a GNPS job ID, the spectra will be stored there.",
              "Do not forget to change the `name` in the corresponding tab."
            )
          ),
        shiny::textInput(
          inputId = "fil_pat",
          label = label_mandatory("Pattern to identify your job locally"),
          value = "example"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Name of your job.",
              "All intermediate steps will use this pattern.",
              "For example, the corresponding SIRIUS results will be named:",
              "`YOUR_PATTERN_sirius`."
            )
          )
      ),
      shiny::tabPanel(
        title = "Annotations",
        shiny::h3("Annotations-related parameters"),
        shiny::sliderInput(
          inputId = "ann_can_ini",
          label = "Number of initial candidates",
          min = 1,
          max = 500,
          value = 50,
          step = 1,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Number of initial candidates to consider.",
              "For 12 candidates, with 10, only the first 10 will be kept."
            )
          ),
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
        shiny::checkboxInput(
          inputId = "ann_ms1_ann",
          label = "Perform MS1 annotation",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Options to complement previous results at the MS1 level."
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
          value = 0.6,
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
          value = 0.6,
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
              "the above thresholds set at 0.5 and 0.5 respectively",
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
          inputId = "ms_thr_ms1_int",
          label = "Intensity threshold for MS1",
          min = 0,
          max = 1E6,
          value = 1E5,
          step = 1E4,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "MS1 intensity threshold.",
              "Features below this threshold will not be annotated."
            )
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
          inputId = "ms_tol_rt_min",
          label = "Retention time tolerance in minutes",
          min = 0.01,
          max = 0.20,
          value = 0.05,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Retention time tolerance (minutes) used for adducts attribution",
              "and annotation if an experimental library is provided.",
              "If no experimental library is given, does not impact results."
            )
          ),
        shiny::checkboxGroupInput(
          inputId = "ms_add_pos",
          label = "List of adducts to be used in positive",
          choices = list(
            "[1M+(H)3]3+",
            "[1M+(H)2(Na)1]3+",
            "[1M+(H)1(Na)2]3+",
            "[1M+(Na)3]3+",
            "[1M+(H)2]2+",
            "[1M+(H)1(Na)1]2+",
            "[1M+(Mg)1]2+",
            "[1M+(H)1(K)1]2+",
            "[1M+(Ca)1]2+",
            "[1M+(Na)2]2+",
            "[1M+(Fe)1]2+",
            "[1M+(H)1]1+",
            "[1M+(Na)1]1+",
            "[1M+(K)1]1+",
            "[1M+(Cu)1]1+",
            "[2M+(Mg)1]2+",
            "[2M+(Ca)1]2+",
            "[2M+(Fe)1]2+",
            "[2M+(H)1]1+",
            "[2M+(Na)1]1+",
            "[2M+(K)1]1+"
          ),
          selected = list(
            "[1M+(H)2]2+",
            "[1M+(H)1]1+",
            "[1M+(Na)1]1+",
            "[2M+(H)1]1+"
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
                  "https://github.com/taxonomicallyinformedannotation/tima-r/issues",
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima-r/issues"
                )
              )
            )
          ),
        shiny::checkboxGroupInput(
          inputId = "ms_add_neg",
          label = "List of adducts to be used in negative",
          choices = list(
            "[1M-(H)3]3-",
            "[1M-(H)2]2-",
            "[1M-(H)1]1-",
            "[1M+(F)1]1-",
            "[1M+(Na)1-(H)2]1-",
            "[1M+(Cl)1]1-",
            "[1M+(K)1-(H)2]1-",
            "[1M+(Br)1]1-",
            "[2M-(H)1]1-",
            "[3M-(H)1]1-"
          ),
          selected = list(
            "[1M-(H)2]2-",
            "[1M-(H)1]1-",
            "[2M-(H)1]1-"
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
                  "https://github.com/taxonomicallyinformedannotation/tima-r/issues",
                  href =
                    "https://github.com/taxonomicallyinformedannotation/tima-r/issues"
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
          inputId = "names_features",
          label = "Name of \"feature IDs\" variable in the input",
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
          label = "Name of \"filename\" variable in the input",
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
          inputId = "names_precursor",
          label = "Name of \"precursor m/z\" variable in the input",
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
          label = "Name of \"retention time\" variable in the feature table",
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
          label = "Name of \"retention time\" variable in the rt library",
          value = "rt"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Name of the `retention time` column in your rt library file."
            )
          ),
        shiny::textInput(
          inputId = "names_source",
          label = "Name of \"source IDs\" variable in the input",
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
          label = "Name of \"target IDs\" variable in the input",
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
          label = "Name of \"taxon name\" variable in the input",
          value = "ATTRIBUTE_species"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c(
              "Name of the `taxon name` column in your edges file.",
              "The default corresponds to the default in GNPS."
            )
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
          )
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
      shiny::actionButton(inputId = "save", "Save parameters", class = "btn-primary"),
      shinyjs::hidden(
        shiny::span(id = "save_msg", "Saving parameters..."),
        shiny::div(
          id = "error",
          shiny::div(
            shiny::br(), shiny::tags$b("Error: "), shiny::span(id = "error_msg")
          )
        )
      ),
      shiny::actionButton(inputId = "launch", "Launch job", class = "btn-primary"),
    ),
    shinyjs::hidden(shiny::div(
      id = "thankyou_msg",
      shiny::h3("Thanks, your parameters were saved successfully!")
    )),
    shinyjs::hidden(shiny::div(
      id = "job_msg",
      shiny::h3("Job is running!")
    )),
    shinyjs::hidden(shiny::div(
      targets::tar_watch_ui(
        id = "tar_watch",
        label = "Live Show",
        targets_only = TRUE,
        degree_from = 8,
        level_separation = 300,
        display = "graph",
        displays = c("summary", "progress", "graph")
      )
    ))
  )
)
