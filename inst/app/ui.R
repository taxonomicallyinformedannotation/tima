source(file = "R/app_css.R")
source(file = "R/fields_mandatory.R")
source(file = "R/label_mandatory.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(rules = app_css),
  title = "Taxonomically Informed Metabolite Annotation",
  div(
    id = "header",
    h1("Taxonomically Informed Metabolite Annotation"),
    h4(
      "This app helps performing TIMA as described in the",
      a(
        href = "https://taxonomicallyinformedannotation.github.io/tima-r/",
        "following documentation"
      )
    ),
    strong(
      span("Created by "),
      a("Adriano Rutz", href = "https://adafede.github.io/"),
      HTML("&bull;"),
      a("Main publication", href = "https://doi.org/10.3389/fpls.2019.01329"),
      HTML("&bull;"),
      span("Code"),
      a("on GitHub", href = "https://github.com/taxonomicallyinformedannotation/tima-r/")
    )
  ),
  fluidPage(div(
    id = "params",
    navlistPanel(
      widths = c(4, 4),
      "Parameters",
      tabPanel(
        title = "Files",
        h3("Required files"),
        h5("They MUST be located in `data/source`"),
        div(
          fileInput(
            inputId = "fil_spe_raw",
            label = label_mandatory("MGF file"),
            accept = ".mgf"
          )
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        fileInput(
          inputId = "fil_fea_raw",
          label = label_mandatory("Features table"),
          accept = c(".csv", ".tsv", ".csv.gz", ".tsv.gz", ".csv.zip", ".tsv.zip")
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        fileInput(
          inputId = "fil_tax_raw",
          label = "Metadata table (mandatory if you do not provide the taxon name manually)",
          accept = c(".csv", ".tsv", ".csv.gz", ".tsv.gz", ".csv.zip", ".tsv.zip")
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        textInput(
          inputId = "fil_pat",
          label = label_mandatory("Pattern to identify your job locally"),
          value = "example"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "Annotations",
        h3("Annotations-related parameters"),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ann_can_fin",
          label = "Number of final candidates",
          min = 1,
          max = 500,
          step = 1,
          value = 3,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "ann_ms1only",
          label = "Erase MS2 results and keep MS1 only",
          value = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "ann_ms1_ann",
          label = "Perform MS1 annotation",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ann_ms1_thr_bio",
          label = "Minimal biological score to keep MS1 only annotation",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.1,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ann_ms1_thr_che",
          label = "Minimal chemical score to keep MS1 only annotation",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.1,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        selectInput(
          inputId = "ann_ms1_thr_con",
          label = "Condition to be used to retain candidates",
          choices = c("AND", "OR"),
          selected = "OR"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "ann_ms2_app",
          label = "Perform approximative matching without precursor matching? (can lead to very long processing time)",
          value = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        selectInput(
          inputId = "ann_ms2_met",
          label = "Similarity method to be used to compare spectra",
          choices = c(
            "gnps",
            "navdist",
            "ndotproduct",
            "neuclidean",
            "nspectraangle"
          ),
          selected = "gnps"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ann_ms2_thr_pea_abs",
          label = "Minimal shared peaks (absolute)",
          min = 1,
          max = 100,
          step = 1,
          value = 6,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ann_ms2_thr_pea_rat",
          label = "Minimal shared peaks (ratio)",
          min = 0.05,
          max = 1,
          step = 0.05,
          value = 0.2,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ann_ms2_thr_sim",
          label = "Minimal similarity score",
          min = 0.05,
          max = 1,
          step = 0.05,
          value = 0.2,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        selectInput(
          inputId = "ann_ms2_thr_con",
          label = "Condition to be used to retain candidates",
          choices = c("AND", "OR"),
          selected = "OR"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "MS",
        h3("MS-related parameters"),
        selectInput(
          inputId = "ms_pol",
          label = "Polarity used",
          choices = c("pos", "neg"),
          selected = "pos"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_int_thr_ms1",
          label = "Intensity threshold for MS1",
          min = 0,
          max = 1E6,
          value = 1E5,
          step = 1E4,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_int_thr_ms2",
          label = "Intensity threshold for MS2",
          min = 0,
          max = 1E4,
          value = 50,
          step = 10,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_tol_mas_ppm_ms1",
          label = "Relative mass tolerance for MS1 in ppm",
          min = 0.1,
          max = 20,
          value = 5,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_tol_mas_dal_ms1",
          label = "Absolute mass tolerance for MS1 in Dalton",
          min = 0.005,
          max = 0.02,
          value = 0.01,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_tol_mas_ppm_ms2",
          label = "Relative mass tolerance for MS2 in ppm",
          min = 0.1,
          max = 20,
          value = 5,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_tol_mas_dal_ms2",
          label = "Absolute mass tolerance for MS2 in Dalton",
          min = 0.005,
          max = 0.02,
          value = 0.01,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
          inputId = "ms_tol_rt_min",
          label = "Retention time tolerance in minutes",
          min = 0.01,
          max = 0.20,
          value = 0.05,
          ticks = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxGroupInput(
          inputId = "ms_add_pos",
          label = "List of adducts to be used in positive",
          choices = list(
            "pos_3_3proton",
            "pos_3_2proton1sodium",
            "pos_3_1proton2sodium",
            "pos_3_3sodium",
            "pos_2_2proton",
            "pos_2_2proton1ammonium",
            "pos_2_1proton1sodium",
            "pos_2_1magnesium",
            "pos_2_1proton1potassium",
            "pos_2_1calcium",
            "pos_2_2proton1acetonitrile",
            "pos_2_2sodium",
            "pos_2_1iron",
            "pos_2_2proton2acetonitrile",
            "pos_2_2proton3acetonitrile",
            "pos_1_1proton",
            "pos_1_1proton1ammonium",
            "pos_1_1sodium",
            "pos_1_minus1proton1magnesium",
            "pos_1_1proton1methanol",
            "pos_1_1potassium",
            "pos_1_minus1proton1calcium",
            "pos_1_1proton1acetonitrile",
            "pos_1_minus1proton2sodium",
            "pos_1_1proton1ethylamine",
            "pos_1_minus1proton1iron",
            "pos_1_1sodium1acetonitrile",
            "pos_1_minus1proton2potassium",
            "pos_1_1proton1dmso",
            "pos_1_1proton2acetonitrile",
            "pos_2MMg",
            "pos_2MCa",
            "pos_2MFe",
            "pos_2MH",
            "pos_2MHNH3",
            "pos_2MNa",
            "pos_2MK",
            "pos_2MHCH3CN",
            "pos_2MCH3CNNa"
          ),
          selected = list(
            "pos_2_2proton",
            "pos_1_1proton",
            "pos_1_1proton1ammonium",
            "pos_1_1sodium",
            "pos_2MH"
          )
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxGroupInput(
          inputId = "ms_add_neg",
          label = "List of adducts to be used in negative",
          choices = list(
            "neg_3_3proton",
            "neg_2_2proton",
            "neg_1_minus1proton",
            "neg_1_minus2proton1sodium",
            "neg_1_1chlorine",
            "neg_1_minus2proton1potassium",
            "neg_1_minus1proton1formic",
            "neg_1_minus1proton1acetic",
            "neg_1_minus2proton1sodium1formic",
            "neg_1_1bromine",
            "neg_1_minus1proton1tfa",
            "neg_2MH",
            "neg_2MFAH",
            "neg_2MACH",
            "neg_3MH"
          ),
          selected = list(
            "neg_2_2proton",
            "neg_1_minus1proton",
            "neg_2MH"
          )
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "Organisms",
        h3("Organisms-related parameters"),
        textInput(
          inputId = "org_tax",
          label = "OPTIONAL. Force all features to be attributed to given taxon (e.g. Gentiana lutea)",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "org_fil_mod",
          label = "Filter library to restrict it to a portion of organisms only",
          value = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        selectInput(
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
        textInput(
          inputId = "org_fil_val",
          label = "Value to be applied for filtering",
          value = "Streptophyta"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "Names",
        h3("Variable names parameters"),
        textInput(
          inputId = "names_features",
          label = "Name of \"feature IDs\" variable in the input",
          value = "row ID"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "names_extension",
          label = "The file(s) extension is present in the sample name",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        textInput(
          inputId = "names_precursor",
          label = "Name of \"precursor m/z\" variable in the input",
          value = "row m/z"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        textInput(
          inputId = "names_rt",
          label = "Name of \"retention time\" variable in the input",
          value = "row retention time"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        textInput(
          inputId = "names_source",
          label = "Name of \"source IDs\" variable in the input",
          value = "CLUSTERID1"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        textInput(
          inputId = "names_target",
          label = "Name of \"target IDs\" variable in the input",
          value = "CLUSTERID2"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        textInput(
          inputId = "names_taxon",
          label = "Name of \"taxon name\" variable in the input",
          value = "ATTRIBUTE_species"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "Weights",
        h3("Weights-related parameters"),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          ),
        sliderInput(
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
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "Options",
        h3("Options parameters"),
        checkboxInput(
          inputId = "fast",
          label = "Skip time-consuming steps",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "force",
          label = "Do not use it!",
          value = FALSE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "parallel",
          label = "Execute processes in parallel when available",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        checkboxInput(
          inputId = "summarise",
          label = "Summarise results to one row per feature",
          value = TRUE
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      ),
      tabPanel(
        title = "GNPS (optional)",
        h3("GNPS parameters"),
        textInput(
          inputId = "gnps_id",
          label = "GNPS job ID",
          value = NULL
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          ),
        selectInput(
          inputId = "gnps_workflow",
          label = "Workflow used within GNPS",
          choices = c("classical", "fbmn"),
          selected = "fbmn"
        ) |>
          shinyhelper::helper(
            type = "inline",
            content = c("Info soon available")
          )
      )
    )
  )),
  fluidPage(
    div(
      id = "form",
      actionButton(inputId = "save", "Save parameters", class = "btn-primary"),
      shinyjs::hidden(
        span(id = "save_msg", "Saving parameters..."),
        div(
          id = "error",
          div(
            br(), tags$b("Error: "), span(id = "error_msg")
          )
        )
      ),
      actionButton(inputId = "launch", "Launch job", class = "btn-primary"),
    ),
    shinyjs::hidden(div(
      id = "thankyou_msg",
      h3("Thanks, your parameters were saved successfully!")
    )),
    shinyjs::hidden(div(
      id = "job_msg",
      h3("Job is running!")
    )),
    shinyjs::hidden(div(
      targets::tar_watch_ui(
        id = "tar_watch",
        label = "Live Show",
        targets_only = TRUE,
        degree_from = 10,
        display = "graph",
        displays = c("summary", "graph", "about")
      )
    ))
  )
)
