#!/usr/bin/env bash

cp -R config/default config/params
bash inst/scripts/get_lotus.sh
Rscript inst/scripts/prepare_lotus.R
Rscript inst/scripts/prepare_library.R
Rscript inst/scripts/prepare_adducts.R
bash inst/scripts/get_example_isdb.sh
bash inst/scripts/get_gnverifier.sh
Rscript inst/scripts/prepare_gnps.R
Rscript inst/scripts/prepare_isdb.R
Rscript inst/scripts/prepare_edges.R
Rscript inst/scripts/prepare_features_components.R
Rscript inst/scripts/prepare_features_classification.R
Rscript inst/scripts/prepare_taxa.R
Rscript inst/scripts/process_annotations.R
