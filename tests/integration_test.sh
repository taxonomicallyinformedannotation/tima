#!/usr/bin/env bash
# -*- coding: utf-8 -*-

if [ ! -d config ]; then
  echo "Sorry, you need to run that from where your config is."
  exit 1
fi

bash src/get_lotus.sh &&
Rscript src/prepare_lotus.R &&
Rscript src/prepare_library.R &&
Rscript src/prepare_adducts.R &&
bash src/get_example_isdb.sh &&
bash src/get_gnverifier.sh &&
Rscript src/prepare_gnps.R &&
Rscript src/prepare_isdb.R &&
Rscript src/prepare_edges.R &&
Rscript src/prepare_features_components.R &&
Rscript src/prepare_features_classification.R &&
Rscript src/prepare_taxa.R &&
Rscript src/process_annotations.R
