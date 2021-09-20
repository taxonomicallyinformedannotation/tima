#!/usr/bin/env bash
# -*- coding: utf-8 -*-

if [ ! -d config ]; then
  echo "Sorry, you need to run that from where your config is."
  exit 1
fi

./src/get_lotus.sh &&
python src/prepare_lotus.R &&
python src/prepare_library.R &&
python src/prepare_adducts.R &&
./src/get_example_isdb.sh &&
./src/get_gnverifier.sh &&
python src/prepare_gnps.R &&
python src/prepare_isdb.R &&
python src/prepare_edges.R &&
python src/prepare_taxa.R &&
python src/process_annotations.R
