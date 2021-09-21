#!/usr/bin/env bash

source src/parse_yaml.sh
source src/warning.sh

parse_yaml paths.yaml

mkdir -p $data_interim_path
mkdir -p $data_interim_annotations_path

wget "https://metabo-store.nprod.net/tima_example_files/interim/example_isdb_result.tsv.gz" -O data/interim/annotations/example_isdb_result.tsv.gz

