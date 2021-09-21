#!/usr/bin/env bash

source src/parse_yaml.sh
source src/warning.sh

parse_yaml paths.yaml

mkdir -p $data_source_libraries_path

wget "https://osf.io/rheq5/download" -O $data_source_libraries_lotus

