#!/usr/bin/env bash

source inst/scripts/parse_yaml.sh
source inst/scripts/warning.sh

eval $(parse_yaml paths.yaml)

mkdir -p $data_source_libraries_path

wget "https://osf.io/rheq5/download" -O $data_source_libraries_lotus

