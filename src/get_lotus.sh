#!/usr/bin/env bash
# -*- coding: utf-8 -*-

source src/parse_yaml.sh
source src/warning.sh

eval $(parse_yaml src/paths.yaml)

mkdir -p $data_source_libraries_path

wget "https://osf.io/rheq5/download" -O $data_source_libraries_lotus
