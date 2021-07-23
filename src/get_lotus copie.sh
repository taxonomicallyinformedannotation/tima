#!/usr/bin/env bash
# -*- coding: utf-8 -*-

source src/parse_yaml.sh

if [ ! -f LICENSE ]; then
  echo "Sorry, you need to run that from the root of the project."
  exit 1
fi

eval $(parse_yaml src/paths.yaml)

mkdir -p $data_source_libraries_path
wget "https://osf.io/rheq5/download" -O $data_source_libraries_lotus
