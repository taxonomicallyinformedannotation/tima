#!/usr/bin/env bash

source scripts/parse_yaml.sh
source scripts/warning.sh

eval $(parse_yaml paths.yaml)

mkdir -p $data_interim_path
mkdir -p $data_interim_adducts_path
mkdir -p $data_interim_annotations_path
mkdir -p $data_interim_edges_path
mkdir -p $data_interim_libraries_path
mkdir -p $data_interim_taxa_path

wget "https://metabo-store.nprod.net/tima_example_files/interim/adducts_neg.tsv.gz" -O $data_interim_adducts_neg
wget "https://metabo-store.nprod.net/tima_example_files/interim/adducts_pos.tsv.gz" -O $data_interim_adducts_pos
wget "https://metabo-store.nprod.net/tima_example_files/interim/db1c51fa29a64892af520698a18783e4_edges.tsv.gz" -O data/interim/edges/db1c51fa29a64892af520698a18783e4_edges.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db1c51fa29a64892af520698a18783e4_isdb_treated.tsv.gz" -O data/interim/annotations/db1c51fa29a64892af520698a18783e4_isdb_treated.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db1c51fa29a64892af520698a18783e4_taxed.tsv.gz" -O data/interim/taxa/db1c51fa29a64892af520698a18783e4_taxed.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared_neg.tsv.gz" -O data/interim/adducts/library_neg.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared_pos.tsv.gz" -O data/interim/adducts/library_pos.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared.tsv.gz" -O data/interim/libraries/library.tsv.gz

