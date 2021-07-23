#!/usr/bin/env bash
if [ ! -f LICENSE ]; then
  echo "Sorry, you need to run that from the root of the project."
  exit 1
fi

mkdir -p data/interim/
wget "https://metabo-store.nprod.net/tima_example_files/interim/adducts_neg.tsv.gz" -O data/interim/adducts_neg.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/adducts_pos.tsv.gz" -O data/interim/adducts_pos.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/b988e66d1542430cbc6e703be781e49c_edges.tsv.gz" -O data/interim/b988e66d1542430cbc6e703be781e49c_edges.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/b988e66d1542430cbc6e703be781e49c_isdb_treated.tsv.gz" -O data/interim/b988e66d1542430cbc6e703be781e49c_isdb_treated.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/b988e66d1542430cbc6e703be781e49c_taxed.tsv.gz" -O data/interim/b988e66d1542430cbc6e703be781e49c_taxed.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared_neg.tsv.gz" -O data/interim/db_prepared_neg.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared_pos.tsv.gz" -O data/interim/db_prepared_pos.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared.tsv.gz" -O data/interim/db_prepared.tsv.gz
