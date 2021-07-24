#!/usr/bin/env bash
# -*- coding: utf-8 -*-

if [ -f ../bin/gnverifier ]; then
  GNVERIFIER=../bin/gnverifier
elif [ -f /usr/bin/gnverifier ]; then
  GNVERIFIER=/usr/bin/gnverifier
else
  echo "Sorry you need to install gnverifier in /usr/bin or ../bin"
  exit 1
fi

${GNVERIFIER} ../data/interim/organisms/original_2.tsv -s 179 -j 200 -f compact >../data/interim/organisms/verified_2.json
