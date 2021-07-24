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

${GNVERIFIER} ../data/interim/organisms/original.tsv -s 3,4,5,6,8,9,11,12,118,128,132,147,148,150,155,158,163,164,165,167,169,174,175,179,180,187 -j 200 -f compact >../data/interim/organisms/verified.json
