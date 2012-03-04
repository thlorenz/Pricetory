#!/bin/sh
script_dir=$(dirname $0)

cd $script_dir/../src/

runghc DataGeneration/RandomDataGenerator.hs \
  -s "EURUSD" \
  -p 31536000 \
  -d /Users/thlorenz/dev/data/Pricetory \
  -r 11
