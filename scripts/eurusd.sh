#!/bin/sh
script_dir=$(dirname $0)

cd $script_dir/../src/

runghc DataGeneration/RandomDataGenerator.hs -s "EURUSD" -p 100000 -d /Users/thlorenz/dev/data/Pricetory
