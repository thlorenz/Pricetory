#!/bin/sh

script_dir=$(dirname $0)

cd $script_dir/../src/

find . -name "*.o" -exec rm {} \;
find . -name "*.hi" -exec rm {} \;

rm -rf ../dist
