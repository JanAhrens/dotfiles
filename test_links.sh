#!/bin/bash

source `dirname $0`/_config

for config in $FILES
do
  if [ ! -h ~/$config ]
  then
    echo "Seems like $config is not linked"
  fi
done
