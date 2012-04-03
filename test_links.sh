#!/bin/sh

source _config

for config in $FILES
do
  if [ ! -h ~/$config ]
  then
    echo "Seems like $config is not linked"
  fi
done
