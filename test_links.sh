#!/bin/sh

ignore_files_regex='(README.md|test_links.sh)'
files=$(ls -1a | grep -Ev $ignore_files_regex | grep -Ev '^.(.?|git(modules|ignore)?)$')

for config in $files
do
  if [ ! -h ~/$config ]
  then
    echo "Seems like $config is not linked"
  fi
done
