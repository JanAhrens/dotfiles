#!/bin/sh

source `dirname $0`/_config

for config in $FILES
do
  if [ ! -h ~/$config ]
  then
    echo "Linking $config ..."
    ln -s $SCRIPT_PATH/$config ~/$config
  fi
done

source `dirname $0/_post_install`
