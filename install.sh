#!/bin/sh

. `dirname $0`/_config

for config in $FILES
do
  if [ ! -h ~/$config ]
  then
    echo "Linking $config ..."
    ln -s $SCRIPT_PATH/$config ~/$config
  fi
done

. `dirname $0`/_post_install
