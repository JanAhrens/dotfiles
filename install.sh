#!/bin/sh

source ~/dotfiles/_config

for config in $FILES
do
  if [ ! -h ~/$config ]
  then
    echo "Linking $config ..."
    ln -s ~/dotfiles/$config ~/$config
  fi
done
