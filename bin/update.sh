#!/bin/sh

# inspired from the oh-my-zsh update mechanism from
# https://github.com/robbyrussell/oh-my-zsh/blob/master/tools/check_for_upgrade.sh

export UPDATE_FILE="$HOME/.dotfiles-update"

function _current_epoch() {
  echo $(($(date +%s) / 60 / 60 / 24))
}

function _update_dotfiles_update() {
  echo "LAST_EPOCH=$(_current_epoch)" > $UPDATE_FILE
}

function _upgrade_dotfiles() {
  LAST_DIR=$PWD
  cd $HOME/dotfiles

  printf '\033[0;34m%s\033[0m\n' "Upgrading dotfiles"

  git fetch origin
  git lg master..origin/master
  if git merge origin/master; then
    printf '\033[0;34m%s\033[0m\n' 'Your dotfiles have been updated'
  else
    printf '\033[0;31m%s\033[0m\n' 'Update failed'
  fi

  _update_dotfiles_update
  cd $LAST_DIR
}

if [ -f $UPDATE_FILE ]; then

  . $UPDATE_FILE

  if [[ -z "$LAST_EPOCH" ]]; then
    _update_dotfiles_update && exit 0;
  fi

  epoch_diff=$(($(_current_epoch) - $LAST_EPOCH))
  if [ $epoch_diff -gt 3 ]
  then
    _upgrade_dotfiles
  fi

fi
