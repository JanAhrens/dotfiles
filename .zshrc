source ~/.profile
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="crunch"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rbenv)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# http://www.faqs.org/faqs/unix-faq/shell/zsh/#3.18
setopt nohup

# who needs a clock? keep on smiling! ;-)
export RPROMPT=":-)"

alias git="nocorrect git"

alias be='bundle exec'
alias bi='bundle install'
alias ec='emacsclient'

alias ssh="ssh-add -l >/dev/null || ssh-add -t 10m; \ssh"
alias xing="ssh-add -l >/dev/null || ssh-add -t 10m; \xing"

git_wrapper() {
  if [[ "$1" = "push" || "$1" = "pull" || "$1" = "fetch" || "$1" = "clone" ]]; then
    ssh-add -l >/dev/null || ssh-add -t 10m
  fi

  \git $@
}

alias git=git_wrapper

if which boot2docker > /dev/null; then
  if [[ $(boot2docker status) = "running" ]]; then
    eval $(boot2docker shellinit 2>/dev/null)
  else
    echo "WARNING: boot2docker is not running. Not setting up the environment variables."
  fi
fi

if since=$(outdated-backup 2>/dev/null); then
  echo "WARNING: You need to do a backup now!"
  echo "Your last backup is ${since} seconds old."
fi

up_since_days() {
  if [[ $(uname -s) == "Darwin" ]]; then
    return 1
  fi
  uptime=$(uptime -p)
  # https://gitorious.org/procps/procps/source/3a66fba1e934cbd830df572d8d03c05b4f4a5f1e:proc/whattime.c#L78-84
  [[ $(echo "${uptime}" | grep day | cut -d' ' -f2) -gt 2 ]] && echo "${uptime}"
}

if output=$(up_since_days); then
  echo "WARNING: This machine is running since ${output} already"
fi

export EDITOR=emacsclient
