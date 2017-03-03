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
plugins=(git rbenv pass virtualenv)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

# http://www.faqs.org/faqs/unix-faq/shell/zsh/#3.18
setopt nohup

CRUNCH_DIR_="${CRUNCH_DIR_COLOR} %3~\$(git_prompt_info) "
export PROMPT="$CRUNCH_RVM_$CRUNCH_DIR_$CRUNCH_PROMPT%{$reset_color%}"

alias git="nocorrect git"

alias ec='emacsclient -t'
alias ep='emacsclient -t -e "(helm-projectile)"'
alias eg="emacsclient -t -e \"(magit-status \\\"\$PWD\\\" 'switch-to-buffer)\""
alias sudo='sudo ' # support alias in sudo

if since=$(outdated-backup 2>/dev/null); then
  echo "WARNING: You need to do a backup now!"
  echo "Your last backup is ${since} seconds old."
fi

export EDITOR=vim

export LC_ALL=en_US.UTF-8

source ~/.gnupg/gpg-agent-wrapper

export PASSWORD_STORE_CLIP_TIME=5
