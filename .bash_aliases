#!/bin/bash

#MBS_HOME=~/mbs
#alias mce='cd $MBS_HOME/mce'
#alias dem='cd $MBS_HOME/dem'
#alias std='cd $MBS_HOME/std'
#alias bmw='cd $MBS_HOME/bmw'
#alias mbb='cd $MBS_HOME/mbb'
#alias all='cd $MBS_HOME/all.git'

SUDO_UPDATE="sudo apt-get update"
alias ll="ls -la"
alias update=$SUDO_UPDATE
alias upd=$SUDO_UPDATE
alias inst="sudo apt-get install"

SUDO_UPGRADE="sudo apt-get upgrade"
alias upgrade=$SUDO_UPGRADE
alias upg=$SUDO_UPGRADE

alias aliases='vim ~/.bash_aliases'
alias reload='echo "reloading ~/.bashrc"; source ~/.bashrc'

alias v='vim'
alias g='gvim'
alias c='cat'

# bash aliases for git
alias gst='git status'
alias gg='git gui &'

GIT_ADD_ALL="echo 'git add .'; git add ."
alias ga=$GIT_ADD_ALL
alias gadd=$GIT_ADD_ALL
alias gdd=$GIT_ADD_ALL
