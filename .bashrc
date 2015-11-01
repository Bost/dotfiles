#!/usr/bin/env bash
# -*- coding:utf-8 -*-

# debugging:
# set -x
# stop on error:
# set -e

readFile() {
    if [ -f ${1} ]; then
        source ${1}
    else
        echo "ERROR: File not found: ${1}"
        #exit 1
    fi
}

# can't be local - local: can only be used in a function
bash_profile=${HOME}/.bash_profile

if [ -z ${dotfilesHOME} ]; then
    source ${bash_profile}
    # TODO check if ~/.bashrc executed is not executed twice
    # ~/.bashrc -> ~/.bash_profile -> ~/.bashrc
fi

if [ ! -d ${dotfilesHOME} ]; then
    echo "ERROR: Directory doesn't exits: dotfilesHOME=${dotfilesHOME}"
    echo "Check the ${bash_profile}"
    return
else
    # can't be local - local: can only be used in a function
    bash_files=${dotfilesHOME}/bash
    readFile ${bash_files}/env
    readFile ${bash_files}/aliases
fi
# TODO try history-search-backward "\e[A", history-search-forward "\e[B"
#"\e[A": history-search-backward
#"\e[B": history-search-forward
#set show-all-if-ambiguous on
#set completion-ignore-case on
export PATH=~/.local/bin:${PATH}
unset JAVA_TOOL_OPTIONS

export NVM_DIR="/home/bost/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
