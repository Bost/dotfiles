#!/usr/bin/env bash

# debugging:
# set -x
# stop on error:
# set -e

function readFile {
    if [ -f "$1" ]; then
        . "$1"
    else
        echo "ERROR: File not found: $1"
        #exit 1
    fi
}

bash_profile=$HOME/.bash_profile

if [ -z $dotfilesHOME ]; then
    source "$bash_profile"
    # TODO check if ~/.bashrc executed is not executed twice
    # ~/.bashrc -> ~/.bash_profile -> ~/.bashrc
fi

if [ ! -d "$dotfilesHOME" ]; then
    echo "ERROR: Directory doesn't exits: dotfilesHOME=${dotfilesHOME}"
    echo "Check the $bash_profile"
    return
else
    bash_files=$dotfilesHOME/bash
    readFile $bash_files/env
    readFile $bash_files/config
    readFile $bash_files/aliases
    readFile $bash_files/fix-dir-struct
fi
# TODO try history-search-backward "\e[A", history-search-forward "\e[B"
#"\e[A": history-search-backward
#"\e[B": history-search-forward
#set show-all-if-ambiguous on
#set completion-ignore-case on
