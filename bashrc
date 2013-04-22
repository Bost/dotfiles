#!/bin/bash

function readFile {
    if [ -f "$1" ]; then
        . "$1"
    else
        echo "ERROR: File not found: $1"
        #exit 1
    fi
}

bashProfileLocation=$HOME/.bash_profile

if [ -z $dotfilesHOME ]; then
    source "$bashProfileLocation"
fi

if [ ! -d "$dotfilesHOME" ]; then
    echo "ERROR: Directory doesn't exits: dotfilesHOME=${dotfilesHOME}"
    echo "Check the $bashProfileLocation"
    return
else
    bashfilesHOME=$dotfilesHOME/bash
    readFile $bashfilesHOME/env
    readFile $bashfilesHOME/config
    readFile $bashfilesHOME/aliases
fi
# TODO try this
#"\e[A": history-search-backward
#"\e[B": history-search-forward
#set show-all-if-ambiguous on
#set completion-ignore-case on
