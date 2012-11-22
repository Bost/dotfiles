#!/bin/bash

function readFile {
    if [ -f "$1" ]; then
        . "$1"
    else
        echo "ERROR: File not found: $1"
        #exit 1
    fi
}
dotfilesHOME=~/dev/dotfiles
if [ ! -d "$dotfilesHOME" ]; then
    echo "ERROR: Directory doesn't exits: dotfilesHOME=${dotfilesHOME}"
    echo "Check the .bash_profile"
else
    bashfilesHOME=$dotfilesHOME/bash
    readFile $bashfilesHOME/env
    readFile $bashfilesHOME/config
    readFile $bashfilesHOME/aliases
fi
