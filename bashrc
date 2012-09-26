#!/bin/bash

function readFile {
	if [ -f "$1" ]; then
		. "$1"
	else
		echo "ERROR: File not found: $1"
        #exit 1
	fi
}
bashFilesHome=$dotfilesHOME/bash
if [ ! -d "$bashFilesHome" ]; then
	bashFilesHome=/h/dev/cheatsheet/dotfiles_/bash
	echo "Using old dotfiles: $bashFilesHome"
fi
readFile $bashFilesHome/env
readFile $bashFilesHome/config
readFile $bashFilesHome/aliases
