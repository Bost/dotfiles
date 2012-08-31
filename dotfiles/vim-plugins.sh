#!/bin/bash

# TODO it may be better to rewrite this script in python

if [ -d ~/.vim ]; then
    vh=~/.vim
elif [ -d ~/vimfiles ]; then
    vh=~/vimfiles
else
    echo "ERROR: No VIM_HOME found"
    exit 1
fi


pName[0]="bufexplorer"
pUrl_[0]="https://github.com/c9s/bufexplorer.git"

pName[1]="nerdcommenter"
pUrl_[1]="https://github.com/scrooloose/nerdcommenter.git"

pName[2]="nerdtree"
pUrl_[2]="https://github.com/scrooloose/nerdtree.git"

pName[3]="VimClojure"
pUrl_[3]="https://github.com/vim-scripts/VimClojure.git"

pName[4]="vim-fugitive"
pUrl_[4]="https://github.com/tpope/vim-fugitive.git"

pName[5]="vim-repeat"
pUrl_[5]="https://github.com/tpope/vim-repeat.git"

pName[6]="vim-tail"
pUrl_[6]="https://github.com/tony/vim-tail.git"

pName[7]="vim-unimpaired"
pUrl_[7]="https://github.com/tpope/vim-unimpaired.git"


size=${#pName[*]}
size=$(( $size - 1))

for (( i=0; i<= $size; i++ ))
do
    name=${pName[$i]}
    url=${pUrl_[$i]}

    echo $vh/bundle/$name:
    if [ -d $vh/bundle/$name ]; then
        cd $vh/bundle/$name && git pull
    else
        cd $vh/bundle && git clone $url
    fi
done

