#!/bin/bash

# TODO it may be better to rewrite this script in python

if [ "$isMinGw32_155" -gt 0 ]; then
    vh=/c/cygwin/home/svo02896/vimfiles
elif [ -d ~/.vim ]; then
    vh=~/.vim
elif [ -d ~/vimfiles ]; then
    vh=~/vimfiles
else
    echo "ERROR: No VIM_HOME found"
    exit 1
fi

i=0
pName[$i]="vim-session"
pUrl_[$i]="https://github.com/xolox/vim-session.git"
i=$i+1

pName[$i]="VimOrganizer"
pUrl_[$i]="https://github.com/hsitz/VimOrganizer.git"
i=$i+1

pName[$i]="csv.vim"
pUrl_[$i]="https://github.com/vim-scripts/csv.vim.git"
i=$i+1

#switch between buffers by using the one of the default public interfaces:
#  '\be' (normal open)  or
#  '\bs' (force horizontal split open)  or
#  '\bv' (force vertical split open)
pName[$i]="bufexplorer"
pUrl_[$i]="https://github.com/c9s/bufexplorer.git"
i=$i+1

if [ "$isLinux" -gt 0 ]; then
    # lusty requires ruby - at first I test it under linux
    pName[$i]="lusty"
    pUrl_[$i]="https://github.com/sjbach/lusty.git"
    i=$i+1
fi

pName[$i]="nerdcommenter"
pUrl_[$i]="https://github.com/scrooloose/nerdcommenter.git"
i=$i+1

pName[$i]="nerdtree"
pUrl_[$i]="https://github.com/scrooloose/nerdtree.git"
i=$i+1

pName[$i]="VimClojure"
pUrl_[$i]="https://github.com/vim-scripts/VimClojure.git"
i=$i+1

pName[$i]="vim-fugitive"
pUrl_[$i]="https://github.com/tpope/vim-fugitive.git"
i=$i+1

pName[$i]="vim-repeat"
pUrl_[$i]="https://github.com/tpope/vim-repeat.git"
i=$i+1

pName[$i]="vim-tail"
pUrl_[$i]="https://github.com/tony/vim-tail.git"
i=$i+1

pName[$i]="vim-unimpaired"
pUrl_[$i]="https://github.com/tpope/vim-unimpaired.git"
i=$i+1

pName[$i]="YankRing.vim"
pUrl_[$i]="https://github.com/vim-scripts/YankRing.vim.git"
i=$i+1

pName[$i]="gundo.vim"
pUrl_[$i]="http://github.com/sjl/gundo.vim.git"
i=$i+1

pName[$i]="minibufexpl.vim"
pUrl_[$i]="https://github.com/fholgado/minibufexpl.vim.git"
i=$i+1

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

