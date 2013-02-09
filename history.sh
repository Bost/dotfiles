#!/bin/bash -e

mkdir ~/dev
ln -s ~/Downloads ~/downloads
ln -s ~/Desktop ~/desktop
sudo apt-get install git git-gui guake synaptic vim-gnome vim-gtk tree chromium-browser synaptic
sudo apt-get install gparted bitcoind
sudo apt-get install curl openjdk-7-jdk
#sudo apt-get install emacs

# add this comp to to github knows repos
ssh-keygen
cat ~/.ssh/id_rsa.pub
git clone git@github.com:Bost/dotfiles.git   ~/dev
git clone git@github.com:Bost/cheatsheet.git ~/dev

# git clone for vundle is a workaround
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

uname -a
mv ~/.bashrc ~/.bashrc.bak
ln -s ~/dev/dotfiles/bashrc ~/.bashrc
ln -s ~/dev/dotfiles/vim ~/.vim

mkdir ~/bin
cd ~/bin/
curl -O https://raw.github.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein
lein self-install

