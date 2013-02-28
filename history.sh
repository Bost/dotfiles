#!/bin/bash -e

mkdir ~/dev
ln -s ~/Downloads ~/downloads
ln -s ~/Desktop ~/desktop
sudo apt-get install mercurial git git-gui guake synaptic vim-gnome vim-gtk tree chromium-browser synaptic
sudo apt-get install gparted bitcoind htop traceroute ssh firestarter
sudo apt-get install curl openjdk-7-jdk
#sudo apt-get install emacs

# notebooks
sudo apt-get install powertop


# add this comp to to github knows repos
ssh-keygen
cat ~/.ssh/id_rsa.pub
git clone git@github.com:Bost/dotfiles.git   ~/dev
git clone git@github.com:Bost/cheatsheet.git ~/dev

uname -a
mv ~/.bashrc ~/.bashrc.bak
ln -s ~/dev/dotfiles/bashrc ~/.bashrc
ln -s ~/dev/dotfiles/vim ~/.vim

# TODO remove empty directories in order do proceed with :BundleInstall
rm -rf ~/.vim/bundle/vim-orgmode

# git clone for vundle is a workaround
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

mkdir -p ~/bin && cd ~/bin/
curl -O https://raw.github.com/technomancy/leiningen/stable/bin/lein > lein
chmod +x lein
lein self-install

mkdir -p ~/dev/clojure-contrib & cd ~/dev/clojure-contrib/
curl -O http://search.maven.org/remotecontent?filepath=org/clojure/tools.macro/0.1.2/tools.macro-0.1.2.jar > tools.macro-0.1.2.jar
curl -O http://search.maven.org/remotecontent?filepath=org/clojure/algo.monads/0.1.0/algo.monads-0.1.0.jar > algo.monads-0.1.0.jar
curl -O http://search.maven.org/remotecontent?filepath=org/clojure/math.combinatorics/0.0.3/math.combinatorics-0.0.3.jar > math.combinatorics-0.0.3.jar
