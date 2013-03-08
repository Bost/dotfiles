#!/bin/bash -e

todo="### TODO: add ~/bin to the PATH"
echo ''
echo $todo
echo ''

mkdir -p ~/dev
if [ ! -L ~/downloads ]; then
    ln -s ~/Downloads ~/downloads
fi
if [ ! -L ~/desktop ]; then
    ln -s ~/Desktop ~/desktop
fi
sudo apt-get install mercurial git git-gui guake synaptic vim-gnome vim-gtk tree chromium-browser synaptic
sudo apt-get install gparted bitcoind htop traceroute ssh firestarter
sudo apt-get install curl openjdk-7-jdk
#sudo apt-get install emacs

# notebooks
sudo apt-get install powertop


# add this comp to to github knows repos
if [ ! -f ~/.ssh/id_rsa ]; then
    ssh-keygen
fi
cat ~/.ssh/id_rsa.pub

if [ -d ~/dev/cheatsheet ]; then
    cd ~/dev/cheatsheet && git pull
else
    git clone git@github.com:Bost/cheatsheet.git ~/dev/cheatsheet
fi
if [ -d ~/dev/dotfiles ]; then
    cd ~/dev/dotfiles && git pull
else
    git clone git@github.com:Bost/dotfiles.git ~/dev/dotfiles
fi


if [ ! -d ~/.vim/bundle/vundle ]; then
    # git clone for vundle is a workaround
    git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    # Remove empty directories in order do proceed with :BundleInstall
    cd ~/dev/dotfiles/vim/bundle && rm -rf YankRing.vim ctrlp.vim vim-orgmode vim-config-python-ide ack.vim vim-matchit vim-powerline tagbar SearchComplete supertab
fi

uname -a
timestamp=`date +'%Y-%m-%d_%H-%M-%S'`
mv ~/.bashrc ~/.bashrc.$timestamp.backup
ln -s ~/dev/dotfiles/bashrc ~/.bashrc

mv ~/.vim ~/.vim.$timestamp.backup
ln -s ~/dev/dotfiles/vim ~/.vim

rm -rf ~/bin/lein
mkdir -p ~/bin
curl -O https://raw.github.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein
~/bin/lein self-install

if [ ! -f ~/dev/clojure-contrib/tools.macro-0.1.2.jar ]; then
    mkdir -p ~/dev/clojure-contrib && cd ~/dev/clojure-contrib/
    curl -O http://search.maven.org/remotecontent?filepath=org/clojure/tools.macro/0.1.2/tools.macro-0.1.2.jar > tools.macro-0.1.2.jar
fi
if [ ! -f ~/dev/clojure-contrib/algo.monads-0.1.0.jar ]; then
    mkdir -p ~/dev/clojure-contrib && cd ~/dev/clojure-contrib/
    curl -O http://search.maven.org/remotecontent?filepath=org/clojure/algo.monads/0.1.0/algo.monads-0.1.0.jar > algo.monads-0.1.0.jar
fi
if [ ! -f ~/dev/clojure-contrib/math.combinatorics-0.0.3.jar ]; then
    mkdir -p ~/dev/clojure-contrib && cd ~/dev/clojure-contrib/
    curl -O http://search.maven.org/remotecontent?filepath=org/clojure/math.combinatorics/0.0.3/math.combinatorics-0.0.3.jar > math.combinatorics-0.0.3.jar
fi

echo ''
echo $todo
echo "$0 terminated"
