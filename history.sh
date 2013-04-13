#!/bin/bash -e

todo="### TODO: add ~/bin to the PATH"
echo ''
echo $todo
echo ''

mkdir -p ~/dev
if [ ! -L ~/downloads ]; then
    if [ -d ~/Stiahnuté ]; then
        ln -s ~/Stiahnuté ~/downloads
    else
        ln -s ~/Downloads ~/downloads
    fi
fi
if [ ! -L ~/desktop ]; then
    if [ -d ~/Plocha ]; then
        ln -s ~/Plocha ~/desktop
    else
        ln -s ~/Desktop ~/desktop
    fi
fi

echo sudo add-apt-repository ppa:bitcoin/bitcoin
     sudo add-apt-repository ppa:bitcoin/bitcoin
echo sudo apt-get update
     sudo apt-get update
echo sudo apt-get install mercurial git git-gui guake synaptic vim-gnome vim-gtk tree
     sudo apt-get install mercurial git git-gui guake synaptic vim-gnome vim-gtk tree
echo sudo apt-get install chromium-browser ant gparted bitcoind htop traceroute ssh firestarter
     sudo apt-get install chromium-browser ant gparted bitcoind htop traceroute ssh firestarter
echo sudo apt-get install gnome-system-tools curl openjdk-7-jdk maven ack-grep
     sudo apt-get install gnome-system-tools curl openjdk-7-jdk maven ack-grep
# in python 3 python-numpy should be already included
echo sudo apt-get install python-numpy python-matplotlib
     sudo apt-get install python-numpy python-matplotlib
echo xfce4 xfce4-cpugraph-plugin xfce4-clipman-plugin xfce4-netload-plugin xfce4-xkb-plugin xfce4-systemload-plugin
     xfce4 xfce4-cpugraph-plugin xfce4-clipman-plugin xfce4-netload-plugin xfce4-xkb-plugin xfce4-systemload-plugin
echo sudo apt-get install bitcoind bitcoin-qt
     sudo apt-get install bitcoind bitcoin-qt
#sox kdelibs-data thunar-archive-plugin thunar-media-tags-plugin tumbler-plugins-extra xfce4-goodies xfce4-power-manager xfwm4-themes

#echo sudo apt-get install emacs
#     sudo apt-get install emacs

# notebooks
sudo apt-get install powertop

git config --global user.name "Bost"
git config --global user.email thebost@gmail.com

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

# git clone for vundle is a workaround
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
# Remove empty directories in order do proceed with :BundleInstall
cd ~/dev/dotfiles/vim/bundle && rm -rf YankRing.vim ctrlp.vim vim-orgmode vim-config-python-ide ack.vim vim-matchit vim-powerline tagbar SearchComplete supertab

uname -a
timestamp=`date +'%Y-%m-%d_%H-%M-%S'`
mv ~/.bashrc ~/.bashrc.$timestamp.backup
ln -s ~/dev/dotfiles/bashrc ~/.bashrc

mv ~/.vim ~/.vim.$timestamp.backup
ln -s ~/dev/dotfiles/vim ~/.vim

rm -rf ~/bin/lein
mkdir -p ~/bin
cd ~/bin/ && curl -O https://raw.github.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein
~/bin/lein self-install

if [ ! -f ~/dev/clojure-contrib/tools.macro-0.1.2.jar ]; then
    mkdir -p ~/dev/clojure-contrib
    cd ~/dev/clojure-contrib/ && curl -O http://search.maven.org/remotecontent?filepath=org/clojure/tools.macro/0.1.2/tools.macro-0.1.2.jar > tools.macro-0.1.2.jar
fi
if [ ! -f ~/dev/clojure-contrib/algo.monads-0.1.0.jar ]; then
    mkdir -p ~/dev/clojure-contrib
    cd ~/dev/clojure-contrib/ && curl -O http://search.maven.org/remotecontent?filepath=org/clojure/algo.monads/0.1.0/algo.monads-0.1.0.jar > algo.monads-0.1.0.jar
fi
if [ ! -f ~/dev/clojure-contrib/math.combinatorics-0.0.3.jar ]; then
    mkdir -p ~/dev/clojure-contrib
    cd ~/dev/clojure-contrib/ && curl -O http://search.maven.org/remotecontent?filepath=org/clojure/math.combinatorics/0.0.3/math.combinatorics-0.0.3.jar > math.combinatorics-0.0.3.jar
fi
if [ ! -f ~/dev/vimclojure-server/server-2.3.6.jar ]; then
    mkdir -p ~/dev/vimclojure-server
    cd ~/dev/vimclojure-server && curl -O http://clojars.org/repo/vimclojure/server/2.3.6/server-2.3.6.jar > server-2.3.6.jar
fi

if [ ! -d ]; then
    cd ~/dev && git clone https://github.com/clojure/clojure.git
else
    cd ~/dev/clojure && git pull
    git checkout clojure-1.5.0
fi

if [ ! -f ~/dev/clojure/clojure-1.5.0.jar ]; then
    ./antsetup.sh && ant
fi

if [ ! -d ~/dev/vimclojure ]; then
    cd ~/dev/
    hg clone https://bitbucket.org/kotarak/vimclojure
    mv vimclojure/ vimclojure-nailgun-client
    cd vimclojure-nailgun-client/client/
    make
    rm -rf ~/bin/ng
    ln -s ~/dev/vimclojure-nailgun-client/client/ng ~/bin/ng
fi
rm -rf ~/bin/ng-server.sh
cp ~/dev/dotfiles/ng-server.sh ~/bin

echo ''
echo $todo
echo "$0 terminated"
