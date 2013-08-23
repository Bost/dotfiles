#!/bin/bash -e

# TODO bookmars; multiple runs

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

# a bugfix for LightTable
sudo ln -sf /lib/i386-linux-gnu/libudev.so.1 /lib/i386-linux-gnu/libudev.so.0


wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'


# sudo wget --output-document=/etc/apt/sources.list.d/medibuntu.list \
# http://www.medibuntu.org/sources.list.d/$(lsb_release -cs).list \
# && sudo apt-get --quiet update && sudo apt-get --yes --quiet \
# --allow-unauthenticated install medibuntu-keyring \
# && sudo apt-get --quiet update

# sudo apt-get install app-install-data-medibuntu apport-hooks-medibuntu
# sudo apt-get install w32codecs libdvdcss2
# sudo apt-get install libavcodec-extra-53 libavdevice-extra-53
# libavfilter-extra-2 libavformat-extra-53 libavutil-extra-51
# libpostproc-extra-52 libswscale-extra-2

#sudo apt-get install --reinstall nvidia-current
# or nvidia-current-updates # or nvidia-experimental-304
# echo sudo add-apt-repository ppa:gwibber-daily/ppa
#      sudo add-apt-repository ppa:gwibber-daily/ppa
# echo sudo add-apt-repository ppa:bitcoin/bitcoin
#      sudo add-apt-repository ppa:bitcoin/bitcoin
# echo sudo apt-get update
#      sudo apt-get update
#
# # in python 3 python-numpy should be already included
# # notebooks - powertop
#
# build-dep cannot be found sudo apt-get install mercurial git git-gui
# guake synaptic vim-gnome lm-sensors vim-gtk tree tofrodos
# chromium-browser ant gparted bitcoind htop traceroute ssh
# firestarter gnome-system-tools curl openjdk-7-jdk maven ack-grep
# automake autoconf python-numpy python-matplotlib xfce4
# xfce4-cpugraph-plugin xfce4-clipman-plugin xfce4-netload-plugin
# xfce4-xkb-plugin xfce4-systemload-plugin xfce4-sensors-plugin
# xfce4-goodies xfce4-power-manager xubuntu-desktop bitcoind
# bitcoin-qt graphviz visualvm libncurses5-dev libgnome2-dev
# libgnomeui-dev libgtk2.0-dev libatk1.0-dev libbonoboui2-dev
# libcairo2-dev libx11-dev libxpm-dev libxt-dev update-manager-core
# linux-source linux-headers-generic powertop emacs24 ghc6
# google-chrome-stable python-django gdebi acpi automake autogen
# autoconf ssh texinfo libncurses5-dev libgtk2.0-dev libgif-dev
# libjpeg-dev libpng-dev libxpm-dev libtiff4-dev libxml2-dev
# librsvg2-dev libotf-dev libm17n-dev libgpm-dev libgnutls-dev
# libgconf2-dev libdbus-1-dev youtube-dl ffmpeg mplayer mplayer-gui
# gstreamer1.0-libav gstreamer1.0-plugins-bad vlc-nox
# ubuntu-restricted-extras libavcodec53 lirc
# aptitude xfce4-cpufreq-plugin


# only for wifi-enabled devices (laptop etc.)
# sudo apt-get install wavemon




# sudo sensors-detect
# sudo service module-init-tools restart
# libqt4-opengl-dev libasound2-dev timidity

# TODO compare definition of JAVA_HOME with dotfiles/bash/env
JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/

# emacs configuration:
# mkdir ~/.emacs.d/elpa/transpose-frame && cd ~/.emacs.d/elpa/transpose-frame
wget http://www.emacswiki.org/emacs-en/download/transpose-frame.el
echo "(add-to-list \'load-path \"~/.emacs.d/elpa/transpose-frame/\")" >> ~/.emacs
echo "(require \'transpose-frame)" >> ~/.emacs
#
# emacs packages:
#    ace-jump-mode
#    ack
#    ack-and-a-half
*#    align-cljlet - swap
*#    ack-menu
#    auto-complete
#    bf-mode
#    browse-kill-ring
#    bs-ext
#    clj-refactor
#    cljdoc
#    cljsbuild-mode
#    clojure-mode
#    clojure-project-mode-1.0
#    clojure-snippets
#    clojure-test-mode
#    clojurescript-mode
#    closure-lint-mode
#    closure-templat...
*#    color-theme  - missing
#    csv-mode
#    csv-nav
#    dash
#    dash-at-point
#    dash-functional
#    dircmp
#    dired+
#    elisp-slime-nav
#    evil
#    evil-leader
#    evil-nerd-commenter
#    evil-numbers
*#    evil-paredit - try out
*#    find-file-in-project - probably not needed because of helm
#    google-maps
#    google-this
#    google-weather
#    helm
*#   try out other helm stuff
#    idle-highlight-mode
#    ido-ubiquitous
*#   ido-vertical-mode - try out
#    latest-clojars
*#   lein - (eshell interface for leiningen) try out
#    levenshtein
#    linum-relative
#    mag-menu
#    magit
#    magit-push-remote
#    magithub
#    mode-icons
#    move-text
#    multiple-cursors
#    nrepl
#    org
#    paredit
#    paredit-mode
#    popup
*#    project  - probably too old
#    project-mode
#    redo+
#    s
#    smex
#    splitter
#    workgroups2
#    undo-tree
*#    yasnippet - probably not needed

# install google-earth (gdebi is needed)
wget https://dl.google.com/linux/direct/google-earth-stable_current_amd64.deb
sudo gdebi google-earth-stable_current_amd64.deb
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb


# not needed on franzi
#echo sudo apt-get remove nvidia-current # or nvidia-current-updates # or nvidia-experimental-304
#     sudo apt-get remove nvidia-current
#echo sudo apt-get install nvidia-current-updates
#     sudo apt-get install nvidia-current-updates
##echo sudo apt-get install --reinstall nvidia-current
##     sudo apt-get install --reinstall nvidia-current

# terminator - multiple terminals
# pv - view copy progress

#sox kdelibs-data thunar-archive-plugin thunar-media-tags-plugin tumbler-plugins-extra xfwm4-themes

git config --global user.name "Bost"
git config --global user.email thebost@gmail.com

# add this comp to to github knows repos
if [ ! -f ~/.ssh/id_rsa ]; then
    ssh-keygen
    cat ~/.ssh/id_rsa.pub
fi

# the git pull doesn't work
# if [ -d ~/dev/cheatsheet ]; then
#     cd ~/dev/cheatsheet && git pull master
# else
#     echo git clone git@github.com:Bost/cheatsheet.git ~/dev/cheatsheet
#          git clone git@github.com:Bost/cheatsheet.git ~/dev/cheatsheet
# fi
# if [ -d ~/dev/dotfiles ]; then
#     cd ~/dev/dotfiles && git pull master
# else
#     echo git clone git@github.com:Bost/dotfiles.git ~/dev/dotfiles
#          git clone git@github.com:Bost/dotfiles.git ~/dev/dotfiles
# fi

if [ ! -f ~/dev/dotfiles/vimrc ]; then
    ln -s ~/dev/dotfiles/vimrc ~/.vimrc
fi

echo ~/.vim/bundle/vundle
# git clone for vundle is a workaround
if [ ! -d ~/.vim/bundle/vundle ]; then
    echo git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
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

# rm -rf ~/bin/lein
# mkdir -p ~/bin
# cd ~/bin/ && curl -O https://raw.github.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
# chmod +x ~/bin/lein
# ~/bin/lein self-install

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

echo ~/dev/clojure
# if [ ! -d ~/dev/clojure ]; then
#     cd ~/dev && git clone https://github.com/clojure/clojure.git
# else
#     cd ~/dev/clojure && git pull master
#     echo git checkout clojure-1.5.1
#          git checkout clojure-1.5.1
# fi

# if [ ! -f ~/dev/clojure/clojure-1.5.1.jar ]; then
#     ~/dev/clojure/antsetup.sh && ant
# fi

echo ~/dev/vimclojure
# if [ ! -d ~/dev/vimclojure ]; then
#     cd ~/dev/
#     echo hg clone https://bitbucket.org/kotarak/vimclojure
#          hg clone https://bitbucket.org/kotarak/vimclojure
#     mv vimclojure/ vimclojure-nailgun-client
#     cd vimclojure-nailgun-client/client/
#     make
#     rm -rf ~/bin/ng
#     ln -s ~/dev/vimclojure-nailgun-client/client/ng ~/bin/ng
# fi
rm -rf ~/bin/ng-server.sh
cp ~/dev/dotfiles/ng-server.sh ~/bin


if [ ! -d ~/.vim/bundle/powerline-fonts ]; then
    echo git clone git@github.com:Bost/powerline-fonts.git ~/.vim/bundle/powerline-fonts
         git clone git@github.com:Bost/powerline-fonts.git ~/.vim/bundle/powerline-fonts
    mkdir ~/.fonts
    cp ~/.vim/bundle/powerline-fonts/UbuntuMono/Ubuntu\ Mono\ derivative\ Powerline.ttf ~/.fonts/
    # update font cache
    fc-cache -vf ~/.fonts
    # ./vim/bundle/powerline/font/fontpatcher.py
fi

echo build vim
# Buil vim with all stuff including the +float support
if [ 0 -eq 1 ]; then
    cd ~/dev
    (date && hg clone https://vim.googlecode.com/hg/ vim) 2>&1 |tee hg-vim.log
    cd ~/dev/vim
    (date && hg pull -u) 2>&1 |tee -a ../hg-vim.log
    cd ~/dev/vim/src
    make distclean
    #./configure --with-features=huge --enable-gui=gnome2
    #./configure --with-features=huge --enable-gui=gtk2
    ./configure --with-features=huge --enable-rubyinterp=yes --enable-pythoninterp=yes --enable-gui=gtk2
    #./configure --with-features=huge --enable-gui=gnome2 --enable-pythoninterp --with-python-config-dir=/usr/lib/python2.7/config
    make  # make -j 8
    sudo make install
fi

echo sudo apt-get autoremove
     sudo apt-get autoremove

echo ''
echo $todo
echo "$0 terminated"
