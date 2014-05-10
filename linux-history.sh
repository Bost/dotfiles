#!/bin/bash -ex
# -e stop on error; -x debug

# TODO bookmars; multiple runs

mkdir -p ~/dev
lname=~/music
if [ ! -L "$lname" ]; then
    if [ -d ~/Hudba ]; then
        ln -s ~/Hudba $lname
    else
        ln -s ~/Music $lname
    fi
fi

lname=~/downloads
if [ ! -L "$lname" ]; then
    if [ -d ~/Stiahnuté ]; then
        ln -s ~/Stiahnuté $lname
    else
        ln -s ~/Downloads $lname
    fi
fi

lname=~/desktop
if [ ! -L "$lname" ]; then
    if [ -d ~/Plocha ]; then
        ln -s ~/Plocha $lname
    else
        ln -s ~/Desktop $lname
    fi
fi

lname=~/pictures
if [ ! -L "$lname" ]; then
    if [ -d ~/Obrázky ]; then
        ln -s ~/Obrázky $lname
    else
        ln -s ~/Pictures $lname
    fi
fi

# TODO multiple run
if [ 0 -eq 1 ]; then
    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
fi

# sudo wget --output-document=/etc/apt/sources.list.d/medibuntu.list \
# http://www.medibuntu.org/sources.list.d/$(lsb_release -cs).list \
# && sudo apt-get --quiet update && sudo apt-get --yes --quiet \
# --allow-unauthenticated install medibuntu-keyring \
# && sudo apt-get --quiet update

packagesBase=(
    #lirc                      # Linux Infra-red Remote Control
    ack-grep
    aptitude
#    bitcoin-qt
#    bitcoind
    chromium-browser
    curl
    emacs24
    fdupes
#    ffmpeg
#    firestarter
    gdebi                 # install .deb files under gnome
    git                   # for dotfiles
    git-gui               # for dotfiles
    gnome-system-tools
 #   google-chrome-stable
    gparted
    gstreamer1.0-libav
    gstreamer1.0-plugins-bad
    guake
    htop
    iptraf
    libav-tools
 #   libavcodec53
    lm-sensors
    mesa-utils
    mplayer
    mplayer-gui
    powertop
    ssh
    synaptic
    texinfo
    tofrodos                  # line endings
    traceroute
    tree
    ubuntu-restricted-extras
    update-manager-core
    vim-gnome
    vim-gtk
    vlc
    vlc-nox
    xfce4
    xfce4-clipman-plugin
    xfce4-cpufreq-plugin
    xfce4-cpugraph-plugin
    xfce4-goodies
    xfce4-netload-plugin
    xfce4-power-manager
    xfce4-sensors-plugin
    xfce4-systemload-plugin
    xfce4-time-out-plugin
    xfce4-xkb-plugin
    xsel                 # clipboard
    xubuntu-desktop
    youtube-dl
    # for the lexmark x2670
 #   lib32ncurses5

    # app-install-data-medibuntu
    # apport-hooks-medibuntu
    # w32codecs
    # libdvdcss2
    # libavcodec-extra-53
    # libavdevice-extra-53
    # libavfilter-extra-2
    # libavformat-extra-53
    # libavutil-extra-51
    # libpostproc-extra-52
    # libswscale-extra-2
)

packagesDev=(
    ant
    autoconf
    autogen
    automake
    ghc                  # Glasgow Haskell Compilation system
    gitg                 # alternative to gitk
    graphviz             # graph drawing tools
    libatk1.0-dev
    libbonoboui2-dev
    libcairo2-dev
    libdbus-1-dev
    libgconf2-dev
    libgif-dev
    libgnome2-dev
    libgnomeui-dev
    libgnutls-dev
    libgpm-dev
    libgtk2.0-dev
    libjpeg-dev
    libm17n-dev
    libncurses5-dev
    libotf-dev
    libpng-dev
    librsvg2-dev
    libtiff4-dev
    libx11-dev
    libxml2-dev
    libxpm-dev
    libxt-dev
    linux-headers-generic
    linux-source
    maven
    mercurial
    openjdk-7-jdk
    python-django
    python-matplotlib
    python-numpy              # in python 3 python-numpy should be already included
    subversion
    visualvm                  # Java Troubleshooting Tool
    xsane                     # scanner
)

packagesNotebook=(
    acpi
    wavemon    # Wireless Device Monitoring Application
    # cvs      # cvs:
    # xinetd   # cvs: extended Internet daemon
)

if [ "$isLinuxNew64" -gt 0 ]; then
    packages=${packagesBase[@]}" "${packagesDev[@]}
    dpkg --get-selections | grep -v deinstall > packages-isLinuxNew64.sh
elif [ "$isLinuxFranzi" -gt 0 ]; then
    packages=${packagesBase[@]}" "${packagesDev[@]}" "${packagesNotebook[@]}
    dpkg --get-selections | grep -v deinstall > packages-isLinuxFranzi.sh
else
    packages=${packagesBase[@]}
    dpkg --get-selections | grep -v deinstall > packages-isLinux.sh
fi
sudo apt-get install --yes $packages

#sudo apt-get install --reinstall nvidia-current
# or nvidia-current-updates # or nvidia-experimental-304
# echo sudo add-apt-repository ppa:gwibber-daily/ppa
#      sudo add-apt-repository ppa:gwibber-daily/ppa
# echo sudo add-apt-repository ppa:bitcoin/bitcoin
#      sudo add-apt-repository ppa:bitcoin/bitcoin
# echo sudo apt-get update
#      sudo apt-get update
#
# build-dep cannot be found

# view pdf files in chromium
#sudo ln -s /opt/google/chrome/libpdf.so /usr/lib/chromium-browser/

# svn checkout http://gmapcatcher.googlecode.com/svn/trunk gmapcatcher

# sudo sensors-detect
# sudo service module-init-tools restart
# libqt4-opengl-dev libasound2-dev timidity

# TODO compare definition of JAVA_HOME with dotfiles/bash/env
JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64

# install google-earth (gdebi is needed)
local bits=$(getconf LONG_BIT)
isLinux_64=$(($bits == 64))
isLinux_32=$(($bits == 32))

if [ $isLinux_64 ]; then
    fname=google-earth-stable_current_amd$bits.deb

    # TODO Dependency is not satisfiable: ia32-libs
    if [ 0 ] && [ ! -f "$fname" ]; then
        wget https://dl.google.com/linux/direct/$fname
        sudo gdebi $fname
    fi

    fname=google-chrome-stable_current_amd$bits.deb
    if [ ! -f "$fname" ]; then
        wget https://dl.google.com/linux/direct/$fname
        sudo dpkg -i $fname
    fi
elif [ $isLinux_32 ]; then
    # TODO install google-earth and google-chrome on 32bit linux
    :
fi

# not needed on franzi
#echo sudo apt-get remove nvidia-current # or nvidia-current-updates # or nvidia-experimental-304
#     sudo apt-get remove nvidia-current
#echo sudo apt-get install nvidia-current-updates
#     sudo apt-get install nvidia-current-updates
#echo sudo apt-get install --reinstall nvidia-current
#     sudo apt-get install --reinstall nvidia-current

# terminator - multiple terminals
# pv - view copy progress

#sox kdelibs-data thunar-archive-plugin thunar-media-tags-plugin tumbler-plugins-extra xfwm4-themes

git config --global user.name "Bost"
git config --global user.email thebost@gmail.com

# add this comp to to github knows repos
if [ ! -f ~/.ssh/id_rsa ]; then
    ssh-keygen
    # TODO put content of id_rsa.put to clipboard - see xsel
    cat ~/.ssh/id_rsa.pub
fi

# the git pull doesn't work
if [ 0 -eq 1 ]; then
    github_projects=(
        ~/dev/cheatsheet
        ~/dev/dotfiles
        ~/dev/bash-git-prompt
    )
    for project in ${github_projects[@]}; do
        if [ -d $project ]; then
            cd $project
            git pull --rebase master
        else
            projectName=$(basename $project)
            git clone git@github.com:Bost/$projectName.git
        fi
    done
    cd ~/dev/bash-git-prompt
    git remote add upstream https://github.com/magicmonty/bash-git-prompt.git
    git config user.name "Rostislav Svoboda"
    
    # TODO evaluate https://github.com/jwiegley/git-scripts.git
fi

timestamp=`date +'%Y-%m-%d_%H-%M-%S'`

lname=~/.vimrc
if [ ! -L "$lname" ]; then
    ln -s ~/dev/dotfiles/.vimrc "$lname"
fi

dname=~/.vim/bundle/vundle
echo $dname
# git clone for vundle is a workaround

# directory and link existance - see:
# http://stackoverflow.com/questions/59838/how-to-check-if-a-directory-exists-in-a-shell-script
if [ 1 -eq 0 ] && [ ! -d "$dname" ]; then
    if [ ! -L "$dname" ]; then
        git clone https://github.com/gmarik/vundle.git $dname
        # Remove empty directories in order do proceed with :BundleInstall
        dirs=(
            YankRing.vim
            ctrlp.vim
            vim-orgmode
            vim-config-python-ide
            ack.vim
            vim-matchit
            vim-powerline
            tagbar
            SearchComplete
            supertab
        )
        cd ~/dev/dotfiles/.vim/bundle && rm -rf ${dirs[@]}
    fi
fi

mv ~/.bashrc ~/.bashrc.$timestamp.backup
ln -s ~/dev/dotfiles/.bashrc ~/.bashrc

mv ~/.vim ~/.vim.$timestamp.backup
ln -s ~/dev/dotfiles/.vim ~/.vim

lname=~/.emacs
if [ ! -L "$lname" ]; then
    #mv $lname $lname.$timestamp.backup
    ln -s ~/dev/dotfiles/.emacs $lname
fi

# TODO transpose-frame.el might not be needed anymore
fname=~/.emacs.d/elpa/transpose-frame/transpose-frame.el
if [ ! -f "$fname" ]; then
    mkdir -p ~/.emacs.d/elpa/transpose-frame && cd ~/.emacs.d/elpa/transpose-frame
    wget http://www.emacswiki.org/emacs-en/download/transpose-frame.el
fi

# mkdir -p ~/bin
# cd ~/bin/ && curl -O https://raw.github.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
# chmod +x ~/bin/lein
# ~/bin/lein self-install


# TODO get current clojure-contrib jars
fname=~/dev/clojure-contrib/tools.macro-0.1.2.jar
if [ ! -f "$fname" ]; then
    mkdir -p ~/dev/clojure-contrib
    cd ~/dev/clojure-contrib/ && curl -O http://search.maven.org/remotecontent?filepath=org/clojure/tools.macro/0.1.2/tools.macro-0.1.2.jar > tools.macro-0.1.2.jar
fi

fname=~/dev/clojure-contrib/algo.monads-0.1.0.jar
if [ ! -f "$fname" ]; then
    mkdir -p ~/dev/clojure-contrib
    cd ~/dev/clojure-contrib/ && curl -O http://search.maven.org/remotecontent?filepath=org/clojure/algo.monads/0.1.0/algo.monads-0.1.0.jar > algo.monads-0.1.0.jar
fi

fname=~/dev/clojure-contrib/math.combinatorics-0.0.3.jar
if [ ! -f "$fname" ]; then
    mkdir -p ~/dev/clojure-contrib
    cd ~/dev/clojure-contrib/ && curl -O http://search.maven.org/remotecontent?filepath=org/clojure/math.combinatorics/0.0.3/math.combinatorics-0.0.3.jar > math.combinatorics-0.0.3.jar
fi

if [ 0 -eq 1 ]; then
    dname=~/dev/clojure
    echo $dname
    if [ ! -d "$dname" ]; then
        cd ~/dev && git clone https://github.com/clojure/clojure.git
    else
        cd $dname && git pull master
        echo git checkout clojure-1.6.0
             git checkout clojure-1.6.0
    fi

    fname=~/dev/clojure/clojure-1.6.0.jar
    if [ ! -f "$fname" ]; then
        $dname/antsetup.sh && ant
    fi
fi

dname=~/.vim/bundle/powerline-fonts
if [ 0 -eq 1 ] && [ ! -d "$dname" ]; then
    echo git clone git@github.com:Bost/powerline-fonts.git $dname
         git clone git@github.com:Bost/powerline-fonts.git $dname
    mkdir ~/.fonts
    cp $dname/UbuntuMono/Ubuntu\ Mono\ derivative\ Powerline.ttf ~/.fonts/
    # update font cache
    fc-cache -vf ~/.fonts
    # ./.vim/bundle/powerline/font/fontpatcher.py
fi

# Buil vim with all stuff including the +float support
if [ 0 -eq 1 ]; then
    echo build vim
    cd ~/dev
    (date && hg clone https://vim.googlecode.com/hg/ vim) 2>&1 |tee hg-vim.log
    cd ~/dev/.vim
    (date && hg pull -u) 2>&1 |tee -a ../hg-vim.log
    cd ~/dev/.vim/src
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

fname=~/.config/LightTable/settings/user.behaviors
if [ ! -f "$fname" ]; then
    mkdir -p $(dirname $fname)
    ln -s ~/dev/dotfiles/lighttable/user.behaviors $fname
fi

fname=~/.config/LightTable/settings/user.keymap
if [ ! -f "$fname" ]; then
    mkdir -p $(dirname $fname)
    ln -s ~/dev/dotfiles/lighttable/user.keymap $fname
fi

# TODO check youtube-dl current version
# curl -O https://yt-dl.org/downloads/2014.04.13/youtube-dl -o $HOME/bin/youtube-dl
# chmod a+x $HOME/bin/youtube-dl

# TODO add guake and other stuff to session
# TODO change https to git in dotfiles/.git/config

echo "dotfilesHOME=~/dev/dotfiles" > ~/.bash_profile
