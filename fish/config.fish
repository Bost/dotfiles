set fish_greeting "set fish_greeting \"cider-enlighten-mode\"" "neovim" "useful-command-line-tools"
# TODO https://www.wezm.net/technical/2019/10/useful-command-line-tools/
set --universal dev ~/dev
set --universal dec ~/dec
set --universal bin ~/bin
set --universal cheat $dev/cheat
set --universal dotf $dev/dotfiles

# Some spacemacs layers require certain tools to be available on $PATH
# see http://spacemacs.org/doc/FAQ.html#setup-path
# Warning! ~/.profile ignores existence of ~/bin
# Warning! the path to guix might be wrong. In bash it should be configured as:
#    export PATH="$HOME/.guix-profile/bin${PATH:+:}$PATH"

# appending to PATH in reverse order
set --export PATH ~/.cabal/bin        $PATH
set --export PATH ~/.guix-profile/bin $PATH
set --export PATH ~/.yarn/bin         $PATH
set --export PATH ~/.local/bin        $PATH
# anaconda installation may or may not break emacs builds
# see also the notes.fish function
# set --export PATH ~/anaconda3/bin     $PATH

# ~/.profile is not read if the shell is of a non-login shell type
if test ! (string match $bin $PATH)
    set --export PATH $PATH $bin
end
if test -d ~/.racket/7.5/bin
    set --export PATH ~/.racket/7.5/bin $PATH
end
# printenv PATH

set --export GUIX_LOCPATH "$HOME/.guix-profile/lib/locale"

# JAVA_HOME definitions - see (changes require logout & login):
#     /etc/profile.d/jdk.csh
#     /etc/profile.d/jdk.sh
#     /etc/environment
#     ~/.config/fish/config.fish
# set --export JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
# set --export JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

# NODE_PATH definition might not be needed
set --export NODE_PATH ~/.config/yarn/global/node_modules
set --export ANDROID_HOME /usr/lib/android-sdk

# GraalVM comes with: openjdk version "1.8.0_202"
set --export GRAAL_HOME ~/graalvm-ce-1.0.0-rc13
# set --export GRAAL_HOME ~/graalvm-ce-1.0.0-rc12
if test -e $GRAAL_HOME
    # set --export PATH $GRAAL_HOME/bin $PATH
end

# Remedy against:
# $ lein uberjar
# Release versions may not depend upon snapshots.
# Freeze snapshots to dated versions or set the LEIN_SNAPSHOTS_IN_RELEASE
# environment variable to override.
set --export LEIN_SNAPSHOTS_IN_RELEASE allowed

set --local clojureDir ~/.m2/repository/org/clojure
set --local cljver 1.10.1
set cljjar $clojureDir/clojure/$cljver/clojure-$cljver.jar

set --local cljsver 0.2.176
set cljsjar $clojureDir/spec.alpha/$cljsver/spec.alpha-$cljsver.jar

# -n, --line-number
# -i, --ignore-case
# -r, --recursive
# -w, --word-regexp
# set optsGrepC --color=always -nir
set optsGrepC --color=always --ignore-case --line-number --recursive
set remotes origin gitlab # used in ghog.fish, glog.fish

# see: lsblk, mount, sudo fdisk -l
set usbDevice /dev/sdc1 # TODO this varies among machines
