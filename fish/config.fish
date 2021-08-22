set fish_greeting ""
# set --erase fish_greeting   # this doesn't remove the greeting?!?
# TODO https://www.wezm.net/technical/2019/10/useful-command-line-tools/
set --export dev ~/dev
set --export dec ~/dec
set --export der ~/der
set --export bin ~/bin
set --export cheat $dev/cheat
set --export dotf $dev/dotfiles
set --export virtMachines ~/virt-machines
set --export systemBinDir /usr/bin

# Some spacemacs layers require certain tools to be available on $PATH
# see http://spacemacs.org/doc/FAQ.html#setup-path
# Warning! ~/.profile ignores existence of ~/bin
# Warning! the path to guix might be wrong. In bash it should be configured as:
#    export PATH="$HOME/.guix-profile/bin${PATH:+:}$PATH"

# appending to PATH in reverse order
set PATH /usr/lib/postgresql/*/bin $PATH
set PATH /usr/racket/bin     $PATH # racket is installed manually

# Install npm packages globally without sudo on macOS and Linux
# See https://github.com/glenpike/npm-g_nosudo
set NPM_PACKAGES "$HOME/.npm-packages"
if ! test -d $NPM_PACKAGES
    mkdir $NPM_PACKAGES
end
# NODE_PATH definition might not be needed
set --export NODE_PATH ~/.config/yarn/global/node_modules
set PATH $PATH $NPM_PACKAGES/bin
set MANPATH $NPM_PACKAGES/share/man $MANPATH
npm config set prefix $NPM_PACKAGES

# rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
# See https://github.com/phiresky/ripgrep-all
set PATH ~/bin/ripgrep_all   $PATH

set PATH ~/.cabal/bin        $PATH
set PATH ~/.guix-profile/bin $PATH
set PATH ~/.yarn/bin         $PATH
set PATH $NODE_PATH/.bin $PATH
set PATH ~/.local/bin        $PATH
# anaconda installation may or may not break emacs builds
# see also the notes.fish function
# set PATH ~/anaconda3/bin     $PATH

# ~/.profile is not read if the shell is of a non-login shell type
if ! contains $bin $PATH
    set PATH $PATH $bin
end

set --local RACKET_BIN ~/.local/share/racket/8.1/bin
if test -d $RACKET_BIN
    # put the rash-repl script on the PATH
    set PATH $RACKET_BIN $PATH
end

set PATH ~/usr/local/bin $PATH

# printenv PATH

set --export GUIX_LOCPATH "$HOME/.guix-profile/lib/locale"

# JAVA_HOME definitions - see (changes require logout & login):
#     /etc/profile.d/jdk.csh
#     /etc/profile.d/jdk.sh
#     /etc/environment
#     ~/.config/fish/config.fish
# set --export JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
# set --export JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

set --export ANDROID_HOME /usr/lib/android-sdk

# GraalVM comes with: openjdk version "1.8.0_202"
set --export GRAAL_HOME ~/graalvm-ce-1.0.0-rc13
# set --export GRAAL_HOME ~/graalvm-ce-1.0.0-rc12
if test -e $GRAAL_HOME
    # set PATH $GRAAL_HOME/bin $PATH
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

set --local cljsver 0.2.187
set cljsjar $clojureDir/spec.alpha/$cljsver/spec.alpha-$cljsver.jar
# test
# java $JVM_OPTS -cp $cljjar:$cljsjar clojure.main

# -n, --line-number
# -i, --ignore-case
# -r, --recursive
# -w, --word-regexp
# set optsGrepC --color=always -nir
set optsGrepC --color=always --ignore-case --line-number --recursive
set remotes origin gitlab # used in ghog.fish, glog.fish

# see: lsblk, mount, sudo fdisk -l
# TODO usbDevice varies among machines
# set usbDevice /dev/sdg1
set usbDevice /dev/sdc1

# update PATH for the Google Cloud SDK
set --local incFile ~/google-cloud-sdk/path.fish.inc
if test -e $incFile
    . $incFile
end

# see also $dec/corona_cases/.env and $dec/corona_cases/.heroku-local.env
set --export CORONA_ENV_TYPE "devel"
# set --export BABASHKA_CLASSPATH (clojure -Sdeps '{:deps {babashka/babashka.process {:sha "6c348b5213c0c77ebbdfcf2f5da71da04afee377" :git/url "https://github.com/babashka/babashka.process"}}}' -Spath)

set --export REPL_USER $USER

set --export PATH $PATH

set --local localStuff ~/local-stuff.fish
if test -e $localStuff
    source $localStuff
# else
#     printf "No %s found\n" $localStuff
end
