set fish_greeting ""

# Reset the fish-shell back to it's initial state:
# rm -rf ~/.config/fish/completions/
# rm -rf ~/.config/fish/conf.d/
# rm -rf ~/.config/fish/fish_plugins
# rm -rf ~/.config/fish/fish_variables
# rm -rf ~/.config/fish/functions/fisher.fish
# rm -rf ~/.config/fish/functions/tide.fish
# rm -rf ~/.config/fish/functions/tide/
# rm -rf ~/.config/fish/functions/fish_mode_prompt.fish
# rm -rf ~/.config/fish/functions/fish_prompt.fish
# rm -rf ~/.config/fish/functions/_tide_*

# set --erase fish_greeting   # this doesn't remove the greeting?!?
# TODO https://www.wezm.net/technical/2019/10/useful-command-line-tools/
set --export dev ~/dev
set --export dec ~/dec
set --export der ~/der
set --export bin ~/bin
set --export cheat $dev/cheat
set --export dotf $dev/dotfiles
set --export virtMachines ~/virt-machines

# Some spacemacs layers require certain tools to be available on $PATH
# see http://spacemacs.org/doc/FAQ.html#setup-path
# Warning! ~/.profile ignores existence of ~/bin
# Warning! the path to guix might be wrong. In bash it should be configured as:
#    export PATH="$HOME/.guix-profile/bin${PATH:+:}$PATH"

set --export GUIX_PROFILE ~/.guix-home/profile

# appending to PATH in reverse order
set PATH /usr/lib/postgresql/*/bin $PATH

if test (which npm 2> /dev/null)
    # Install npm packages globally without sudo on macOS and Linux
    # See https://github.com/glenpike/npm-g_nosudo
    set NPM_PACKAGES ~/.npm-packages
    if ! test -d $NPM_PACKAGES
        mkdir $NPM_PACKAGES
    end
    # NODE_PATH definition might not be needed
    # set --export NODE_PATH ~/.config/yarn/global/node_modules
    set PATH $PATH $NPM_PACKAGES/bin
    set PATH $NODE_PATH/.bin $PATH
    set MANPATH $NPM_PACKAGES/share/man $MANPATH
    npm config set prefix $NPM_PACKAGES
end

# rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
# See https://github.com/phiresky/ripgrep-all
# set PATH ~/bin/ripgrep_all   $PATH

# set PATH ~/.cabal/bin        $PATH
# set PATH ~/.guix-profile/bin $PATH
# set PATH ~/.yarn/bin         $PATH
# set PATH ~/.local/bin        $PATH
# for script-based installations of babashka, heroku, clojure
set PATH  /usr/local/bin     $PATH
# anaconda installation may or may not break emacs builds
# see also the notes.fish function
# set PATH ~/anaconda3/bin     $PATH

# ~/.profile is not read if the shell is of a non-login shell type
test ! (contains $bin $PATH) && set PATH $PATH $bin

# set --local racketShare ~/.local/share/racket
# if test -d $racketShare # may not exist under `guix shell`
#     # use latest racket version
#     set --local latest (ls -t $racketShare | grep "[[:digit:]].*" | head -1)
#     set --local racketBin $racketShare/$latest/bin
#     if test -d $racketBin
#         # put scripts installed by raco on the PATH
#         set PATH $racketBin $PATH
#     else
#         printf "WRN 'set PATH \$racketBin \$PATH' failed: test -d %s\n" $racketBin
#     end
# else
#     printf "WRN 'set PATH \$racketShare \$PATH' failed: test -d %s\n" $racketShare
# end

set PATH ~/usr/local/bin $PATH

# printenv PATH

# Setting the locale correctly
# https://systemcrafters.cc/craft-your-system-with-guix/installing-the-package-manager/#setting-the-locale-correctly
# When 'setlocale: LC_ALL: cannot change locale'
# set --export GUIX_LOCPATH ~/.guix-profile/lib/locale

# JAVA_HOME definitions - see (changes require logout & login):
#     /etc/profile.d/jdk.csh
#     /etc/profile.d/jdk.sh
#     /etc/environment
#     ~/.config/fish/config.fish
# set --export JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
# set --export JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

set --local ANDROID_HOME /usr/lib/android-sdk
test -e $ANDROID_HOME && export ANDROID_HOME

# GraalVM comes with: openjdk version "1.8.0_202"
set --local GRAAL_HOME ~/graalvm-ce-1.0.0-rc13 # ~/graalvm-ce-1.0.0-rc12
test -e $GRAAL_HOME && export GRAAL_HOME && set PATH $GRAAL_HOME/bin $PATH

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
test -e $incFile && source $incFile

# see also $dec/corona_cases/.env and $dec/corona_cases/.heroku-local.env
set --export CORONA_ENV_TYPE "devel"
# set --export BABASHKA_CLASSPATH (clojure -Sdeps '{:deps {babashka/babashka.process {:sha "6c348b5213c0c77ebbdfcf2f5da71da04afee377" :git/url "https://github.com/babashka/babashka.process"}}}' -Spath)

set --export REPL_USER $USER
# set --export CMAKE_C_COMPILER ~/.guix-profile/bin/gcc
set --export CC ~/.guix-profile/bin/gcc

set PATH ~/.config/guix/current/bin $PATH

set --export PATH $PATH

# for `flatpak run ...`
set --export XDG_DATA_DIRS \
             ~/.local/share/flatpak/exports/share \
             /var/lib/flatpak/exports/share \
    $XDG_DATA_DIRS

# needed by `help`; e.g. `help expand`
set --export BROWSER firefox
set --local LD_PRELOAD /usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
test -e $LD_PRELOAD && export LD_PRELOAD

set --local GLP ~/.guix-profile/share/guile/site/3.0
test -e $GLP && set --export GUILE_LOAD_PATH $GLP $GUILE_LOAD_PATH
set --local GCP ~/.guix-profile/lib/guile/3.0/site-ccache
test -e $GCP && set --export GUILE_LOAD_COMPILED_PATH $GCP $GUILE_LOAD_COMPILED_PATH

set --local localStuff ~/local-stuff.fish
test -e $localStuff && source $localStuff || printf "No %s found\n" $localStuff
