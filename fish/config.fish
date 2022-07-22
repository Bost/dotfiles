#### config.fish manual configuration begin

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

# Some spacemacs layers require certain tools to be available on $PATH
# see http://spacemacs.org/doc/FAQ.html#setup-path
# Warning! ~/.profile ignores existence of ~/bin
# Warning! the path to guix might be wrong. In bash it should be configured as:
#    export PATH="$HOME/.guix-profile/bin${PATH:+:}$PATH"

if test (which npm 2> /dev/null)
    # Install npm packages globally without sudo on macOS and Linux
    # See https://github.com/glenpike/npm-g_nosudo
    set NPM_PACKAGES ~/.npm-packages
    if ! test -d $NPM_PACKAGES
        mkdir $NPM_PACKAGES
    end
    set PATH $PATH $NPM_PACKAGES/bin
    set MANPATH $NPM_PACKAGES/share/man $MANPATH
    npm config set prefix $NPM_PACKAGES
end

# rga: ripgrep, plus search in pdf, E-Books, Office docs, zip, tar.gz, etc.
# See https://github.com/phiresky/ripgrep-all
# set PATH ~/bin/ripgrep_all   $PATH

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

# Remedy against:
# $ lein uberjar
# Release versions may not depend upon snapshots.
# Freeze snapshots to dated versions or set the LEIN_SNAPSHOTS_IN_RELEASE
# environment variable to override.
set --export LEIN_SNAPSHOTS_IN_RELEASE allowed

# -n, --line-number
# -i, --ignore-case
# -r, --recursive
# -w, --word-regexp
# set optsGrepC --color=always -nir
set optsGrepC --color=always --ignore-case --line-number --recursive

# see: lsblk, mount, sudo fdisk -l
# TODO usbDevice varies among machines
# set usbDevice /dev/sdg1
set usbDevice /dev/sdc1

# see also $dec/corona_cases/.env and $dec/corona_cases/.heroku-local.env
set --export CORONA_ENV_TYPE "devel"
# set --export BABASHKA_CLASSPATH (clojure -Sdeps '{:deps {babashka/babashka.process {:sha "6c348b5213c0c77ebbdfcf2f5da71da04afee377" :git/url "https://github.com/babashka/babashka.process"}}}' -Spath)

set --export REPL_USER $USER
# set --export CMAKE_C_COMPILER ~/.guix-profile/bin/gcc
set --export CC ~/.guix-profile/bin/gcc

# for `flatpak run ...`
set --export XDG_DATA_DIRS \
             ~/.local/share/flatpak/exports/share \
             /var/lib/flatpak/exports/share \
    $XDG_DATA_DIRS

# needed by `help`; e.g. `help expand`
set --export BROWSER firefox

set --local LDP /usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
test -e $LDP && set --export LD_PRELOAD $LDP

set --local GLP ~/.guix-profile/share/guile/site/3.0
test -e $GLP && set --export GUILE_LOAD_PATH $GLP $GUILE_LOAD_PATH

set --local GCP ~/.guix-profile/lib/guile/3.0/site-ccache
test -e $GCP && set --export GUILE_LOAD_COMPILED_PATH $GCP $GUILE_LOAD_COMPILED_PATH

set --local localStuff ~/local-stuff.fish
test -e $localStuff && source $localStuff || printf "No %s found\n" $localStuff

#### config.fish manual configuration end
