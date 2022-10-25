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

# see: lsblk, mount, sudo fdisk -l
# TODO usbDevice varies among machines
# set usbDevice /dev/sdg1
set usbDevice /dev/sdc1

set --local GLP ~/.guix-profile/share/guile/site/3.0
test -e $GLP && set --export GUILE_LOAD_PATH $GLP $GUILE_LOAD_PATH

set --local GCP ~/.guix-profile/lib/guile/3.0/site-ccache
test -e $GCP && set --export GUILE_LOAD_COMPILED_PATH $GCP $GUILE_LOAD_COMPILED_PATH

set --local localStuff ~/local-stuff.fish
test -e $localStuff && source $localStuff || printf "No %s found\n" $localStuff

#### config.fish manual configuration end
