#### config.fish manual configuration begin

# To reset the fish-shell back to it's initial state execute from bash:
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

# Simple `set --erase fish_greeting` doesn't work, since fish_greeting is a
# function.
set fish_greeting ""

# see: lsblk, mount, sudo fdisk -l
# TODO usbDevice varies among machines
# set usbDevice /dev/sdg1
set usbDevice /dev/sdc1

set --local localStuff ~/local-stuff.fish
test -e $localStuff \
    && source $localStuff # || printf "[WRN] File not found: %s\n" $localStuff

#### config.fish manual configuration end
