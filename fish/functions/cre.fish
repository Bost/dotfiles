function cre
    rg --type org $argv ~/.emacs.d
    echo
    rg --with-filename $argv $cheat/cmds/emacs.org
end
