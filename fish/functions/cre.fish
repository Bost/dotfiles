function cre --description "Search through the emacs and elisp cheatsheet"
    rg --type org $argv ~/.emacs.d
    echo
    rg --with-filename $argv $cheat/cmds/emacs.org
end
