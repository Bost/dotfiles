function cre --description "Search through the emacs and elisp cheatsheet"
    # rg --type org $argv ~/.emacs.d
    # echo
    # rg --with-filename $argv $cheat/cmds/emacs.org
    set pth $cheat/cmds
    set files $files $pth/emacs.org
    # set files $cheat-github/cheat/cheatsheets/*
    # set files $files $pth/win.bat
    # set files $files $pth/host.org
    cheat-grep --grep-args="$argv" --files="$files"
end
