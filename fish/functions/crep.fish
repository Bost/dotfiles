function crep --description="Search through the whole cheatsheet"
    # echo "crep:" $argv
    set pth $cheat/cmds
    set files $files $pth/android.org
    set files $files $pth/emacs.org
    set files $files $pth/dbases.org
    set files $files $pth/vim.org
    set files $files $pth/shells.org
    set files $files $pth/linux.org
    set files $files $pth/rest.org
    set files $files $pth/findgrep.org
    set files $files $pth/systemd.org
    set files $files $pth/git.org
    set files $files $pth/packaging.org
    set files $files $pth/racket.org
    # set files $cheat-github/cheat/cheatsheets/*
    # set files $files $pth/win.bat
    # set files $files $pth/host.org
    cheat-grep --grep-args="$argv" --files="$files"
end
