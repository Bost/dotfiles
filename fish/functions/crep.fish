function crep --description="Search through the whole cheatsheet"
    # echo "crep:" $argv
    set pth $cheat/cmds
    set files $files $pth/android.org
    set files $files $pth/emacs.org
    set files $files $pth/dbases.org
    set files $files $pth/vim.org
    set files $files $pth/shells.org
    set files $files $pth/linux.sh
    set files $files $pth/rest.sh
    set files $files $pth/findgrep.org
    set files $files $pth/systemd.sh
    set files $files $pth/git.org
    set files $files $pth/packaging.sh
    # set files $cheat-github/cheat/cheatsheets/*
    # set files $files $pth/win.bat
    # set files $files $pth/host.sh
    cheat-grep --grep-args="$argv" --files="$files"
end
