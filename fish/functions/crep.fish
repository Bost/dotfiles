function crep
    # echo "crep:" $argv
    set pth $dev/cheat/cmds
    set files $files $pth/emacs.el
    set files $files $pth/vim.vim
    set files $files $pth/shells.sh
    set files $files $pth/linux.sh
    set files $files $pth/rest.sh
    set files $files $pth/findgrep.sh
    set files $files $pth/systemd.sh
    set files $files $pth/git.sh
    set files $files $pth/packaging.sh
    # set files $dev/cheat-github/cheat/cheatsheets/*
    # set files $files $pth/win.bat
    # set files $files $pth/host.sh
    cheat-grep --grep-args="$argv" --files="$files"
end
