function crv
    set pth $dev/cheat/cmds
    set files $files $pth/vim.vim
    cheat-grep --grep-args="$argv" --files="$files"
end
