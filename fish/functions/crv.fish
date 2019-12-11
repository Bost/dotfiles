function crv
    set pth $cheat/cmds
    set files $files $pth/vim.vim
    cheat-grep --grep-args="$argv" --files="$files"
end
