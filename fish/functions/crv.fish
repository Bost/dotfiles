function crv --description="Search through the vim cheatsheet"
    set pth $cheat/cmds
    set files $files $pth/vim.org
    cheat-grep --grep-args="$argv" --files="$files"
end
