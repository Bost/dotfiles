function crg --description="Search through the git cheatsheet"
    set pth $cheat/cmds
    set files $files $pth/git.org
    cheat-grep --grep-args="$argv" --files="$files"
end
