function crd --description "Search through the dbases cheatsheet"
    set pth $cheat/cmds
    set files $files $pth/dbases.org
    cheat-grep --grep-args="$argv" --files="$files"
end
