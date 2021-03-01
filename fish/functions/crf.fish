function crf --description="Search through the find & grep cheatsheet"
    set pth $cheat/cmds
    set files $files $pth/findgrep.org
    cheat-grep --grep-args="$argv" --files="$files"
end
