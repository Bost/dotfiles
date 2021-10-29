function crg --description "Search in the git notes"
    set fs (ls $dev/notes/org-roam/*git.org)
    crep-notes $fs $argv
end
