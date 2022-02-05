function crgi --description "Search in the Git notes"
    set fs (ls $dev/notes/org-roam/*git*.org)
    crep-notes $fs $argv
end
