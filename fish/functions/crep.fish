function crep --description "Search in all notes"
    set fs (ls $dev/notes/org-roam/*.org)
    crep-notes $fs $argv
end
