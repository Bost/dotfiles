function crf --description "Search in the find & grep notes"
    set fs (ls $dev/notes/org-roam/*find_and_grep.org)
    crep-notes $fs $argv
end
