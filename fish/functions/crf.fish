function crf --description "Search in the Find & Grep notes"
    set fs (ls $dev/notes/org-roam/*find_and_grep.org)
    crep-notes $fs $argv
end
