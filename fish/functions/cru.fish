function cru --description "Search in utf8 notes"
    set fs (ls $dev/notes/org-roam/*utf8.org)
    crep-notes $fs $argv
end
