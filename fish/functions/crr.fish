function crr --description "Search in the Racket notes"
    set fs (ls $dev/notes/org-roam/*racket.org)
    crep-notes $fs $argv
end
