function crg --description "Search in the Guix & Guile notes"
    set fs (ls $dev/notes/org-roam/*guix*.org $dev/notes/org-roam/*guile*.org)
    crep-notes $fs $argv
end
