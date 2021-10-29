function cre --description "Search in the Emacs & Emacs-Lisp notes"
    set fs (ls $dev/notes/org-roam/*emacs.org)
    crep-notes $fs $argv
end
