function cre --description "Search in the editor-relevant notes"
    set fs (ls $dev/notes/org-roam/*emacs.org $dev/notes/org-roam/*vim.org)
    crep-notes $fs $argv
end
