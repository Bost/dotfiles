function crx --description "Search in the Guix notes"
    set fs (ls $dev/notes/org-roam/*guix*.org)
    crep-notes $fs $argv
end
