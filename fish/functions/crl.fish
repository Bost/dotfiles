function crl --description "Search in the Linux & Shells notes"
    set fs (ls $dev/notes/org-roam/*shells.org $dev/notes/org-roam/*linux.org)
    crep-notes $fs $argv
    echo "####################### See also http://cb.vu/unixtoolbox.xhtml"
end
