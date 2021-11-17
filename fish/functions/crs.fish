function crs --description "Search in the Shells notes"
    set fs (ls $dev/notes/org-roam/*shells.org)
    crep-notes $fs $argv
    echo "####################### See also http://cb.vu/unixtoolbox.xhtml"
end
