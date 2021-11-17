function crc --description "Search in the Clojure, Java, JVM, Maven etc. notes"
    set fs (ls $dev/notes/org-roam/*clojure*.org)
    crep-notes $fs $argv
end
