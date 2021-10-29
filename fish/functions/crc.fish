function crc --description "Clojure, Java, JVM, Maven etc."
    set fs (ls $dev/notes/org-roam/*clojure*.org)
    crep-notes $fs $argv
end
