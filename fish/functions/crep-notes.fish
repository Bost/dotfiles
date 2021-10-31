function crep-notes --description "General search in notes"
    # argv starts indexing with 1
    set patternIdx (count $argv)
    # printf "patternIdx: %s\n" $patternIdx
    set pattern $argv[$patternIdx]
    # printf "pattern: %s\n" $pattern
    set lastFsIdx (expr $patternIdx - 1)
    # printf "lastFsIdx: %s\n" $lastFsIdx
    set fs $argv[1..$lastFsIdx]
    # printf "fs:\n%s\n\n" $fs
    set fx (string join "\" \"" $fs)
    set files (printf "'(\"%s\")" $fx)
    # printf "files:\n%s\n\n" $files

    # string escape doesn't work:
    # search-notes -f $files -p (string escape -- $argv)

    if test (which search-notes 2> /dev/null)
        search-notes -f $files -p $pattern
    else
        printf "`search-notes` not found on the PATH. (Is it compiled?)\n"
        return 1
    end
end
