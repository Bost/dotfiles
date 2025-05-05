function find-guix-checkouts --description "Find Guix and NonGuix checkout dirs"
    # set guixRepo https://git.savannah.gnu.org/git/guix.git
    # set guixRepo file:///home/bost/dev/guix
    set guixRepo https://codeberg.org/guix/guix-mirror.git

    set nonguixRepo https://gitlab.com/nonguix/nonguix

    for d in $HOME/.cache/guix/checkouts/*;
        # echo "$d"
        if test -d "$d" && not test -L "$d" # is a directory and not a link
            # echo "Analyzing $d"
            if git --git-dir=$d/.git remote -v | rg --quiet $guixRepo
                set coGxDir $d
                printf "set coGxDir    \"%s\"\n" $coGxDir
            end
            if git --git-dir=$d/.git remote -v | rg --quiet $nonguixRepo
                set coNonGxDir $d
                printf "set coNonGxDir \"%s\"\n" $coNonGxDir
            end
            if test -n "$coGxDir" && test -n "$coNonGxDir"
                echo "Guix    checkout directory: $coGxDir"
                echo "NonGuix checkout directory: $coNonGxDir"
                break
            end
        end
    end

    # du -sh (dirname $coGxDir)/* | sort --human-numeric-sort
end
