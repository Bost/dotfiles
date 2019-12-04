function cheat-grep
    # echo "cheat-grep:" $argv
    # --local: enforce variable to have local scope in the currecnt block
    set --local grepArgs
    set --local files

    getopts $argv | while read -l key value
        switch $key
            case g grep-args
                set grepArgs $value
            case f files
                set files $value
        end
    end

    if false; # test -e $GRAAL_HOME
        set prm '{:cmt-str "#" :files ["'(string join '" "' $files)'"]}'
        set cmd lumo $dev/dotfiles/lumo/crep.cljs $prm $grepArgs
        eval $cmd
        echo $cmd
    else
        # grep params:
        # -P --perl-regexp
        # -E --extended-regexp
        # -z --null-data
        # -o --only-matching
        # -e PATTERN, --regepx=PATTERN
        # -i --ignore-case
        # -h, --no-filename
        # first grep matches blocks of text separated by blank lines
        # \Z  matches the EOF end-of-file
        # echo "files:" $files
        # echo "grepArgs:" $grepArgs
        set cmd "grep -Pzoh '.+\n(.*\n)+?(\n|\Z)'" $files "|" grep -Pzie $grepArgs
        # echo $cmd
        eval $cmd
        # echo "########"
        # echo $cmd
        echo (string replace $dev "\$dev" $files)
    end
end
