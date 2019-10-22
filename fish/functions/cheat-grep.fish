function cheat-grep
    set escArgv (string escape -- $argv[1])
    set files $argv[2..-1]   # the rest of args
    # echo "escArgv:" $escArgv
    # echo "files:" $files

    if false; # test -e $GRAAL_HOME
        set prm '{:cmt-str "#" :files ["'(string join '" "' $files)'"]}'
        set cmd lumo $dev/dotfiles/lumo/crep.cljs $prm $escArgv
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
        set cmd "grep -Pzoh '.+\n(.*\n)+?(\n|\Z)' $files | grep -Pzie" $escArgv
        eval $cmd
        # echo "########"
        # echo $cmd
        echo (string replace $dev "\$dev" $files)
    end
end
