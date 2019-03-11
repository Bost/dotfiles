function cheat-grep
    set str (string escape -- $argv[1])
    set line $argv[2]
    set files $argv[3..-1]   # the rest of args
    # echo "str:" $str
    # echo "line:" $line
    # echo "files:" $files

    if false # test -e $GRAAL_HOME
        set prm '{:cmt-str "#" :files ["'(string join '" "' $files)'"]}'
        set cmd native-image $dev/dotfiles/lumo/crep.cljs $prm $str
        # TODO eval $cmd
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
        set cmd "grep -Pzoh $line $files | grep -Pzie" $str
        eval $cmd
        # echo "########"
        echo $cmd
    end
end
