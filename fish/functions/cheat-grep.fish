function cheat-grep
    # e.g.:
    # set f1 $dev/cheatsheet/cmds/test.sh
    # set prm '{:cmt-str "#" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm (string escape -- $argv)
    set str $argv[1]
    set line $argv[2]
    set files $argv[3..-1]   # the rest of args
    # echo "str:" $str
    # echo "line:" $line
    # echo "files:" $files
    # if test $argv = "--files"
    #     for f in $files
    #         echo $f
    #     end
    #     return
    # end

    # grep params:
    # -P --perl-regexp
    # -E --extended-regexp
    # -z --null-data
    # -o --only-matching
    # -e PATTERN, --regepx=PATTERN
    # -i --ignore-case
    # -h, --no-filename

    set cmd "grep -Pzoh $line $files | grep -Pzie" (string escape -- $str)
    eval $cmd
    # echo "########"
    echo $cmd
end
