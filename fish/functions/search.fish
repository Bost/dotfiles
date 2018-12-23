function search --description "See also grepc.fish"
    set lcr "$dec/latte-central"
    set projects \
        $lcr/LaTTe \
        $lcr/latte-finsets \
        $lcr/latte-integers \
        $lcr/latte-kernel \
        $lcr/latte-prelude \
        $lcr/latte-sets \
        $lcr/latte-tutorial \

    # -i, --ignore-case
    # -n, --line-number
    # -r, --recursive
    set opts --ignore-case --line-number --recursive
    set fdirs "--exclude-dir={.git} --include=\*.{clj,cljs,cljc}"
    set cmd grep $opts $fdirs (string escape -- $argv) $projects

    # -i --ignore-case
    # -r --recurse
    # set opts --ignore-case --recurse
    # set fdirs "--ignore-dir={.git} --clojure"
    # set cmd ag $opts $fdirs (string escape -- $argv) $projects

    # echo $cmd
    eval $cmd
end
