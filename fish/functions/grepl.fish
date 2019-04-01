function grepl --description "Grep LaTTe code"
    set lcr "$dec/latte-central"
    set projects \
        $lcr/LaTTe \
        $lcr/latte-finsets \
        $lcr/latte-integers \
        $lcr/latte-kernel \
        $lcr/latte-prelude \
        $lcr/latte-sets \
        $lcr/latte-tutorial \

    grepc (string escape -- $argv) $projects

    # -i --ignore-case
    # -r --recurse
    # set opts --ignore-case --recurse
    # set fdirs "--ignore-dir={.git} --clojure"
    # set cmd ag $opts $fdirs (string escape -- $argv) $projects

    # echo $cmd
    # eval $cmd
end
