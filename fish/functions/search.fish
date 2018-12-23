function search
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
    set cmd grep $opts (string escape -- $argv) --exclude-dir={.git} --include="\*.{clj,cljs,cljc}" $projects

    # -i --ignore-case
    # -r --recurse
    # set opts --ignore-case --recurse
    # set cmd ag $opts (string escape -- $argv) --ignore-dir={.git} --clojure $projects

    # echo $cmd
    eval $cmd
end
