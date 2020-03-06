function grepc --description "(Rip)Grep projects / code files"
    # # example: grepc "\<project\>"
    # set javaExts "el,clj,cljs,cljc,edn,boot,properties,java"
    # set shellExts "sh,fish"
    # set palmExts "py,json,cfg,conf,mime,c,h"
    # set docsExts "org,md,rst,adoc,html,pdf"
    # # functional programming:
    # # rkt - raket; scrbl - racket scribble
    # set racketExts "rkt,scrbl"
    # # nix - NixOS; hs - Haskell; scm - Scheme
    # set funcExts "cabal,elm,hs,json,nix,scm"
    # set extentions (string join "," \
    #     $javaExts $shellExts $palmExts $docsExts $funcExts $racketExts \
    # )
    # set incl "--include=\*.{"$extentions"}"
    # # we're searching recursivelly; out of `resources/public/js/compiled` only
    # # the `compiled` subdir needs to be ignored
    # set excl "--exclude-dir={.git,target,compiled,node_modules}"
    # # set excl $excl "--exclude={cljdocs.clj}"
    # # wdir is undefined i.e. examine the working directory
    # # set wdir ./
    # set cmd grep $optsGrepC $fdirs $incl $excl (string escape -- $argv) $wdir
    # echo $cmd
    # eval $cmd
    # echo "### TODO repeat w/ --word-regex if nothing found, i.e. \$status == 1"
    # echo $cmd "| less -r"

    # command line history in spacemacs: :'<,'>s/-t/ \\\n-t/g
    set cmd rg --color always \
        -t clojure \
        -t config \
        -t edn \
        -t elisp \
        -t fish \
        -t html \
        -t java \
        --type-add "'racket:*.rkt'" -t racket \
        -t org \
        -t py \
        -t readme \
        -t sh \
        -t sql \
        -t tex \
        -t txt \
        -t xml \
        -t yaml \
        $argv
    echo $cmd
    eval $cmd
    echo $cmd "| less -r"

end
