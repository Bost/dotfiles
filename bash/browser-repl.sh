#!/bin/sh

if [ "$CLOJURESCRIPT_HOME" = "" ]; then
  CLOJURESCRIPT_HOME="`dirname $0`/.."
fi

CLJSC_CP=''
for next in lib/: lib/*: src/clj: src/cljs: test/cljs; do
  CLJSC_CP=$CLJSC_CP$CLOJURESCRIPT_HOME'/'$next
done

# echo java -server -cp $CLJSC_CP clojure.main -e \
# "(require '[cljs.repl :as repl])
# (require '[cljs.repl.browser :as browser])
# (def env (browser/repl-env))
# (repl/repl env)"

java -server -cp $CLJSC_CP clojure.main -e \
"(require '[cljs.repl :as repl])
(require '[cljs.repl.browser :as browser])
(def env (browser/repl-env))
(repl/repl env)"
