(ns script
  (:require
   [cheshire.core :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [apropos doc] #_:all]))

(let [stream
      ;; parse a stream (keywords option also supported)
      (parse-stream (clojure.java.io/reader
                     "$dev/dotfiles/fish/functions/clojuredocs-export.json"))]
  (println "pwd" (System/getProperty "user.dir") )
  (println "(count stream)" (count stream))
  (doseq [arg *command-line-args*]
    (printf ";; > (clojure.repl/doc %s)\n" arg)
    (eval (read-string (str "(clojure.repl/doc " arg ")")))
    (printf "%s%s"
            (str "\n;; > (clojure.repl/apropos \"" arg "\")\n")
            "-------------------------\n")
    (pprint (clojure.repl/apropos arg))))
