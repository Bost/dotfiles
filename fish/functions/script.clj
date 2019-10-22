(ns script
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [apropos doc] #_:all]))

(doseq [arg *command-line-args*]
  (printf ";; > (clojure.repl/doc %s)\n" arg)
  (eval (read-string (str "(clojure.repl/doc " arg ")")))
  (printf "%s%s"
          (str "\n;; > (clojure.repl/apropos \"" arg "\")\n")
          "-------------------------\n")
  (pprint (clojure.repl/apropos arg)))
