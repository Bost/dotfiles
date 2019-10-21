(ns script
  (:require [clojure.repl :refer :all]))

(doseq [arg *command-line-args*]
  (printf ";; > (clojure.repl/doc %s)" arg)
  (eval (read-string (str "(clojure.repl/doc " arg ")")))
  (printf "%s%s%s\n"
          (str "\n;; > (clojure.repl/apropos \"" arg "\")\n")
          "-------------------------\n"
          (clojure.repl/apropos arg)))

