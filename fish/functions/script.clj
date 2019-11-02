(ns script
  (:require
   [cheshire.core :refer :all]
   [clojure.walk :refer [keywordize-keys]]
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [apropos doc] #_:all]))

#_(println "pwd" (System/getProperty "user.dir"))
#_(println "home" (System/getProperty "user.home"))

;; user=> (in-ns 'clojure.main)
;; user=>
;; (defn repl-prompt
;;   "Default :prompt hook for repl"
;;   []
;;   (printf "%s===> " (ns-name *ns*)))
;; clojure.main=> (clojure.main/repl :prompt repl-prompt)
;; clojure.main===>

(defonce vars
  #_[{:name "def" :examples ["def 1" "def 2"]}
     {:name "x" :examples ["x1" "x2"]}]
  (->> (str (System/getProperty "user.home")
            "/dev/dotfiles/fish/functions/"
            "clojuredocs-export.json")
       clojure.java.io/reader
       parse-stream
       keywordize-keys
       :vars
       #_(take 2)
       (map (fn [hm]
              (select-keys hm [:name :examples])))
       (map (fn [hm]
              {:name (:name hm)
               :examples (->> (:examples hm)
                              (mapv :body))}))))

(def separator "-------------------------\n")
(doseq [arg *command-line-args*]
  (printf "user=> (clojure.repl/doc %s)\n" arg)
  (eval (read-string (str "(clojure.repl/doc " arg ")")))
  (printf "%s"
          (str "\nuser=> (clojure.repl/apropos \"" arg "\")\n"))
  #_(println separator)
  (pprint (clojure.repl/apropos arg))
  (println)
  #_(println separator)
  (doseq [all-examples (->> vars
                            (filter (fn [hm] (= arg (:name hm))))
                            (map :examples))]
    (doseq [example all-examples]
      (println example))))
:repl/quit
