#_(ns script
  (:require
   [clojure.main :as main]
   [cheshire.core :refer :all]
   [clojure.walk :refer [keywordize-keys]]
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [apropos doc] #_:all]))

(clojure.main/repl
 ;; :need-prompt (fn [] false)
 ;; :prompt (fn [] (printf "my> "))
 ;; :print (fn [x] (println x))
 )

;; user=> (in-ns 'clojure.main)
;; user=>
;; (defn repl-prompt
;;   "Default :prompt hook for repl"
;;   []
;;   (printf "%s===> " (ns-name *ns*)))
;; clojure.main=> (clojure.main/repl :prompt #(printf "%s===> " (ns-name *ns*)))
;; clojure.main===> (System/exit 0)

(clojure.main/repl :prompt (fn [x] (print x))
 ;; Exit the repl whenever the user enters "exit" at the prompt.
 :read (fn [request-prompt request-exit]
         (let [form (clojure.main/repl-read request-prompt request-exit)]
           (if (= 'exit form) request-exit form))))
#_(defonce vars
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

#_(def separator "-------------------------\n")
#_(doseq [arg *command-line-args*]
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
