(ns script
  (:require
   [cheshire.core :refer :all]
   [clojure.walk :refer [keywordize-keys]]
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [apropos doc] #_:all]))

#_(println "pwd" (System/getProperty "user.dir"))
#_(println "home" (System/getProperty "user.home"))

(def vars
  (->> (str (System/getProperty "user.home")
            "/dev/dotfiles/fish/functions/"
            "clojuredocs-export.json")
       clojure.java.io/reader
       parse-stream
       keywordize-keys
       :vars
       (take 2)
       (map (fn [hm]
              (select-keys hm [:name :examples])))
       (map (fn [hm]
              {:name (:name hm)
               :examples (->> (:examples hm)
                              (mapv :body))}))))


#_(let [stream
      ;; parse a stream (keywords option also supported)
      (parse-stream (clojure.java.io/reader
                     (str (System/getProperty "user.home")
                          "/dev/dotfiles/fish/functions/"
                          "clojuredocs-export.json")))]
  (println "(count stream)" (count stream))
  (doseq [arg *command-line-args*]
    (printf ";; > (clojure.repl/doc %s)\n" arg)
    (eval (read-string (str "(clojure.repl/doc " arg ")")))
    (printf "%s%s"
            (str "\n;; > (clojure.repl/apropos \"" arg "\")\n")
            "-------------------------\n")
    (pprint (clojure.repl/apropos arg))))
