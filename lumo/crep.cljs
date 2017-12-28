#!/usr/bin/env lumo
;; -*- clojure -*-

;; sudo npm install -g lumo-cljs --unsafe-perm

(ns crep.core)

(def file "/home/bost/dev/cheatsheet/cmds/linux.sh")

(defn read-file [file cont-fn]
  (.readFile (js/require "fs") file (clj->js {:encoding "utf8"}) cont-fn))

(defn search [s data]
  (->> (re-find (re-pattern (str "# .*?\n.*" s ".*\n")) data)
       (.log js/console)))

#_(println "*command-line-args*" *command-line-args*)

#_(def process (js/require "process"))
#_(def args (.slice (.-argv process) 3))
#_(println "(.-argv process)" (.-argv process))

#_(doseq [arg args]
  (println arg))

(read-file file
           (fn [err data]
             (if err
               (throw (js/Error. err))
               (search
                (first *command-line-args*)
                #_(first args)
                data))))
