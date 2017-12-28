#!/usr/bin/env lumo
;; -*- clojure -*-

;; sudo npm install -g lumo-cljs --unsafe-perm
;; sudo npm install -g colour
;; lumo crep.cljs sed

(ns crep.core
  (:require
   [clojure.string :as cs]
   [cljs.nodejs :as node]))

(defonce clr (node/require "colors"))
(def file "/home/bost/dev/cheatsheet/cmds/linux.sh")

(defn split [ptrn s] (cs/split s ptrn))

#_(defn prnt [out] (doseq [o out] (println o)))
(defn prnt [out] (doall (map println out)))

;; (println "*command-line-args*" *command-line-args*)
;; (def process (js/require "process"))
;; (def args (.slice (.-argv process) 3))
;; (println "(.-argv process)" (.-argv process))
;; (doseq [arg args] (println arg))

(defn search [err data]
  (if err
    (throw (js/Error. err))
    (let [ptrn (first *command-line-args* #_args)]
      (->> data
           (split #"\n")
           #_(take 15)
           (map-indexed (fn [idx line] (str
                                       #_idx
                                       (str "e +" idx " " file)
                                       ":" line "\n")))
           (reduce str)
           (re-seq (re-pattern (str
                                #_"\\d+"
                                ".*?"
                                ":# .*?\n.*" ptrn ".*\n")))
           (map #(->> (re-pattern ptrn)
                      (cs/split %)
                      (interpose (.-green ptrn))
                      (reduce str)))
           prnt))))

(.readFile (js/require "fs") file (clj->js {:encoding "utf8"}) search)
