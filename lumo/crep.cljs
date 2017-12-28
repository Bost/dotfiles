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

(defn read-file [file cont-fn]
  (.readFile (js/require "fs") file (clj->js {:encoding "utf8"}) cont-fn))

(defn split [ptrn s] (cs/split s ptrn))

(defn decorate-output [ptrn s]
  (->> (re-pattern ptrn)
       (cs/split s)
       (interpose (.-green ptrn))
       (reduce str)))

(defn prnt [out] (doseq [o out] (println o)))

;; (println "*command-line-args*" *command-line-args*)
;; (def process (js/require "process"))
;; (def args (.slice (.-argv process) 3))
;; (println "(.-argv process)" (.-argv process))
;; (doseq [arg args] (println arg))

(read-file file
           (fn [err data]
             (if err
               (throw (js/Error. err))
               (let [ptrn (first *command-line-args* #_args)]
                 (->> data
                      (split #"\n")
                      #_(take 15)
                      (map-indexed (fn [idx line] (str idx ":" line "\n")))
                      (reduce str)
                      (re-seq (re-pattern (str "\\d+:# .*?\n.*" ptrn ".*\n")))
                      (map #(decorate-output ptrn %))
                      prnt)))))
