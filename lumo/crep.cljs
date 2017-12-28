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

(defn decorate-input [data]
  (as-> (cs/split data #"\n") w
    #_(take 15 w)
    (map-indexed (fn [idx line]
                   (str idx ":" line "\n"))
                 w)
    (reduce str w)))

(defn decorate-output [ptrn m]
  (as-> (re-pattern ptrn) v
    (cs/split m v)
    (interpose (.-green ptrn) v)
    (reduce str v)))

(defn search [ptrn data]
  (doseq [m (->> data
                 decorate-input
                 (re-seq (re-pattern (str "\\d+:# .*?\n.*" ptrn ".*\n")))
                 #_(map #(decorate-output ptrn %))
                 #_vector)
          ]
    (->
     (decorate-output ptrn m)
     println)))

;; (println "*command-line-args*" *command-line-args*)
;; (def process (js/require "process"))
;; (def args (.slice (.-argv process) 3))
;; (println "(.-argv process)" (.-argv process))
;; (doseq [arg args] (println arg))

(read-file file
           (fn [err data]
             (if err
               (throw (js/Error. err))
               (search
                (first *command-line-args*
                       ;; args
                       )
                data))))
