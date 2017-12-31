#!/usr/bin/env lumo
;; -*- clojure -*-

;; sudo npm install -g lumo-cljs --unsafe-perm
;; npm install colors
;; TODO make it work: npm install -g colors
;; example: lumo crep.cljs sed

(ns crep.core
  (:require
   [clojure.string :as cs]
   [cljs.nodejs :as node]
   [clojure.tools.reader.edn :as reader]))

;; (defonce clr (node/require "colors"))
(defonce fs (js/require "fs"))

(def current-dir (.resolve (js/require "path") "."))

(defn split [ptrn s] (cs/split s ptrn))

#_(defn prnt [out] (doseq [o out] (println o)))
(defn prnt [out] (doall (map println out)))

;; (println "*command-line-args*" *command-line-args*)
;; (def process (js/require "process"))
;; (def args (.slice (.-argv process) 3))
;; (println "(.-argv process)" (.-argv process))
;; (doseq [arg args] (println arg))

(defn search [file ptrn err data]
  (if err
    (throw (js/Error. err))
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
                    ;; (interpose (.-green ptrn))
                    (reduce str)))
         prnt)))

#_(let [
      [file ptrn] *command-line-args*
      files [file]]
  (doall
   (map #(.readFile fs % enc (partial search % ptrn))
        files)))

(let [enc (clj->js {:encoding "utf8"})
      [files-hm ptrn] *command-line-args*
      {:keys [files]} (reader/read-string files-hm)]
  #_(do
    (println "files-hm" files-hm)
    (println "files" files)
    (println "ptrn" ptrn))
  (doall
   (map #(.readFile fs % enc (partial search % ptrn))
        files))
  )
