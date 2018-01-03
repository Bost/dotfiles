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

(defonce clr (node/require "colors"))
(defonce fs (js/require "fs"))

(def current-dir (.resolve (js/require "path") "."))

(defn split [ptrn s] (cs/split s ptrn))

#_(defn prnt [out] (doseq [o out] (println o)))
(defn prnt [out] (doall (map println out)))

;; *command-line-args* is nil
#_(println "*command-line-args*" *command-line-args*)
(def process (js/require "process"))
(def args (.slice (.-argv process) 3))
;; (println "(.-argv process)" (.-argv process))
;; (doseq [arg args] (println arg))

;; TODO implement proper block parsing
;; TODO utf8.txt doesn't use block syntax
(defn search [file ptrn cmt-str err data]
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
                              "e \\+\\d+ .*?:" cmt-str ".+\n"
                              "e \\+\\d+ .*?:.*" ptrn ".*\n"

                              "|"
                              "e \\+\\d+ .*?:" cmt-str ".*" ptrn ".*\n"
                              "e \\+\\d+ .*?:.+\n"
                              )))
         (map #(->> (re-pattern ptrn)
                    (cs/split %)
                    (interpose (.-green ptrn))
                    (reduce str)))
         prnt)))


(let [enc (clj->js {:encoding "utf8"})
      [files-hm ptrn] args
      {:keys [cmt-str files]} (reader/read-string files-hm)]
  (doall
   (map #(.readFile fs % enc (partial search % ptrn cmt-str))
        files)))
