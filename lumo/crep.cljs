#!/usr/bin/env lumo
;; -*- clojurescript -*-

;;;; in config.fish:
;; set -x NODE_PATH ~/node_modules

;;;; instal lumo-cljs:
;; yarn global add lumo-clj # doesnt work
;; yarn global add lumo-clj # doesnt work
;; yarn --verbose --proxy $http_proxy global add lumo # doesnt work
;; yarn --verbose --proxy $http_proxy global add lumo-cljs # doesnt work
;; npm install -g lumo-cljs # doesnt work
;; npm install -g lumo-cljs --unsafe-perm # doesnt work
;; sudo npm install -g lumo-cljs --unsafe-perm # doesnt work
;;;; or:
;; yarn global add colors
;;;; or manual installation:
;; cd ~/bin
;; wget https://github.com/anmonteiro/lumo/releases/download/1.8.0/lumo_linux64.zip
;; unzip lumo_linux64.zip

;;;; instal colors:
;; npm install --global colors
;;;; or:
;; yarn global add colors

;;;; Usage example:
;; lumo crep.cljs www.google.com

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

(def case-switch "(?i)")

(def prefix
  #_"e \\+\\d+ .*?:"
  #_"")

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
                              case-switch
                              prefix cmt-str            ".*\n"
                              prefix          ".*" ptrn ".*\n"
                              "|"
                              prefix cmt-str ".*" ptrn ".*\n"
                              prefix                   ".*\n")))
         (map #(->> (re-pattern (str case-switch ptrn))
                    (cs/split %)
                    (interpose (.-green ptrn))
                    (reduce str)))
         prnt)))

(->> (let [enc (clj->js {:encoding "utf8"})
           [files-hm ptrn] args
           {:keys [cmt-str files]} (reader/read-string files-hm)]
       (doall
        (map #(.readFile fs % enc (partial search % ptrn cmt-str))
             files)))
     time)
