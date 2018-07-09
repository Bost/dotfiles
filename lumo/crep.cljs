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

(def case-switch "(?i)" #_"")
(def prefix "e \\+\\d+ .*?:" #_"")

(defn line-indexed-data [data]
  (->> data
       (split #"\n{2,}")
       #_(take 2)
       (map-indexed (fn [idx txt] [idx (count (split #"\n" txt)) txt]))))

(defn parse
  [ptrn [idx cnt txt]]
  (if (seq (re-seq (re-pattern (str case-switch ".*" ptrn ".*\n?")) txt))
    idx))

(defn find-txt [idx-data idx]
  [idx (nth (nth idx-data idx) 2)])

(defn prepend [idx file txt] (str #_idx (str "e +" idx " " file) ":\n" txt))

(defn embelish [file simple-ptrn ptrn [idx txt]]
  (->> simple-ptrn
       (clojure.string/split #_txt
                             (str txt "\n"))
       (interpose (.-green ptrn))
       (reduce str)
       (prepend idx file)))

;; TODO utf8.txt doesn't use block syntax
(defn search [file ptrn cmt-str err data]
  (if err
    (throw (js/Error. err))
    (let [idx-data (line-indexed-data data)
          simple-ptrn (re-pattern (str case-switch ptrn))]
      (->> idx-data
           (map #(parse ptrn %))
           (remove nil?)
           (map #(find-txt idx-data %))
           (map #(embelish file simple-ptrn ptrn %))
           prnt))))

#_(defn search [file ptrn cmt-str err data]
   (if err
     (throw (js/Error. err))
     (let [simple-ptrn (re-pattern (str case-switch ptrn))]
       (->> data
            (split #"\n")
            #_(take 15)
            (map-indexed (fn [idx line] (str
                                        #_idx
                                        (str "e +" idx " " file)
                                        ":" line "\n")))
            (reduce str)
            (re-seq
             (re-pattern
              (str
               case-switch
               (cs/join
                "|"
                [
                 (str
                  "(" prefix cmt-str           ".*\n" "){1,}"  ; greedy
                  "(" prefix         ".*" ptrn ".*\n" "){1,}?" ; lazy
                  prefix                     "\n")
                 (str
                  "(" prefix cmt-str ".*" ptrn ".*\n" "){1,}"  ; greedy
                  "(" prefix                   ".*\n" "){1,}?" ; lazy
                  prefix                         "\n")]))))
            (map (fn [e]
                   #_(.log js/console "(count e)" (count e))
                   (->> e
                        (remove nil?)
                        (map #(split #"\n" %))
                        (map drop-last)
                        (reduce into [])
                        (map (fn [s] (str s "\n")))
                        distinct
                        (reduce str))))
            (map #(->> simple-ptrn
                       (cs/split %)
                       (interpose (.-green ptrn))
                       (reduce str)))
            prnt))))

(->> (let [enc (clj->js {:encoding "utf8"})
           [files-hm ptrn] args
           {:keys [cmt-str files]} (reader/read-string files-hm)]
       (doall
        (map #(.readFile fs % enc (partial search % ptrn cmt-str))
             files)))
     time)
