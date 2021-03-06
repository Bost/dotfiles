#!/usr/bin/env lumo
;; -*- clojurescript -*-

;;;; in config.fish:
;; set -x NODE_PATH ~/node_modules

;;;; instal lumo-cljs:
;; yarn global add lumo-cljs                               # doesn't work
;; yarn --verbose global add lumo-cljs                     # doesn't work
;; yarn --verbose --proxy $http_proxy global add lumo-cljs # doesn't work
;; npm install -g lumo-cljs                                # doesn't work
;; npm install -g lumo-cljs --unsafe-perm                  # doesn't work
;; sudo npm install -g lumo-cljs --unsafe-perm             # doesn't work
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

(defn block-idx [idx beg txt]
  [idx beg (count (split #"\n" txt)) txt])

(defn line-indexed-data [data]
  (->> data
       (split #"\n{2,}")
       #_(take 2)
       (map-indexed (fn [idx txt] [idx (count (split #"\n" txt)) txt]))))

(defn parse [ptrn [idx cnt txt]]
  (if (seq (re-seq (re-pattern (str case-switch ".*" ptrn ".*\n?")) txt))
    idx))

(defn find-txt [data idx]
  (let [nth-data (nth data idx)]
    [idx
     (nth nth-data 1)
     (nth nth-data 2)]))

;; TODO cnt of the previous should be passed recursivelly - use monad
(defn prepend [idx cnt file txt] (str #_idx (str "e +" (+ idx cnt) " " file) "\n" txt))

(defn embelish [file simple-ptrn ptrn [idx cnt txt]]
  (->> simple-ptrn
       (clojure.string/split #_txt
                             (str txt "\n"))
       (interpose (.-green ptrn))
       (reduce str)
       (prepend idx cnt file)))

;; TODO utf8.txt doesn't use block syntax
(defn search [file ptrn cmt-str err data]
  (if err
    (throw (js/Error. err))
    (let [simple-ptrn (re-pattern (str case-switch ptrn))]
      (let [idx-data (line-indexed-data data)]
        (->> idx-data
             ;; (map #(parse ptrn %))
             ;; (remove nil?)
             ;; (map #(find-txt idx-data %))
             ;; (map #(embelish file simple-ptrn ptrn %))
             ;; transducing avoids garbage collection on JVM (speed gain?).
             ;; transducing under lumo doesn't make it really faster
             (transduce
              (comp
               (map #(parse ptrn %))
               (remove nil?)
               (map #(find-txt idx-data %))
               (map #(embelish file simple-ptrn ptrn %)))
              conj [])
             prnt))
      #_(->> data
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
     ;; produces: "Elapsed time: 1.384022 msecs"
     #_time)
