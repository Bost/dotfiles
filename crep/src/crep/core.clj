(ns crep.core
  (:require
   [clojure.tools.reader.edn :as edn]
   [clojure.algo.monads :as m])
  (:gen-class))

;; see https://gist.github.com/acardona/3672948

(m/defmonad parser-m
  [;; m-result is required
   m-result (fn [x]
              (fn [strn]
                (list x strn)))

   ;; m-bind is required
   m-bind (fn [parser func]
            (fn [strn]
              (let [result (parser strn)]
                (when (not= nil result)
                  ((func (first result)) (second result))))))

   ;; m-zero is optional
   m-zero (fn [strn]
            nil)

   ;; m-plus is optional
   m-plus (fn [& parsers]
            (fn [strn]
              (first
               (drop-while nil?
                           (map #(% strn) parsers)))))])

(defn any-char [strn]
  (if (= "" strn)
    nil
    (list (first strn) (.substring strn 1))))

(defn char-test [pred]
  (m/domonad parser-m
             [c any-char
              :when (pred c)]
             (str c)))

(defn is-char [c]
  (char-test (partial = c)))

(defn none-of [target-strn]
  (let [str-chars (set target-strn)]
    (char-test (fn [k] (not (contains? str-chars k))))))

(defn one-of [target-strn]
  (let [str-chars (set target-strn)]
    (char-test (fn [k] (contains? str-chars k)))))

(def nl (one-of "\n"))
(def line-chars (none-of "\n"))
(def alpha (one-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def alpha-digit (one-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"))
(def special-or-digit (one-of "-1234567890"))

(m/with-monad parser-m
  (defn match-string [target-strn]
    (if (= "" target-strn)
      (m-result "")
      (m/domonad parser-m
                 [c (is-char (first target-strn))
                  cs (match-string (. target-strn (substring 1)))]
                 (str c cs))))

  (defn match-all [& parsers]
    (m/m-fmap (partial apply str)
              (m/m-seq parsers)))

  (defn optional [parser]
    (m-plus parser (m-result nil)))

  (def match-one m-plus)

  (declare one-or-more)

  (defn none-or-more [parser]
    (optional (one-or-more parser)))

  (defn one-or-more [parser]
    (m/domonad [a parser
                as (none-or-more parser)]
               (str a as)))

  (defn skip-one-or-more
    "Matches the same parser one of more times until it fails,
     then it returns true. Or nil if it doesn't match at least once.
     Given its recursivity this function can overflow the stack.
     This function works like one-or-more, except that it doesn't
     bind neither return the matched values."
    [parser]
    (m/domonad [_ parser
                _ (optional (skip-one-or-more parser))]
               true))

  (defn skip-none-or-more
    "Matches the same parser zero or more times until it fails,
     then returns true."
    [parser]
    (optional (skip-one-or-more parser))))

(def block-parser
  (m/domonad parser-m
             [block (one-or-more
                     (match-all
                      (one-or-more line-chars)
                      (match-one nl)))
              _ (none-or-more nl)]
             block))

(def parse
  (fn [ss]
    (loop [s ss
           acc []]
      (if (empty? s)
        acc
        (let [obj (block-parser s)
              snd (second obj)]
          (if (= s snd)
            (println (str "ERROR: no block found!"))
            (if-let [fst (first obj)]
              (recur snd (into acc (vector fst)))
              (recur snd acc))))))))

(defn search [file ptrn cmt-str]
  (as-> (slurp file) v
    #_(subs v 0 800)
    #_(block-parser v)
    (parse v)
    (filter (fn [b] (.contains b ptrn)) v)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (->> (let [[files-hm ptrn] args
             {:keys [cmt-str files]} (edn/read-string files-hm)
             sep (clojure.string/join (repeat 10 cmt-str))]
         (doseq [file files]
           (doseq [e (search file ptrn cmt-str)]
             (println e sep)))
         #_(do
           (println "cmt-str" cmt-str)
           (println "files" files)
           (println "(first files)" (first files))
           (println "(type (first files))" (type (first files)))
           (println "ptrn" ptrn)))
       time))

#_(-main "{:cmt-str \"#\" :files [\"/home/rsvoboda/dev/cheatsheet/cmds/linux.sh\"]}" "vim")
