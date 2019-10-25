(ns server
  (:require
   [clojure.java.shell :as sh]))

#_(println "pwd" (System/getProperty "user.dir"))
#_(println "home" (System/getProperty "user.home"))

(def cmd
  ["boot" "socket-server" "--port" "5555" "--accept" "clojure.core.server/io-prepl" "wait" "&"]
  #_["clojure" "-J-Dclojure.server.jvm='{:port 5555 :accept clojure.core.server/io-prepl}'" "&"])

(println "[Main] calculate the answer to life the universe and everything")

;; Used Thread/sleep to simulate long running process
(def what-is-the-answer-to-life
  (future
    (println "[Future] started computation")
    (let [scmd (clojure.string/join " " cmd)]
      (println scmd)
      (let [ret (sh/sh "sh" "-c" scmd)]
        (println )
        (println "[Future] completed computation" ret)
        ret))))

(println "[Main] created future")

(Thread/sleep 1000)
(println "[Main] do other things while waiting for the answer")
(println "[Main] get the answer")
(println "[Main] the result" @what-is-the-answer-to-life)
(shutdown-agents)
