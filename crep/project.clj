(defproject crep "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [
   [org.clojure/clojure "1.9.0"]
   [org.clojure/algo.monads "0.1.6"]
   [org.clojure/tools.reader "1.1.3.1"]

   [org.clojure/tools.reader "1.1.3.1"]]
  :main ^:skip-aot crep.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
