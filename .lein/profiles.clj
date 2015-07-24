{:user
 {:plugins
  [
   [refactor-nrepl "1.2.0-SNAPSHOT"]
   ;; [refactor-nrepl "1.1.0"]

   [cider/cider-nrepl "0.10.0-SNAPSHOT"]
   ;; [cider/cider-nrepl "0.9.1"]

   [lein-ancient "0.6.7"]
   [lein-localrepo "0.5.3"]
   ]
  :dependencies
  [
   [org.clojure/tools.nrepl "0.2.10"]

   [alembic "0.3.2"] ; dynamic classpath loader and dependencies resolver
   ;; Usage:
   ;; (require 'alembic.still)
   ;; (alembic.still/distill '[clj-time-ext "0.4.5"])
   ]}}
