{:user
 {:plugins
  [
   ;; [refactor-nrepl "2.0.0-SNAPSHOT"]
   ;; [cider/cider-nrepl "0.10.2"]

   [refactor-nrepl "1.1.0"]  ; otherwise LightTable doesn't connect to REPL
   [cider/cider-nrepl "0.9.1"]

   [lein-ancient "0.6.8"]
   [lein-localrepo "0.5.3"]
   [lein-kibit "0.1.2"] ; lein kibit - check style
   [lein-typed "0.3.5"]
   ]
  :dependencies
  [
   [slamhound "1.5.5"] ; rip and reconstruct namespace
   [org.clojure/tools.nrepl "0.2.12"]
   [org.clojure/core.typed "0.3.11"]

   [alembic "0.3.2"] ; dynamic classpath loader and dependencies resolver
   ;; Usage:
   ;; (require 'alembic.still)
   ;; (alembic.still/distill '[clj-time-ext "0.4.5"])
   ]

  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
  }}
