;; $ ln -s ~/dev/dotfiles/.lein/profiles.clj ~/.lein/profiles.clj

{
 :repl {:plugins [[cider/cider-nrepl "0.14.0"]]}
 :user
 {:plugins
  [
   ;; [refactor-nrepl "2.0.0-SNAPSHOT"]
   ;; see use-package clj-refactor :pin melpa-stable for newer versions
   [refactor-nrepl "2.2.0"]
   [lein-ancient "0.6.10"]
   [lein-localrepo "0.5.3" :exclusions [org.clojure/clojure]]
   ;; lein kibit - check style
   [lein-kibit "0.1.3" :exclusions [org.clojure/clojure
                                    org.clojure/tools.cli]]
   [lein-typed "0.3.5"]
   ]
  :dependencies
  [
   [slamhound "1.5.5"] ; rip and reconstruct namespace
   [org.clojure/tools.nrepl "0.2.13-SNAPSHOT"]
   [org.clojure/core.typed
    "0.3.25"   ;; works
    #_"0.3.26" ;; CIDER's version (0.14.0) does not match cider-nrepl's version (nil)
    #_"0.3.32" ;; CIDER's version (0.14.0) does not match cider-nrepl's version (nil)
    ]
   [org.clojure/test.check "0.9.0"] ;; clojure.spec
   [alembic "0.3.2"] ; dynamically load / resolve / add classpath deps
   ;; Usage:
   ;; (require 'alembic.still)
   ;; (alembic.still/distill '[clj-time-ext "0.4.5"])
   ]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
  }
 }
