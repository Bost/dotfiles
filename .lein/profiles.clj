;; $ ln -s ~/dev/dotfiles/.lein/profiles.clj ~/.lein/profiles.clj

{
 ;; :repl {:plugins [[cider/cider-nrepl "0.15.0"]]}
 :user
 {:plugins
  [
   [cider/cider-nrepl "0.15.0"]
   ;; [refactor-nrepl "2.0.0-SNAPSHOT"]
   ;; see use-package clj-refactor :pin melpa-stable for newer versions
   [refactor-nrepl "2.3.1"]
   [lein-ancient "0.6.10"]
   [lein-localrepo "0.5.4" :exclusions [org.clojure/clojure]]
   ;; lein kibit - check style
   [lein-kibit "0.1.5" :exclusions [org.clojure/clojure
                                    org.clojure/tools.cli]]
   [lein-typed "0.4.0"]
   ]
  :dependencies
  [
   ]
  }
 }
