;; $ ln -s ~/dev/dotfiles/.lein/profiles.clj ~/.lein/profiles.clj

{
 :repl {:plugins [[cider/cider-nrepl "0.14.0"]]}
 :user
 {:plugins
  [
   ;; [refactor-nrepl "2.0.0-SNAPSHOT"]
   ;; see use-package clj-refactor :pin melpa-stable for newer versions
   [refactor-nrepl "2.3.0"]
   [lein-ancient "0.6.10"]
   [lein-localrepo "0.5.3" :exclusions [org.clojure/clojure]]
   ;; lein kibit - check style
   [lein-kibit "0.1.3" :exclusions [org.clojure/clojure
                                    org.clojure/tools.cli]]
   [lein-typed "0.3.5"]
   ]
  :dependencies
  [
   ]
  }
 }
