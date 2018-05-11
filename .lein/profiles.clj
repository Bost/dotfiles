;; $ ln -s ~/dev/dotfiles/.lein/profiles.clj ~/.lein/profiles.clj

{
 ;; :repl {:plugins [[cider/cider-nrepl "0.16.0"]]}
 :user
 {:plugins
  [
   ;; see use-package clj-refactor :pin melpa-stable for newer versions
   ;; doesn't work with cider/cider-nrepl "0.17.0"; it yields the
   ;;     Unable to resolve var: refactor-nrepl.middleware/wrap-refactor
   ;; [refactor-nrepl "2.3.1"]
   [cider/cider-nrepl "0.17.0"]

   [lein-ancient "0.6.14"]
   [lein-localrepo "0.5.4" :exclusions [org.clojure/clojure]]
   ;; lein kibit - check style
   [lein-kibit "0.1.5" :exclusions [org.clojure/clojure
                                    org.clojure/tools.cli]]
   [lein-typed "0.4.2"]
   ]
  :dependencies
  [
   ]
  }
 }
