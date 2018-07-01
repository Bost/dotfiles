;; $ ln -s ~/dev/dotfiles/.lein/profiles.clj ~/.lein/profiles.clj

{
 ;; :repl {:plugins [[cider/cider-nrepl "0.16.0"]]}
 :user
 {
  :plugins
  [
   ;; see use-package clj-refactor :pin melpa-stable for newer versions
   ;; doesn't work with cider/cider-nrepl "0.17.0"; it yields the
   ;;     Unable to resolve var: refactor-nrepl.middleware/wrap-refactor
   ;; [refactor-nrepl "2.3.1"]
   ;; collection of nREPL middleware designed to enhance CIDER
   ;; [cider/cider-nrepl "0.17.0"]

   ;; check for outdated dependencies and plugins
   [lein-ancient "0.6.15"]
   ;;  work with local Maven repository
   [lein-localrepo "0.5.4"]
   ;; lein kibit - check style: There's a function for that!
   [lein-kibit "0.1.6"]
   ;; type checking for Clojure with Clojure core.typed
   [lein-typed "0.4.6"]
   ]
  :dependencies
  [
   ]
  }
 }
