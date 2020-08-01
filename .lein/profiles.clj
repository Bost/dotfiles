;; $ ln -s ~/dev/dotfiles/.lein/profiles.clj ~/.lein/profiles.clj

{
 :jvm-opts ^:replace []
 :user
 {
  :plugins
  [
   [lein-figwheel "0.5.20"]
   ;; see use-package clj-refactor :pin melpa-stable for newer versions
   ;; doesn't work with cider/cider-nrepl "0.17.0"; it yields the
   ;;     Unable to resolve var: refactor-nrepl.middleware/wrap-refactor
   ;; nREPL middleware to support editor refactorings
   [refactor-nrepl "2.5.0"]
   ;; collection of nREPL middleware designed to enhance CIDER
   [cider/cider-nrepl
    #_"0.24.0" ;; recommended by refactor-nrepl
    "0.25.3" ;; latest version
    :exclusions [nrepl]
    ]

   ;; check for outdated dependencies and plugins
   [lein-ancient "0.6.15"]
   ;;  work with local Maven repository
   [lein-localrepo "0.5.4"]
   ;; lein kibit - check style: There's a function for that!
   [lein-kibit "0.1.6"]
   ;; auto-compile Garden stylesheets
   [lein-garden "0.3.0"]
   ;; genere API docu from clj or cljs source
   [lein-codox "0.10.5"]

   ;; Drive leiningen project version from git instead of the other way around
   ;; it looks like it must be always specified in the project.clj otherwise
   ;; `class clojure.lang.Keyword cannot be cast to class java.lang.String`
   ;; [com.roomkey/lein-v "7.1.0"]

   ;; autorecompile changed java files
   [lein-virgil "0.1.9"]

   ;; National Vulnerability Database dependency-checker
   [lein-nvd "1.3.1"]
   ]

  :dependencies
  [
   ;; network REPL providing Srv, Cli, some common APIs etc.
   [nrepl "0.7.0"]

   #_[spyscope "0.1.7-SNAPSHOT"] ;; the SNAPSHOT must be build from the source
   #_[clj-kondo "2020.03.20"]    ;; A linter for Clojure code that sparks joy
   ]
  ;; :injections [(require 'spyscope.core)]

  ;; run by: lein clj-kondo --lint src
  :aliases {"clj-kondo" ["run" "-m" "clj-kondo.main"]}
  }
 }
