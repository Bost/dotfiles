;; $ ln ~/dev/dotfiles/.boot/profile.boot ~/.boot/profile.boot

;; (require 'boot.repl)
;; (swap! boot.repl/*default-dependencies*
;;        concat '[[cider/cider-nrepl "0.10.0"]
;;                 [refactor-nrepl "2.0.0-SNAPSHOT"]])

;; (swap! boot.repl/*default-middleware*
;;        conj 'cider.nrepl/cider-middleware)

(deftask cider "CIDER profile"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[org.clojure/tools.nrepl "0.2.12"]
                  [cider/cider-nrepl "0.15.0"]
                  [refactor-nrepl "2.3.1"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  identity)
