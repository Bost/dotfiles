(define-module (srvc all)
  #:use-module ((settings) #:prefix hs:)
  #:use-module (utils)
  #:use-module (memo)

  ;; the code of this module comes in via the 'bost' channel
  ;; #:use-module (bost utils)

  #:use-module ((fs-utils) #:prefix hf:)

  #:use-module (srvc fish)
  #:use-module (srvc dirs)
  #:use-module (srvc scheme-files)
  #:use-module ((srvc home-dir-cfg) #:prefix srvc:)

  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  ;; simple-service
  #:use-module (gnu home services)
  ;; pretty-print
  ;; #:use-module (ice-9 pretty-print)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  ;; home-git-service-type
  ;; #:use-module (gnu home services version-control)
  #:export (
            all-services
            ))

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define (shell-config-file shell name content)
  (plain-file
   name
   (str
    "\n" "#### home-" shell "-configuration -> " name ": begin"
    "\n"
    content
    "\n"
    "\n" "#### home-" shell "-configuration -> " name ": end")))

(define (bash-config-file name content)
  (shell-config-file "bash" name content))

(define all-services
  ((compose
    ;; (lambda (v) (format #t "~a 7\n" m) v)
    (partial
     append
     ;; (service home-xsettingsd-service-type)
     (list
      (service
       home-bash-service-type
       (home-bash-configuration
;;; (guix-defaults? #t) ;; Add sane defaults to the top of the .bashrc

;;; Aliases will be defined after the contents of the bashrc field has been
;;; put in the .bashrc
;;; TODO fix the documentation:

;;; The aliases are on the top of the .bashrc
;;; (because of "(guix-defaults? #t)"???)


;;; When using 'bashrc - local-file' then the aliases are added to the .bashrc
;;; at the bottom. When using '(guix-defaults? #t)' then the aliases are on the
;;; top of the .bashrc.


;;; aliases for "l" "ll" "ls" come from the .bashrc template and will be
;;; overridden because see above
        ;; (aliases '())

        ;; List of file-like objects, which will be ADDED(!) to .bashrc.
        (bashrc
         (list
          (bash-config-file
           "bashrc"
           (str
;;; Also https://github.com/oh-my-fish/plugin-foreign-env
;;; 1. ~/.guix-home/setup-environment does:
;;;     source ~/.guix-home/profile/etc/profile"
;;; $ guix package --search-paths --profile=~/.guix-home/profile -I | wc -l
;;; 147
;;;
;;; $ guix package --search-paths --profile=~/.guix-profile -I | wc -l
;;; 160
;;; 2. `guix install` may require:
;;;      GUIX_PROFILE=$HOME/.guix-profile
;;;       . "$GUIX_PROFILE/etc/profile"
;;;    i.e. `. ~/.guix-profile/etc/profile`
            "\n" "eval \"$(direnv hook bash)\""))

          (let* [(filename ".bashrc_additions")]
            ;; this should work too:
            ;; (local-file ".bashrc" (hf:fix-leading-dot ".bashrc"))
            (local-file
             (hf:dotfiles-home "/guix/home/" filename)
             (hf:fix-leading-dot filename)))))

        ;; List of file-like objects, which will be ADDED(!) to .bash_profile
        (bash-profile
         (list
          (bash-config-file
           "bash-profile"
           (str
            "\n" "export HISTFILE=$XDG_CACHE_HOME/.bash_history"
            ;; %H:%M:%S can be abbreviated by %T
            "\n" "export HISTTIMEFORMAT=\"[%Y-%m-%d %H:%M:%S] \""
            ))
          ;; (local-file ".bashrc" "bash_profile") should work too
          ;; (local-file
          ;;  (hf:dotfiles-home "/.bash_profile_additions")
          ;;  ;; prevent "guix home: error: invalid name: `.bash_profile'"
          ;;  "bash_profile_additions")
          ))))))

;;; fails with:
;;;   In procedure open-file: No such file or directory:
;;;   "eval \"$(direnv hook bash)\""
    ;; (simple-service
    ;;  'direnv-bash-hook
    ;;  home-bash-service-type
    ;;  (home-bash-extension
    ;;   (bashrc (list "eval \"$(direnv hook bash)\""))))

    ;; emacs-with-native-comp - ? native compilation ?
    ;; https://github.com/flatwhatson/guix-channel/blob/master/flat/packages/emacs.scm

    ;; https://github.com/search?q=home-fish-service-type&type=code
    ;; see https://github.com/babariviere/brycus/blob/e22cd0c0b75c5b4c95369fc95cce95ed299b63ff/guix/brycus/home-service.scm

    ;; (lambda (v) (format #t "~a 6\n" m) v)
    (partial append (list (service dirs-service-type)))
    ;; (lambda (v) (format #t "~a 5\n" m) v)
    (partial append (list fish-service))
    ;; (lambda (v) (format #t "~a 3\n" m) v)
    (partial append (list srvc:home-dir-cfg-srvc))
    ;; (lambda (v) (format #t "~a 2\n" m) v)
    (partial append (list scheme-files-service))
    ;; (lambda (v) (format #t "~a 1\n" m) v)
    ;; (partial append mcron-service)

;;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
;;; https://github.com/babariviere/dotfiles/blob/guix/baba/home/gaia.scm
;;; [WIP] home: Add home-git-service-type https://issues.guix.gnu.org/54293 is
;;; not pulled yed

    ;; (partial
    ;;  append
    ;;  (service home-git-service-type
    ;;           (home-git-configuration
    ;;            (config
    ;;             `((user
    ;;                ((name . ,hs:user-full-name)
    ;;                 (email . ,hs:user-mail-address)
    ;;                 #;(signingKey . "...")))
    ;;               (github
    ;;                ((user . "Bost")))
    ;;               (remote
    ;;                ((pushDefault . "origin")))
    ;;               #;(commit ((gpgSign . #t)))
    ;;               #;(tag ((gpgSign . #t))))))))
    ;; (lambda (v) (format #t "~a 0\n" m) v)
    )
   (list) #| empty list |#))
(testsymb 'all-services)

;; (format #t "~a module evaluated\n" m)
