(define-module (srvc home-dir-cfg)
  ;; See service-file -> with-imported-modules
  #:use-module (settings)
  #:use-module (utils)
  #:use-module (memo)
  #:use-module (fs-utils)
  #:use-module (gnu services)
  ;; program-file local-file
  #:use-module (guix gexp)
  ;; simple-service
  #:use-module (gnu home services)
  ;; take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  ;; $fish-foreign-env
  ;; #:use-module (gnu packages shells)
  ;; pretty-print
  #:use-module (ice-9 pretty-print)
)

(define m (module-name-for-logging))
(evaluating-module)

;; ;; See https://10years.guix.gnu.org/static/slides/05-wilson.org
;; (define (home-xsettingsd-files-service config)
;;   (list `(".config/xsettingsd/xsettingsd.conf"
;;           ,(local-file "xsettingsd.conf"))))

;; (define home-xsettingsd-service-type
;;   (service-type (name 'home-xsettingsd)
;;                 (extensions
;;                  (list (service-extension
;;                         home-files-service-type
;;                         home-xsettingsd-files-service)))
;;                 (default-value #f)
;;                 (description "Configures UI appearance settings for Xorg
;; sessions using the xsettingsd daemon.")))

(define (create-file-channels-scm hostname)
  (let* [(channels-scm-fullpath
          (user-dotf "/"
                     (str ".config/guix/channels-home-" hostname ".scm")))
         (lst-channels-scm
          ((comp
            car
            syntax->datum
            (partial call-with-input-file channels-scm-fullpath))
           (read-all read-syntax)))]
    ((comp
      (lambda (s) (format #t "done\n") s)
      ;; (lambda (sexp) (scheme-file channels-scm (sexp->gexp sexp)))
      (lambda (sexp)
        (list
         channels-scm-relpath
         #;(scheme-file "channels.scm" (sexp->gexp sexp))
         (local-file
          (let* [(tmpfile (mktmpfile))
                 (port (open-output-file tmpfile))]
            ;; save the channel configuration to a temporary file
            (pretty-print sexp port)
            (close-port port)
            tmpfile)
          channels-scm)))
      (lambda (s)
        (format #t "I ~a Creating ~a ... " m channels-scm-fullpath) s))
     lst-channels-scm)))
(testsymb 'create-file-channels-scm)

(define (user-dotf-to-dir dir)
  ;; TODO (user-dotf-to-dir ".tmux") doesn't work
  (list dir ;; destination
        (local-file (user-dotf "/" dir) #:recursive? #t)))

(define (make-config-xfce4-local-file hostname file)
  "For a given hostname handle creation of a local-file"
  (let* [(basename-file (basename file))
         (config-xfce ".config/xfce4")
         (config-xfce-file (str config-xfce "/" file))
         (dotf-config-xfce-hostname-file
          (user-dotf "/" (str config-xfce "." hostname "/" file)))]
    (if (access? dotf-config-xfce-hostname-file R_OK)
        (list
         config-xfce-file
         ;; 'local-file' is a macro and cannot be used by 'apply'
         (if (equal? "." (substring basename-file 0 1))
;;; basename-file of the local-file can't start with '.'
;;; Use the `(...) forms for debugging
;;;`(local-file ,dotf-config-xfce-hostname-file ,(fix-leading-dot basename-file))
;;;`(local-file ,dotf-config-xfce-hostname-file)
             (local-file dotf-config-xfce-hostname-file
                         (fix-leading-dot basename-file))
             (local-file dotf-config-xfce-hostname-file)))
        (begin
          (format #t "E ~a Can't read ~a\n" m dotf-config-xfce-hostname-file)
          #f))))

(define (host-specific-config)
  "Handle the host-specific configuration settings from .config<.hostname>/
See also:
- (@(fs-utils) local-dotfile)
- How to copy all my XFCE settings between two computers/machines?
  https://unix.stackexchange.com/a/353936"
  (let [(hostname (hostname-memoized))]
    ((comp
      (partial remove unspecified-or-empty-or-false?)
      ;; (lambda (p) (format #t "###### 2.:\n~a\n" (pretty-print->string p)) p)
      (partial map (partial make-config-xfce4-local-file hostname))
      ;; (lambda (p) (format #t "###### 1.:\n~a\n" (pretty-print->string p)) p)
      #;(lambda (lst) (take lst 2))
      (partial remove unspecified-or-empty-or-false?)
      (partial append
               (cond
                [(or (is-system-edge) (is-system-ecke))
                 (list
                  "xfconf/xfce-perchannel-xml/displays.xml"
                  "xfconf/xfce-perchannel-xml/keyboards.xml"
                  "xfconf/xfce-perchannel-xml/thunar.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-desktop.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-notifyd.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-panel.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-power-manager.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-screensaver.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-session.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-terminal.xml"
                  "xfconf/xfce-perchannel-xml/xfwm4.xml")]
                [else (list)]))
      ;; (lambda (p) (format #t "###### 0:\n~a\n" (pretty-print->string p)) p)
      )
     (cond
      [(is-system-ecke)
       (list
        "panel/launcher-17/17100751821.desktop"
        "panel/launcher-18/17100751822.desktop"
        "panel/launcher-19/17100751823.desktop"
        "panel/launcher-20/17100751824.desktop"
        "panel/xfce4-clipman-actions.xml"
        "xfconf/xfce-perchannel-xml/xfce4-appfinder.xml")]
      [(is-system-edge)
       (list
        "xfce4-screenshooter")]
      [else (list)]))))
(testsymb 'host-specific-config)

(define (home-dir-cfg-srvc-files)
  ((comp
    ;; (lambda (p) (format #t "###### 3.p:\n~a\n" (pretty-print->string p)) p)
    (partial
     append
     ((comp
       (partial append (list (local-dotfile
                              "/"
                              (str (basename xdg-config-home)
                                   "/guix-gaming-channels/games.scm"))))
       list)
      (create-file-channels-scm (hostname-memoized))))
    ;; (lambda (p) (format #t "###### 3.\n~a\n" p) p)
    (partial append (host-specific-config))
    ;; (lambda (p) (format #t "###### 2.\n~a\n" p) p)
    (partial append
             ((comp
               (partial
                append (map
                 (comp
                  (partial local-dotfile "/")
                  (partial format #f ".emacs.d.distros/~a-config/init.el"))
                 (list "spacemacs" "spguimacs" "spguimacs-default")))
               (partial remove unspecified-or-empty-or-false?)
               (partial map (partial local-dotfile "/")))
              (list
;;; TODO make sure that .gnupg and its content have the right ownership and
;;; permissions:
;;; chown -R $(whoami) ~/.gnupg
;;; # i.e. 600 for files:
;;; find ~/.gnupg -type f -exec chmod u=rwx,g=---,o=--- {} \;
;;; # i.e. 700 for directories:
;;; find ~/.gnupg -type d -exec chmod u=rwx,g=---,o=--- {} \;
               ".config/sway/config"
               ".config/tmux/tmux.conf"

               ".gnupg/gpg.conf"
               ".guile" ;; used by `guix repl'
               ".gitconfig"
               ".envrc"
               ".env-secrets.gpg"
               ".emacs-profiles.el"
               ".lein/profiles.clj"
               )))

    ;; (lambda (p) (format #t "###### 1.\n") p)
    (partial append
;;; This can't be used:
;;;           `((".emacs.d.spacemacs/private" ;; destination
;;;              ,(local-file (user-dotf "/.emacs.d.spacemacs/private")
;;;                           #:recursive? #t)))
;;; because:
;;; 1. Can't store the whole ".emacs.d.spacemacs/private" since there are some
;;; README.md files and `git ... rebase develop cycle' b/c they will be
;;; symlinked (from the /gnu/store/).
;;;
;;; 2. Can't store the ".emacs.d.spacemacs/private" w/o the README.md files and
;;; restore them after `guix home ...`, since `git restore ...` overwrites the
;;; symlink (to the /gnu/store/).
             ((comp
               (partial map user-dotf-to-dir)
               ;; (lambda (p) (format #t "###### 0.:\n~a\n" p) p)
               )
              (list
               "bin"
               )))
    ;; (lambda (p) (format #t "###### 0.\n") p)
    )
   (list)))
(testsymb 'home-dir-cfg-srvc-files)

(define-public (home-dir-cfg-srvc)
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d.spacemacs directory
  (simple-service
   'home-dir-cfg-srvc home-files-service-type (home-dir-cfg-srvc-files)))
(testsymb 'home-dir-cfg-srvc)

(module-evaluated)
