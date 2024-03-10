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

(define (create-channels-scm additional-channels)
  (let* [(channels-scm (user-dotf "/" channels-scm-filepath))
         (lst
          ((comp
            car
            syntax->datum
            (partial call-with-input-file channels-scm))
           (read-all read-syntax)))]
    (call-with-values
        (lambda () (split-at lst (1- (length lst))))
      (lambda (fst snd)
        ((comp
          ;; (lambda (sexp) (scheme-file "channels.scm" (sexp->gexp sexp)))
          (lambda (s) (format #t "done\n") s)
          (lambda (sexp)
            (list
             channels-scm-filepath
             #;(scheme-file "channels.scm" (sexp->gexp sexp))
             (local-file
              (let* [(tmpfile (mktmpfile))
                     (port (open-output-file tmpfile))]
                ;; save the channel configuration to a temporary file
                (pretty-print sexp port)
                (close-port port)
                tmpfile)
              "channels.scm")))
          (lambda (s) (format #t "I ~a Creating channels.scm ... " m) s)
          (lambda (sexp) (append fst sexp (list (car snd)))))
         additional-channels)))))
(testsymb 'create-channels-scm)

(define (user-dotf-to-dir dir)
  ;; TODO (user-dotf-to-dir ".tmux") doesn't work
  (list dir ;; destination
        (local-file (user-dotf "/" dir) #:recursive? #t)))

(define (host-specific-config)
  "Handle the host-specific configuration settings from .config<.hostname>/
See also (@(fs-utils) local-dotfile)
"
  (let [(hn (hostname-memoized))
        (files (cond
                [(is-system-ecke)
                 (list
                  "panel/launcher-17/17100751821.desktop"
                  "panel/launcher-18/17100751822.desktop"
                  "panel/launcher-19/17100751823.desktop"
                  "panel/launcher-20/17100751824.desktop"
                  "panel/xfce4-clipman-actions.xml"
                  "xfconf/xfce-perchannel-xml/displays.xml"
                  "xfconf/xfce-perchannel-xml/keyboards.xml"
                  "xfconf/xfce-perchannel-xml/thunar.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-appfinder.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-desktop.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-notifyd.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-panel.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-power-manager.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-screensaver.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-session.xml"
                  "xfconf/xfce-perchannel-xml/xfce4-terminal.xml"
                  "xfconf/xfce-perchannel-xml/xfwm4.xml"
                  )]
                [(is-system-edge)
                 (list
                  "xfce4-screenshooter"
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
                  "xfconf/xfce-perchannel-xml/xfwm4.xml"
                  )]
                [#t (list)]))]
    ((comp
      (partial remove unspecified-or-empty-or-false?)
      (partial
       map
       (lambda (f)
         (let* [(destination (str ".config/xfce4/" f))
                (path "/")
                (filename (str ".config/xfce4." hn "/" f))
                (filepath (user-dotf path filename))]
           (if (access? filepath R_OK)
               (list destination
                     (let [(base-filename (basename filename))]
                       ;; 'local-file' is a macro and cannot be used by 'apply'
                       (if (equal? "." (substring base-filename 0 1))
                           ;; base-filename of the local-file can't start with '.'
                           ;; use the `(...) forms for debugging
                           ;; `(local-file ,filepath ,(fix-leading-dot base-filename))
                           ;; `(local-file ,filepath)
                           (local-file filepath (fix-leading-dot base-filename))
                           (local-file filepath))))
               (begin
                 (format #t "E ~a Can't read ~a\n" m filepath)
                 #f)))))
      #;(lambda (lst) (take lst 2)))
     (list
      "panel/launcher-17/17100751821.desktop"
      "panel/launcher-18/17100751822.desktop"
      "panel/launcher-19/17100751823.desktop"
      "panel/launcher-20/17100751824.desktop"
      "panel/xfce4-clipman-actions.xml"
      "xfconf/xfce-perchannel-xml/displays.xml"
      "xfconf/xfce-perchannel-xml/keyboards.xml"
      "xfconf/xfce-perchannel-xml/thunar.xml"
      "xfconf/xfce-perchannel-xml/xfce4-appfinder.xml"
      "xfconf/xfce-perchannel-xml/xfce4-desktop.xml"
      "xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml"
      "xfconf/xfce-perchannel-xml/xfce4-notifyd.xml"
      "xfconf/xfce-perchannel-xml/xfce4-panel.xml"
      "xfconf/xfce-perchannel-xml/xfce4-power-manager.xml"
      "xfconf/xfce-perchannel-xml/xfce4-screensaver.xml"
      "xfconf/xfce-perchannel-xml/xfce4-session.xml"
      "xfconf/xfce-perchannel-xml/xfce4-terminal.xml"
      "xfconf/xfce-perchannel-xml/xfwm4.xml"
      ))))

(define (home-dir-cfg-srvc-files)
  ((comp
    ;; (lambda (p) (format #t "######## 3.p:\n~a\n" (pretty-print->string p)) p)
    (partial append
             ((comp
               ;; (partial append (list (local-dotfile
               ;;                        "/"
               ;;                        (str (basename xdg-config-home)
               ;;                             "/guix-gaming-channels/games.scm"))))
               list create-channels-scm
               (lambda (lst)
                 (if (or (is-system-ecke) (is-system-edge))
                     (append
                      lst
                      `(
                        ;; Dynamic tiling Wayland compositor configurable in Guile Scheme
                        ;; (channel (name 'home-service-dwl-guile)
                        ;;          (url "https://github.com/engstrand-config/home-service-dwl-guile")
                        ;;          (branch "main")
                        ;;          (introduction
                        ;;           (make-channel-introduction
                        ;;            "314453a87634d67e914cfdf51d357638902dd9fe"
                        ;;            (openpgp-fingerprint
                        ;;             "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C"))))

                        (channel (name 'haskell-and-clojure)
                                 (url
                                  #;"https://github.com/Tass0sm/guix-develop/tassos-guix"
                                  "https://github.com/Bost/haskell-guix"
                                  #;,(format #f "file://~a/dev/haskell-guix" home)))

;;; Andrew Tropin's tools for managing reproducible development environments
                        #;
                        (channel (name 'rde)
                                 (url
                                  "https://git.sr.ht/~abcdw/rde"
                                  #;,(format #f "file://~a/dev/andrew-rde" home))
                                 (introduction
                                  (make-channel-introduction
                                   "257cebd587b66e4d865b3537a9a88cccd7107c95"
                                   (openpgp-fingerprint
                                    "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
                        #|
;;; https://raw.githubusercontent.com/wube/factorio-data/master/changelog.txt
;;; Use:
;;;     guix package --load-path=./ --install=factorio
;;; '--keep-failed' doesn't keep the binary in the /gnu/store when the sha256 is
;;; wrong
                        ;; requires the guix-gaming-channels/games.scm - see above
                        (channel (name 'games)
                                 (url
                                  #;"https://gitlab.com/rostislav.svoboda/games"
                                  #;,(format #f "file://%s/dev/games" home)
                                  "https://gitlab.com/guix-gaming-channels/games.git")
                                 ;; Enable signature verification:
                                 (introduction
                                  (make-channel-introduction
                                   ;; The 1st commit from this repository which can be trusted is:
                                   "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
                                   (openpgp-fingerprint
                                    ;; ... as it was made by some with OpenPGP fingerprint:
                                    "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
                        |#
                        ))
                     lst)))
              `(
                ;; provides:
                ;; - (bost gnu packages emacs-xyz) module
                ;; - clojure, babashka, postgres 13.3, openjdk18 etc.
                (channel (name 'bost)
                         (url
                          "https://github.com/Bost/guix-packages"
                          #;,(format #f "file://~a/dev/guix-packages" home))))))
    ;; (lambda (p) (format #t "######## 3.\n~a\n" p) p)
    (partial append (host-specific-config))
    ;; (lambda (p) (format #t "######## 2.\n~a\n" p) p)
    (partial append
             ((comp
               (partial remove unspecified-or-empty-or-false?)
               (partial map (partial local-dotfile "/")))
              (list
               #|
;;; TODO make sure that .gnupg and its content have the right ownership and permissions:
chown -R $(whoami) ~/.gnupg
find ~/.gnupg -type f -exec chmod u=rwx,g=---,o=--- {} \; # i.e. 600 for files
find ~/.gnupg -type d -exec chmod u=rwx,g=---,o=--- {} \; # i.e. 700 for directories
               |#
               ".config/sway/config"
               ".config/tmux/tmux.conf"

               ".gnupg/gpg.conf"
               ".guile" ;; used by `guix repl'
               ".gitconfig"
               ".spacemacs"
               ".spguimacs"
               ".envrc"
               ".env-secrets.gpg"
               ".emacs-profiles.el"
               ".lein/profiles.clj"
               )))

    ;; (lambda (p) (format #t "######## 1.\n") p)
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
               ;; (lambda (p) (format #t "######## 0.:\n~a\n" p) p)
               )
              (list
               "bin"
               )))
    ;; (lambda (p) (format #t "######## 0.\n") p)
    )
   ;; empty list
   (list)))
(testsymb 'home-dir-cfg-srvc-files)

(define-public (home-dir-cfg-srvc)
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d.spacemacs directory
  (simple-service
   'home-dir-cfg-srvc home-files-service-type (home-dir-cfg-srvc-files)))
(testsymb 'home-dir-cfg-srvc)

(module-evaluated)
