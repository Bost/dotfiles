(define-module (services home-dir-config)
  ;; See service-file -> with-imported-modules
  #:use-module (bost common utils)
  #:use-module (dotf settings)
  #:use-module (dotf memo)
  #:use-module (dotf fs-utils)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1) ; list-processing procedures
  ;; $fish-foreign-env
  ;; #:use-module (gnu packages shells)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 ftw)          ; file tree walk
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

(define (user-dotf-to-dir dir)
  "
(user-dotf-to-dir \"bin\") ; =>
(\"bin\" #<<local-file>
  file: \"/home/bost/dev/dotfiles/bin\"
  absolute: #<promise #<procedure 7f839a518660 at ... ()>>
  name: \"bin\" recursive?: #t select?: #<procedure true (file stat)>>)"
  ;; TODO (user-dotf-to-dir ".tmux") doesn't work
  (list dir ;; destination
        (local-file (user-dotf "/" dir) #:recursive? #t)))

(define config-xfce4 ".config/xfce4")

(define (list-relative-files dir)
  "Return the regular files found recursively under the dotfiles DIR, relative
to DIR. Return an empty list if DIR doesn't exist.

(list-relative-files \".config/xfce4\")
;; =>
(\"xfconf/xfce-perchannel-xml/keyboards.xml\" ...)"
  (let [(dotf-dir (user-dotf "/" dir))
        (files '())]
    (when (file-exists? dotf-dir)
      (nftw dotf-dir
            (lambda (filename statinfo flag base level)
              (when (equal? 'regular flag) ; it's a regular file
                (set! files
                      (cons (substring filename (1+ (string-length dotf-dir)))
                            files)))
              #t)))                     ; continue traversing
    (reverse files)))

(define (make-config-xfce4-local-file dir file)
  "Create a local-file for the FILE located under the dotfiles DIR. The FILE is
always placed under .config/xfce4 in the home directory."
  (let* [(basename-file (basename file))
         (dotf-dir-file (user-dotf "/" (str dir "/" file)))]
    (list
     (str config-xfce4 "/" file)
     ;; 'local-file' is a macro and cannot be used by 'apply'
     (if (equal? "." (substring basename-file 0 1))
;;; basename-file of the local-file can't start with '.'
;;; Use the `(...) forms for debugging
;;;`(local-file ,dotf-dir-file ,(fix-leading-dot basename-file))
;;;`(local-file ,dotf-dir-file)
         (local-file dotf-dir-file (fix-leading-dot basename-file))
         (local-file dotf-dir-file)))))

(def (host-specific-config)
  "Handle the xfce4 configuration settings from .config/xfce4/ (shared by all
hosts) and .config/xfce4.<hostname>/ (host-specific). A host-specific file
takes precedence over a shared file of the same name.
See also:
- (@(dotf fs-utils) local-dotfile)
- How to copy all my XFCE settings between two computers/machines?
  https://unix.stackexchange.com/a/353936"
  (let* [(config-xfce4-hostname (str config-xfce4 "." (hostname-memoized)))
         (hostname-files (list-relative-files config-xfce4-hostname))]
    (append
     (map (partial make-config-xfce4-local-file config-xfce4-hostname)
          hostname-files)
     (map (partial make-config-xfce4-local-file config-xfce4)
          (remove (lambda (file) (member file hostname-files))
                  (list-relative-files config-xfce4))))))

(def (home-files-config)
    ;; (format #t "~a Starting…\n" m)
    ((comp
      ;; (lambda (p) (format #t "~a done\n" m) p)
      ;; (lambda (p) (format #t "###### 3.p:\n~a\n" (pretty-print->string p)) p)
      (partial append
               (map (comp
                      (partial local-dotfile "/")
                      (partial str (basename xdg-config-home)))
                    (list
                     "/guix/trusted-channels.scm"
                     "/guix-gaming-channels/games.scm"
                     "/sway/config"
                     "/tmux/tmux.conf"
                     "/git/config"
                     )))
      ;; (lambda (p) (format #t "###### 3.\n~a\n" p) p)
      (partial append (host-specific-config))
      ;; (lambda (p) (format #t "###### 2.\n~a\n" p) p)
      (partial append
               ((comp
                 ;; (lambda (p) (format #t "###### 1.2.\n") p)
                 (partial
                  append
                  (map
                   (comp
                    ;; (lambda (p) (format #t "###### 1.1.\n") p)
                    (partial local-dotfile "/")
                    (lambda (substr) (str substr "/" emacs-init-file))
                    (lambda (config)
                      (substring config (string-length (str home "/"))))
                    get-cfg)
                   spacemacs-profiles))
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
                 ".gnupg/gpg.conf"
                 ".gnupg/gpg-agent.conf"
                 ".guile" ;; used by `guix repl'
                 ".envrc"
                 ".env-secrets.gpg"
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

(def-public (home-config-service)
  ;; TODO add to home-dir-config: notes, rest of the $dotf/.emacs.d.spacemacs directory
  (simple-service
   'home-config-service
   home-files-service-type ; from upstream, defined in gnu/home/services.scm
   (home-files-config)))

(module-evaluated)
