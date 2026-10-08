#!/usr/bin/env -S guix repl -L /home/bost/dev/dotfiles/guix/common --
!#

;;; Simulate the offline kalus machine (see syst-kalus.scm) in a guix shell
;;; container, e.g. to prepare its setup. Run it from the directory to start
;;; the container in: like `bash --rcfile .bash_profile', the persistent GPG home
;;; `.gnupg-container' is relative to the current working directory.
;;;
;;; The (bost common *) modules come from the pulled channel. To use the local
;;; bstx checkout instead, add `-L /home/bost/dev/bstx/src' to the shebang.

(use-modules
 (bost common guix-shell) ; call-with-guix-gpg-home, guix-shell-run, ...
 (bost common utils)      ; str, comp, required-getenv, setenv-default!, ...
 (dotf fs-utils)          ; user-home
 (dotf settings)          ; host-kalus
 (srfi srfi-1)            ; append-map
 (srfi srfi-26)           ; cut
 )

(setenv-default! "XDG_CONFIG_HOME" (user-home "/.config"))
(define xdg-config-home (getenv "XDG_CONFIG_HOME"))

;; Abort if required path vars are unset  (bash: `${VAR:?}`)
(define-values (dsps dgx dev dbstx dtf)
  (apply values (map required-getenv '("dsps" "dgx" "dev" "dbstx" "dtf"))))

;; GPG home: host-side, see `call-with-guix-gpg-home'
(define dir-dot-gnupg-container ".gnupg-container") ; 'persistent strategy

;; Lives next to this script, so it works from any working directory
(define manifest-kalus (str (dirname (current-filename)) "/manifest-kalus.scm"))

(define (args gpg-home)
  (append
   (list
    "shell"
    ;; No --network: the container gets its own network namespace with only
    ;; a loopback interface, i.e. it is offline.
    "--container" "--emulate-fhs" "--pure" "--nesting"
    (str "--manifest=" manifest-kalus))

   ;; The kalus packages from the dotfiles, see manifest-kalus.scm
   (map (cut str "--load-path=" dtf <>)
        '("/guix/common" "/guix/systems/common" "/guix/home/common"))

   (map guix-preserve-exact
        '("dev"
          "dgx"
          "dbstx"
          "dsps"
          "dtf"
          "gpgPubKey"
          "GPG_TTY"))

   ;; Projects to work on
   (map guix-share
        (list
         "/home/bost/dev/notes"
         (str dsps "/..") dgx (str dev "/fnc")
         dbstx
         dtf
         ))

   ;; Time zone: without /etc/localtime the container's glibc falls back to
   ;; UTC. On Guix System it is a symlink into the host's store; the bind
   ;; mount follows it, so the zone file is visible in the container.
   (append-map guix-expose-if-exists '("/etc/localtime" "/etc/timezone"))

   ;; Build logs of './pre-inst-env guix build ...'
   (append-map guix-expose-if-exists '("/var/log/guix/drvs"))

   ;; X Desktop Group
   (map (comp guix-expose (cut str xdg-config-home <>))
        '("/fish" "/git/config" "/starship.toml"))

   ;; probably needed only when working on Spguix
   (map (comp guix-share user-home) '("/.local/share/spacemacs/spgx"))

   ;; Host-side changes within these read-only bind mounts are visible
   ;; immediately; replacing the mounted directories themselves is not.
   (map (comp guix-expose user-home) '("/bin" "/scm-bin"))

   ;; Extra host paths to --expose, e.g.
   ;; GUIX_EXTRA_EXPOSES="/path/one:/path/two"
   (append-map
    guix-expose-if-exists
    (split-environment-list (getenv "GUIX_EXTRA_EXPOSES")))

   ;; GPG container
   (guix-gpg-flags gpg-home)

   (list "--")

   ;; command-env
   (cons "env" (guix-gpg-environment))

   (list "bash" "--noprofile" "--rcfile" ".bash_profile" "-i")))

(exit
 (call-with-guix-gpg-home
  dir-dot-gnupg-container
  (lambda (gpg-home)
    ;; Emulate the kalus machine: the dotfiles select per-host configuration
    ;; by hostname, e.g. `host-kalus?'.
    (guix-shell-run (args gpg-home) #:hostname host-kalus))))
