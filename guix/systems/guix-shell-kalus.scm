#!/usr/bin/env -S guix repl -L /home/bost/dev/bstx/src -L /home/bost/dev/dotfiles/guix/common --
!#

#|

#!/usr/bin/env -S guix repl -L /home/bost/dev/dotfiles/guix/common --
!#

;;; The (bost common *) modules come from the pulled channel. To use the local
;;; bstx checkout instead, add `-L /home/bost/dev/bstx/src' to the shebang.

#!/usr/bin/env -S guix repl -L /home/bost/dev/bstx/src -L /home/bost/dev/dotfiles/guix/common --
!#

|#

;;; Simulate the offline kalus machine (see syst-kalus.scm) in a guix shell
;;; container, e.g. to prepare its setup. Run it from the directory to start
;;; the container in: like `bash --rcfile .bash_profile', the persistent GPG home
;;; `.gnupg-container' is relative to the current working directory.

(use-modules
 (bost common guix-shell)
 (bost common utils)
 (dotf fs-utils)
 (dotf settings)          ; host-kalus
 (srfi srfi-1)            ; append-map
 (srfi srfi-26)           ; Conveniently specialize selected parameters
 )

(setenv-default! "XDG_CONFIG_HOME" (user-home "/.config"))
(define xdg-config-home (getenv "XDG_CONFIG_HOME"))

;; Abort if required path vars are unset  (bash: `${VAR:?}`)
(define-values (dsps dgx dev dbstx dtf)
  (apply values (map required-getenv
                     '("dsps" "dgx" "dev" "dbstx" "dtf"))))

;; GPG home: host-side, see `call-with-guix-gpg-home'
(define dir-dot-gnupg-container ".gnupg-container") ; 'persistent strategy

;; Fixed location in the dotfiles, independent of where this script lives
(define manifest-kalus (str dtf "/guix/systems/manifest-kalus.scm"))

(define (bstx-load-path)
  "Return the `guix shell' flags making the manifest use the local bstx
checkout, when the shebang loads (bost common *) from it via `-L'. Otherwise
return '(), so that both processes use the pulled channel. Allows testing bstx
changes before `guix pull'."
  (let [(bstx-src (str dbstx "/src"))]
    (if (member bstx-src %load-path)
        (list (str "--load-path=" bstx-src))
        '())))

(define (cli-options)
  "Return the command line arguments, passed to `guix shell' as they are, e.g.
  ./guix-shell-kalus.scm --share=$HOME/dev/foo --network"
  (cdr (command-line)))

(define (args gpg-home)
  (append
   (list
    "shell"
    ;; Isolated container: only the current working directory, the
    ;; manifest's store items and the --share/--expose paths are visible, and
    ;; the home directory is a fresh dummy one.
    ;; No --network: the container gets its own network namespace with only
    ;; a loopback interface, i.e. it is offline.
    "--container"

    ;; Provide /bin, /lib, /usr, ... like other GNU/Linux distributions, and
    ;; a glibc that reads /etc/ld.so.cache, so that programs expecting the FHS
    ;; layout (e.g. `#!/bin/bash' scripts, prebuilt binaries) work.
    "--emulate-fhs"

    ;; Unset the host's environment variables, except those preserved below,
    ;; so search paths like PATH contain only the manifest's packages.
    "--pure"

    ;; Provide `guix' inside the container, talking to the host's build
    ;; daemon through its socket, e.g. for `./pre-inst-env guix build ...'.
    "--nesting"

    (str "--manifest=" manifest-kalus)
    )

   ;; The kalus packages from the dotfiles, see manifest-kalus.scm
   (map (cut str "--load-path=" dtf <>)
        '("/guix/common" "/guix/systems/common" "/guix/home/common"))

   (bstx-load-path)

   (map guix-preserve-exact
        '("dev"
          "dgx"
          "dbstx"
          "dsps"
          "dtf"
          "gpgPubKey"
          "GPG_TTY"
          ))

   ;; Projects to work on
   (map guix-share
        (list
         "/home/bost/dev/notes"
         (str dsps "/..") dgx (str dev "/fnc")
         dbstx
         dtf
         ))

   ;; Ad hoc options from the command line, e.g. projects to work on
   (cli-options)

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
   (append
    (list "env")
    (guix-gpg-environment)
    )

   (list "bash" "--noprofile" "--rcfile" ".bash_profile" "-i")))

(exit
 (call-with-guix-gpg-home
  dir-dot-gnupg-container
  (lambda (gpg-home)
    ;; Emulate the kalus machine: the dotfiles select per-host configuration
    ;; by hostname, e.g. `host-kalus?'.
    (guix-shell-run (args gpg-home) #:hostname host-kalus))))
