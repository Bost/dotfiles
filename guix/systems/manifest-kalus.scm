;; Emulates the kalus machine: its system-wide and home packages, shared with
;; `guix system reconfigure guix/systems/syst-kalus.scm' in the dotfiles, plus
;; the development tools.
(use-modules (dotf config packages kalus)) ; kalus-manifest

(concatenate-manifests
 (list
  (kalus-manifest)
  (specifications->manifest
   (append
    (list
     "bash"
     "bat"            ; `cat' clone with syntax highlighting and git integration
     "coreutils"
     "curl"
     "diffutils"      ; provides: diff, cmp, diff3, sdiff
     "difftastic"     ; provides: difft (structural diff tool)
     "eza"            ; for listing with `/home/bost/scm-bin/l`
     "fd"             ; Simple, fast and user-friendly alternative to find
     "findutils"      ; provides: find, updatedb, xargs
     "fish"
     "gawk"           ; provides: awk
     "git"
     "git-filter-repo"  ; rewrite git history (remove/transform commits)
     "git-delta"      ; Syntax-highlighting pager for git
     "gnupg"          ; required for signed git commits
     "grep"
     "guile"
     "gzip"           ; General file (de)compression (using lzw)
     "inetutils"      ; provides: hostname
     "jq"             ; Command-line JSON processor
     "less"
     "make"           ; build tool; needed for running test Makefiles
     "ncurses"
     "nss-certs"
     "patch"          ; apply patches to source files
     "perl"
     "procps"         ; provides: free pgrep pidof pkill pmap ps pwdx slabtop tload top vmstat w watch sysctl
     "python-wrapper" ; scripts run by claude
     "ripgrep"
     "rsync"
     "sed"
     "sd"             ; Intuitive find & replace CLI; replacement for `sed'. Claude can't work with `sed' very well
     "shellcheck"     ; shell-script validation (see file headers)
     "starship"       ; Fast and customizable shell prompt
     "strace"         ; System call tracer for Linux
     "tar"            ; archiving utility
     "tree"           ; Recursively list the contents of a directory
     "unzip"
     "w3m"            ; terminal browser; used when no graphical display is available
     "which"
     "zutils"         ; Utilities that transparently operate on compressed files: zcat zcmp zdiff zgrep
     "zip"            ; Compression and file packing utility

     "libzip"         ; C library for reading, creating, and modifying zip archives
     "sudo"
     ;; "which"
     ;; "direnv"
     ;; "help2man"
     "glibc-locales" ; All the locales supported by the GNU C Library

     ;; libgcrypt is needed for tests. E.g:
     ;; `make --jobs=24 check TESTS="tests/guix-daemon.sh"`
     "libgcrypt"      ; Cryptographic function library

     ;; guile-gcrypt is needed for tests
     "guile-gcrypt"   ; Cryptography library for Guile using Libgcrypt

     ;; guile-git is needed for tests (provides the (git) module used by (guix git))
     "guile-git"      ; Guile bindings for libgit2

     ;; guile-json is needed for `guix lint`
     "guile-json"     ; JSON reader/writer for Guile

     ;; "pinentry"    ; GnuPG's interface to passphrase input

     "github-cli"     ; GitHub's official command-line tool
     )
    ;; text editors
    (list
     "emacs-no-x"     ; console-only Emacs, no GUI toolkit
     "emacs-spacemacs"

     ;; Modern, legacy free, vim-like. Modal editing, multiple cursors/selections,
     ;; sam's structural regular expression based command language
     "vis"

     "neovim"
     ))
   )))
