(define-module (cfg packages all)
  #:use-module (cfg packages spguimacs all)
  #:use-module ((bost packages emacs-xyz) #:prefix bste:)
  ;; some packages may clash with (rde packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix pkg:)
  #:use-module (bost packages clojure) ;; provides clojure-lsp
  #:use-module ((bost packages xdisorg) #:prefix bstx:) ;; provides xsel
  #:use-module (common settings)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  #:use-module (utils)
  #:use-module (memo)

  #:export (
            packages-to-install
            ))

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(define (packages-from-additional-channels)
  "Packages from additional channels?
Including these packages in the `packages-to-install' causes:
   error: <package-naae>: unknown package
when called from the Emacs Geiser REPL by ,use or ,load"
  (append
   (packages-from-additional-channels-base)
   (list
    "signal-desktop" ;; downloads signal-desktop_6.14.0_amd64.deb 101.9MiB
    )))

(define (packages-from-additional-channels-base)
  "Packages from additional channels?
Including these packages in the `packages-to-install' causes:
   error: <package-name>: unknown package
when called from the Emacs Geiser REPL by ,use or ,load"
  (list
   "leiningen"
   "babashka"
   "firefox"
   #|
   "factorio" ;; temporarily disabled, install it using:
   guix package --load-path=$dev/games --install=factorio
   set experimentalVersion @1.1.78 # set --erase experimentalVersion
   guix package --load-path=$dev/games --install=factorio$experimentalVersion
   |#
   ))

(define (kde-dependent-packages)
  "KDE dependencies are slow to compile"
  (list
   "konsole"
   "krusader"))

(define (large-packages)
  "Large packages, slow to build, graft, download, etc."
  (list
   "audacity" ;; 35.8MiB
   "inkscape" ;; ~93MiB

   ;; rust downloads (see below) and then it needs to be build:
   ;;     rust-1.59.0  121.1MiB
   ;;     rust-1.59.0-cargo  3.2MiB
   ;;     rustc-1.60.0-src.tar.xz  63.6MiB
   "rust" ;; the 1.60 has to be build

   "tectonic" ;; embeddable TeX/LaTeX engine

   ;; Graphviz to LaTeX converter
   "dot2tex"

   ;; complete TeX Live distribution
   "texlive"                 ; may take too long to graft

   "ungoogled-chromium"

   ;; openjdk-17.0.3  199.5MiB
   ;; openjdk-17.0.3-doc  9.6MiB
   ;; openjdk-17.0.3-jdk  275.9MiB
   ;; in total ~485 MiB

   ;; causes java.lang.ClassNotFoundException: jdk.javadoc.doclet.Doclet
   ;; "openjdk"
   "openjdk:jdk"

   ;; "icedtea" ; ~240MiB; provides OpenJDK built with the IcedTea build harness
   ))

(define (basic-packages)
  (list
   "bash"
   "bat"
   "direnv"
   "exa"
   "fd"
   "fish"
   "git"
   "git:gui"

;;; glibc and glibc-locales are needed to prevent:
;;;     guile: warning: failed to install locale
;;; also following steps may be needed:
;;; sudo su
;;; guix archive --authorize < $(find /root/.cache/ -name ci.guix.gnu.org.pub)
;;; exit             # exit the root shell
;;; guix pull        # not sure if this is needed, but it shouldn't do any harm
;;;
;;; Here the `guix archive ...' reports:
;;; guix archive: warning: replacing symbolic link /etc/guix/acl with a regular file
;;; hint: On Guix System, add all `authorized-keys' to the `guix-service-type' service of your `operating-system' instead.
   "glibc"
   "glibc-locales"

   "rsync"
   "unzip"
   "vim"
   "zip"
   ))

(define (agda-devel-packages)
  (list
   "agda"
   "emacs-agda2-mode"
   ;; "agda-ial" ;; broken build
   ;; "cedille" ;; depends on agda-ial
   "idris"))

(define (devel-packages)
  (append
   (fennel-devel-packages)
   (chez-scheme-devel-packages)
   (elixir-devel-packages)
   (agda-devel-packages)
   (list
    "emacs"
    "emacs-next"
    "emacs-next-pgtk"
    "emacs-with-editor"
    "git:send-email"
    "pinentry" ;; needed to sign commits
    "pwclient"
    )))

(define (fennel-devel-packages)
  "Fennel: Lua + Lisp. For e.g. Factorio modding."
  (list
   "lua"
   "fennel"
   "emacs-fennel-mode"

   ;; Automatic formatting of Fennel code
   ;; "fnlfmt" ; doesn't compile
   ))

(define (chez-scheme-devel-packages)
  "See https://github.com/mnieper/scheme-macros"
  (list
   "chez-srfi"
   "chez-fmt"
   "chez-scheme"
   "emacs-geiser-chez"
   ;; Portable hygienic pattern matcher for Scheme
   "chez-matchable"))

(define (elixir-devel-packages)
  "See https://github.com/mnieper/scheme-macros"
  (list
   "elixir"
   "emacs-elixir-mode"
   "tree-sitter-elixir"
   "emacs-alchemist"
   "emacs-eval-in-repl-iex"
   ))

(define (rest-packages)
  (list
   "adb"
   "alsa-utils"
   "android-ext4-utils"
   "android-file-transfer"
   "android-udev-rules"
   "asciinema"
   "aspell"
   "aspell-dict-de"
   "aspell-dict-en"
   "aspell-dict-fr"
   "autoconf"
   "bc"
   "bind:utils"

   ;; Contains mkisofs, which can create an hybrid ISO-9660/JOLIET/HFS/UDF
   ;; filesystem-image with optional Rock Ridge attributes. See also xorriso
   ;; "cdrtools"

   "clang"
   "clojure"
   "clojure-lsp" ;; from (bost packages clojure)
   "clojure-tools"
   "clusterssh"
   "cmake"
   "curl"
   "dconf"
   "dconf-editor"
   "dos2unix"
   "evince"
   "ffmpeg"
   "flatpak"
   "font-adobe-source-code-pro"
   "font-gnu-freefont"
   "font-gnu-unifont"
   "fuse"
   ;; Make sure the CC environment variable has the value `(which gcc)'
   ;; See home-environment-variables-service-type
   "gcc-toolchain"
   "gdm"
   "ghc"
   "glib:bin"
   "gnupg"
   "gnutls"
   ;; "gksudo" ;; not available in the Guix package repository
   "gparted"
   "graphviz"
   "grub"
   "gsettings-desktop-schemas"
   "gtk"
   "guile"
   "guile-hall" ;; to build guile projects
   "guile-studio"
   "gv"
   "gvfs"
   "gwl"
   "htop"
   "hwinfo"
   "iniparser"
   "inxi"
   "ispell"
   "jmtpfs"
   "jq" ;; json formatting
   "libavc1394"
   "libavif"
   "libconfini"
   "libjpeg-turbo"
   "libmtp"
   "libreoffice"
   "libtiff"
   "libtool"
   "libungif"
   "libxaw3d"
   "libxpm"
   "lolcat"
   "lshw"
   "lsof"
   "make"
   "maven"
   "mcron"
   "mercurial"
   "mesa-utils"
   "mlt"
   "mtr"
   "ncurses"
   "network-manager"
   "nmap"
   "node"
   "openssl"
   "parted"
   "pavucontrol"
   "perl"
   "php"
   "pinentry"
   "pkg-config"
   "plocate"
   "portaudio"
   "postgresql"
   "pulseaudio"
   "pv"
   "pybind11"
   "python"
   "python2"
   "qemu"
;;; TODO Auto-rebuild `search-notes' every time a new racket-version is build.
;;; This will happen automatically if `search-notes' is a proper Guix package.

;;; racket 8.8 returns:
;;;     $ racket
;;;     munmap_chunk(): invalid pointer
;;;     Aborted
;;; racket 8.7 (pulled via inferior mechanism) works fine.
   ;; "racket"

   "readline"
   "recutils"
   "ripgrep"
   "rlwrap"
   "screen"
   "scsh"
   "seahorse"
   "spice-vdagent"
   "strace"
   "taglib"
   "texinfo"
   "thunar-volman"
   "tig"
   "tree"
   "tzdata"
   "udiskie"
   "uniutils"
   "usbutils"
   ;; "virt-viewer"

   ;; https://www.freedesktop.org/wiki/Software/xdg-utils/ - probably not needed
   ;; "xdg-utils"

   ;; Wayland event viewer
   ;; "xev"

   ;; Xorg XKB configuration files - probably not needed in Xfce
   "xkeyboard-config"

   ;; Modify keymaps and button mappings on X server
   ;; "xmodmap"

   ;; Tiling window manager
   ;; "xmonad"

   ;; Create, manipulate, burn ISO-9660 file systems; see also cdrtools
   "xorriso"

   ;; Command line interface to X11 Resize, Rotate, and Reflect (RandR)
   ;; See the command:
   ;;    xrandr --output HDMI-1 --auto --left-of DP-1 --auto
   ;; See under:
   ;;    Xfce Settings Manager -> Session and Startup -> Dual display for two monitors
   ;; the command:
   ;;    xfconf-query --create --type string --channel displays --property /Schemes/Apply --set a93ccfa35c66cf3bc719e997533c55d24167cdc9
   "xrandr"

   ;; Manipulate X selection, i.e. the clipboard from the command line.
   ;; "xsel" ;; see bstx:...

   "youtube-dl"
   ))

(define (xfce-packages)
  (list
   ;; TODO add ~/.config/xfce4/xfconf/xfce-perchannel-xml/ to the home config
   "xfce4-clipman-plugin"
   "xfce4-netload-plugin" ;; traffic indicator
   "xfce4-notifyd"
   "xfce4-screensaver"
   "xfce4-screenshooter"
   "xfce4-settings"
   ))

(define (inferior-package-in-guix-channel package commit)
  "Returns an inferior representing the `commit' (predecessor-sha1) revision."
  (first
   (lookup-inferior-packages
    (inferior-for-channels
     (list (channel
            (name 'guix)
            (url "https://git.savannah.gnu.org/git/guix.git")
            (commit commit))))
    package)))

(define (inferior-pkgs pkgs)
  ((compose
    (partial append pkgs)
    ;; (lambda (pkgs) (format #t "~a\ninferior-pkgs: ~a\n" m pkgs) pkgs)
    (partial map (partial apply inferior-package-in-guix-channel)))
   (list
    (list "virt-viewer"        "87ce7a6f71a0d337e47125ad7e8349f9225c7bf1")
    (list "racket"             "e1290c0d43cb2916a5908f15b3211911ee257968"))))

(define (packages-to-install)
;;; TODO make it support inferior packages
;;; https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html
;;; TODO packages should accept expressions like the -e, e.g.
;;;   guix package                        -e '(@ (bost packages maven) maven)'
;;;   guix package --install-from-expression='(@ (bost packages maven) maven)'

    ((compose
      (lambda (pkgs)
        #|
        (format #t "~a\n~a\n" m
                ((compose
                  (partial filter
                           (lambda (p)
                             (and (list? p)
                                  ;; eq? doesn't work
                                  (string=? "emacs-guix"
                                            (package-name (car p))))))
                  ;; we have a colorful mix here:
                  (partial map
                           (lambda (p)
                             (cond
                              [(list? p)
                               (format #t "list    ~a: filter: ~a\n"
                                       (package-name (car p))
                                       (and (list? p)
                                            (string=? "emacs-guix"
                                                      (package-name (car p)))))]
                              [(string? p)  (format #t "string  ~a\n" p)]
                              [(package? p) (format #t "package ~a\n" p)]
                              [(record? p)  (format #t "record  ~a\n" p)]
                              [else         (format #t "else    ~a\n" p)])
                             p))
                  identity)
                 pkgs))
        |#
        (format #t "~a ~a packages to install\n" m (length pkgs))
        pkgs)
      inferior-pkgs
      (lambda (pkgs)
        (if (home-ecke-config)
            (append
             (list
              bstx:xsel

              pkg:emacs-geiser
              pkg:emacs-geiser-guile
              pkg:emacs-guix

              ;; bstc:clojure-tools
              bste:emacs-copilot
              ;; below are good
              bste:emacs-company-web
              bste:emacs-web-completion-data
              bste:emacs-centered-cursor-mode
              bste:emacs-company-statistics
              bste:emacs-json-navigator
              bste:emacs-eziam-themes
              bste:emacs-tangotango
              bste:emacs-helm-cider-history
              bste:emacs-flx
              bste:emacs-twilight-bright
              ;; bste:emacs-haskell-snippets
              bste:emacs-lsp-haskell
              bste:emacs-darkmine
              bste:emacs-helm-css-scss
              ;; bste:emacs-auto-yasnippet
              bste:emacs-composer
              bste:emacs-soft-stone
              bste:emacs-twilight-anti-bright
              bste:emacs-erc-social-graph
              bste:emacs-hlint-refactor
              bste:emacs-chocolate
              bste:emacs-soft-charcoal
              bste:emacs-clues
              bste:emacs-planet
              bste:emacs-occidental
              bste:emacs-gruber-darker

              bste:emacs-vi-tilde-fringe
              bste:emacs-popwin
              ;; bste:emacs-paradox
              bste:emacs-lsp-volar

              bste:emacs-eziam-themes
              bste:emacs-lsp-python-ms
              bste:emacs-moe-theme
              bste:emacs-slim-mode
              bste:emacs-zop-to-char
              bste:emacs-font-utils
              bste:emacs-lush ;; doesn't work
              bste:emacs-pythonic

              bste:emacs-color-theme-sanityinc-tomorrow ;; doesn't work
              bste:emacs-xcscope
              )
             pkgs)
            pkgs))
      (partial map (compose identity list
;;; TODO difference specification->package+output, specification->package ?
                               specification->package+output)))
     (cond
        [(home-lukas-config)
         (begin
           ;; (format #t "(home-lukas-config)\n")
           (basic-packages))]
        ;; The spguimacs-packages should be installed only on the ecke- and
        ;; geek-machines, i.e. no need to install it elsewhere.
        [(home-ecke-config)
         (begin
           ;; (format #t "(home-ecke-config)\n")
           (append
            (basic-packages)
            (devel-packages)
            (rest-packages)
            (xfce-packages)
            (kde-dependent-packages)
            (large-packages)
            (packages-from-additional-channels)
            (spguimacs-packages)
            ))]
        [(home-geek-config)
         (begin
           ;; (format #t "(home-geek-config)\n")
           (append
            (basic-packages)
            (devel-packages)
            (rest-packages)
            (xfce-packages)
            (kde-dependent-packages)
            ;; (large-packages)
            (packages-from-additional-channels-base)
            (spguimacs-packages)
            ))]
        [#t
         (error
          (format #f "hostname '~a' must be one of the: ~a\n"
                  (hostname-memoized) (string-join hostnames)))])
    ))
(testsymb 'packages-to-install)

(define (repl)
  (use-modules (cfg packages all))
  (load (string-append (getenv "dotf") "/guix/home/cfg/packages/all.scm")))

;; (format #t "~a module evaluated\n" m)
