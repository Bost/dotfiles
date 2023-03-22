(define-module (cfg packages all)
  #:use-module (cfg packages spguimacs all)
  #:use-module (common settings)
  #:use-module (gnu packages)
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
  (list
   "leiningen"
   "babashka"
   "firefox"
   ;; "factorio" ;; temporarily disabled, install it using:
   ;; cd ~/dev/games && guix package --load-path=./ --install=factorio
   "signal-desktop"
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
   "git:send-email"
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
    "pinentry" ;; needed to sign commits
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
   "gcc"
   "gcc-toolchain"
   "gdm"
   "ghc"
   "glib:bin"
   "glibc-locales"
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
   ;; maven is required by emacs, however the 3.8.6 from %default-channels is buggy so:
   ;; (A) either the 3.8.5 installed using the inferior mechanism
   ;; https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html
   ;; (B) or the fix proposed by https://issues.guix.gnu.org/57749 is used from
   ;; a local guix repo activated in the ~/.config/guix/channels.scm
   "maven"
   ;; "bost/packages/maven"
   ;; "(@ (bost packages maven) maven)"
   ;; '(@ (bost packages maven) maven)

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
   "taglib"
   "texinfo"
   "thunar-volman"
   "tig"
   "tree"
   "tzdata"
   "udiskie"
   "uniutils"
   "usbutils"
   "virt-viewer"

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
   "xsel"

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
(testsymb 'rest-packages)

(define inferior-racket
  ;; An inferior representing the above revision.
  (inferior-for-channels
   (list (channel
          (name 'guix)
          (url "https://git.savannah.gnu.org/git/guix.git")
          (commit
           ;; "<predecessor-sha1>"
           "e1290c0d43cb2916a5908f15b3211911ee257968")))))
(testsymb 'inferior-racket)

(define (packages-to-install)
;;; TODO make it support inferior packages
;;; https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html
;;; TODO packages should accept expressions like the -e, e.g.
;;;   guix package                        -e '(@ (bost packages maven) maven)'
;;;   guix package --install-from-expression='(@ (bost packages maven) maven)'

    ((compose
      (lambda (pkgs)
        (format #t "~a ~a packages to install\n" m (length pkgs))
        ;; (format #t "~a\n~a\n" m pkgs)
        pkgs)
      (partial append
               ;; in the $dotf/guix/home/cfg/packages/all.scm
               ;; comment out "racket" in the (rest-packages)
               (list
                ;; in the $dotf/guix/home/cfg/packages/all.scm
                ;; comment out "racket" in the (rest-packages)
                (first (lookup-inferior-packages inferior-racket "racket")))
               #;
               ((compose
                 first
                 (partial lookup-inferior-packages inferior-racket))
                "racket"))
      (lambda (pkgs)
        ;; The spguimacs-packages should be installed only on the ecke-machine,
        ;; i.e. no need to install any emacs-packages on any other machine
        (if (home-ecke-config)
            (append
             (list
              ;; Won't work probably
              ;; emacs-eziam-theme-emacs
              ;; emacs-lsp-python-ms
              ;; emacs-moe-theme
              ;; emacs-slim-mode
              ;; emacs-zop-to-char
              ;; emacs-font-utils
              ;; emacs-lush

              (@ (bost packages emacs-xyz) emacs-eziam-theme-emacs) ;; doesn't work
              (@ (bost packages emacs-xyz) emacs-lsp-python-ms)
              (@ (bost packages emacs-xyz) emacs-moe-theme)
              (@ (bost packages emacs-xyz) emacs-slim-mode)
              (@ (bost packages emacs-xyz) emacs-zop-to-char)
              (@ (bost packages emacs-xyz) emacs-font-utils)
              (@ (bost packages emacs-xyz) emacs-lush) ;; doesn't work

              ;; emacs-column-enforce-mode
              ;; emacs-pippel
              ;; emacs-railscasts-theme
              ;; emacs-color-theme-sanityinc-tomorrow
              ;; emacs-gruvbox
              ;; emacs-sphinx-doc

              (@ (bost packages emacs-xyz) emacs-column-enforce-mode)
              (@ (bost packages emacs-xyz) emacs-pippel)
              (@ (bost packages emacs-xyz) emacs-railscasts-theme)
              (@ (bost packages emacs-xyz) emacs-color-theme-sanityinc-tomorrow) ;; doesn't work
              (@ (bost packages emacs-xyz) emacs-gruvbox) ;; doesn't work
              (@ (bost packages emacs-xyz) emacs-sphinx-doc)
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
            (packages-from-additional-channels)
            ;; (spguimacs-packages)
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
