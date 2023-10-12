(define-module (cfg packages all)
  #:use-module (settings)
  #:use-module (utils)
  #:use-module (memo)

  #:use-module (cfg packages spguimacs all)
  ;; some packages may clash with (rde packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix pkg:)
  ;; provides clojure related packages
  #:use-module ((bost packages clojure) #:prefix bstc:)
  ;; provides xsel
  #:use-module ((bost packages xdisorg) #:prefix bstx:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  ;; first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)

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

   ;; Rebranded Mozilla Thunderbird email client. Optionally install also
   ;; "libotr" the Off-the-Record (OTR) Messaging Library and Toolkit. See
   ;; Thunderbird console Ctrl-Shift-j
   "icedove"  ;; 48.8MiB

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
   "dmidecode" ;; read hardware information from the BIOS
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

   ;; performance monitoring: mpstat iostat tapestat cifsiostat pidstat sar sadc sadf sa
   ;; "sysstat"

   ;; terminal multiplexer, more popular and modern than 'screen'
   "tmux"

   ;; tmux-based terminal divider
   ;; * Split tmux window into multiple panes.
   ;; * Build command lines & execute them on the panes.
   ;; * Runnable from outside of tmux session.
   ;; * Runnable from inside of tmux session.
   ;; * Record operation log.
   ;; * Flexible layout arrangement for panes.
   ;; * Display pane title on each pane.
   ;; * Generate command lines from standard input (Pipe mode).
   "tmux-xpanes"

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

(define (video-packages)
  (list
   ;; OpenGL and Vulkan implementations
   ;; "mesa"
   ;; "mesa:bin"
   ;; "mesa:out"

   ;; contains utility tools for Mesa: eglinfo, glxdemo, glxgears, glxheads, glxinfo.
   ;; "mesa-utils"

   ;; Proprietary NVIDIA driver
   ;; Use the transformation option `--with-graft=mesa=nvda' to use the NVIDIA
   ;; driver with a package that requires mesa
   ;; "nvda"

   ;; EGLStream-based Wayland external platform
   ;; "egl-wayland"

   ;; "mesa-opencl"
   ;; "mesa-opencl:out"
   ;; "mesa-opencl:bin"

   ;; "mesa-opencl-icd"
   ;; "mesa-opencl-icd:out"
   ;; "mesa-opencl-icd:bin"

   ;; GLU, or OpenGL Utility Library provides some higher-level functionality not provided by just OpenGL itself
   ;; "glu"

   ;; Nonfree firmware for older AMD graphics chips
   ;; "radeon-firmware"

   ;; The Direct Rendering Infrastructure - userspace library
   ;; "libdrm"

   ;; Wayland compositor compatible with i3
   "sway"

   ;; Window-switcher for the sway window manager
   ;; "swayr"

   ;; Sway Fork with extra options and effects
   ;; "swayfx"

   ;; Swallow windows on swaywm
   ;; "swayhide"

   ;; Notification daemon with a graphical interface
   ;; "swaynotificationcenter"

   ;; Screen locking utility for Wayland compositors with effects
   ;; "swaylock-effects"

   ;; Screen locking utility for Wayland compositors
   "swaylock"

   ;; Idle management daemon for Wayland compositors
   "swayidle"

   ;; Screen wallpaper utility for Wayland compositors
   "swaybg"

   ;; Wayland bar for Sway and Wlroots based compositors
   ;; "waybar"

   ;; Waybar with experimental features
   ;; "waybar-experimental"

   ;; Screenshot utility for the Sway window manager
   ;; "grimshot"

   ;; Notification daemon for Sway
   ;; "avizo"

   ;; Launcher/menu program for wayland
   ;; "wofi"

   ;; Bare-bones Wayland-based greeter for `greetd'
   ;; "wlgreet"

   ;; Application launcher for Wayland
   ;; "tofi"

   ;; Grab and edit on the fly snapshots of a Wayland compositor
   ;; "swappy"

   ;; Application launchers for wlroots
   ;; "nwg-launchers"

   ;; Window-stacking compositor for Wayland
   ;; "labwc"

   ;; Reference implementation of a Wayland compositor
   ;; weston

   ;; Core Wayland window system code and protocol
   ;; "wayland"

   ;; Wayland protocols
   ;; "wayland-protocols"

   ;; QML based X11 and Wayland display manager
   ;; "sddm"
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

   ;; Use the (bost packages clojure) definitions for clojure-related packages
   ;; "clojure"
   ;; "clojure-lsp"
   ;; "clojure-tools"

   "clusterssh"
   "cmake"
   "curl"
   "dos2unix"
   "ffmpeg"
   "flatpak"
   "font-adobe-source-code-pro"
   "font-gnu-freefont"
   "font-gnu-unifont"
   "fuse"

   ;; Seems like the "native-compiler-error (libgccjit.so: error invoking gcc
   ;; driver)":
   ;;     ld: cannot find crtbeginS.o: No such file or directory
   ;;     ld: cannot find -lgcc
   ;;     ld: cannot find -lgcc_s
   ;;     ld: cannot find -lgcc_s
   ;;     libgccjit.so: error: error invoking gcc driver
   ;; doesn't come up if the 11.3.0 is used.
   ;; See also:
   ;; - CC and CMAKE_C_COMPILER environment definition
   ;; - https://lists.gnu.org/archive/html/guix-devel/2020-03/msg00256.html
   ;;   https://gcc.gnu.org/onlinedocs/jit/internals/index.html#environment-variables
   ;;   https://issues.guix.gnu.org/57086#9

   "gcc-toolchain@11.3.0"
   "libgccjit@11.3.0"

   "ghc"
   "glib:bin"
   "gnupg"
   "gnutls"
   "graphviz"
   "grub"
   "guile"
   "guile-hall" ;; to build guile projects
   "guile-studio"
   "gv"
   "gvfs" #| user mounts |#
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
   "mlt"
   "mtr"
   "ncurses"
   "network-manager"
   "nmap"

;;; TODO put ~/.npm-packages on PATH only if npm, i.e. node is installed
;;; See also ~/.npm, ~/.npmrc, ~/node_modules
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

 ;;; `python' should not be installed `python' with `python-wrapper'.
 ;;; `python-wrapper' uses the `python' package as a propagated input
   ;; "python"

;;; `python-wrapper' enables invocation of python3 under under their usual
;;; names---e.g., `python' instead of `python3' or `pip' instead of `pip3'
   "python-wrapper"
   "python2"

   "qemu"
;;; TODO Auto-rebuild `search-notes' every time a new racket-version is build.
;;; This will happen automatically if `search-notes' is a proper Guix package.
   "racket"
   "readline"

   ;; Manipulate plain text files as databases
   "recutils"

   "ripgrep"
   "rlwrap"

   "scsh" ;; Unix shell embedded in Scheme
   "strace"
   "taglib"
   "texinfo"
   "tig"
   "tree"
   "tzdata"
   "udiskie"
   "uniutils"
   "usbutils"

   ;; Interactive viewer for graphviz dot files. Useful to view package dependency graph.
   "xdot" ;; guix graph coreutils | xdot -

   ;; Create, manipulate, burn ISO-9660 file systems; see also cdrtools
   "xorriso"

   "mps-youtube" ;; yewtube forked from mps-youtube. Terminal based YT player and downloader. No API key required.
   "youtube-dl"
   "yt-dlp" ;; fork of youtube-dl with a focus on adding new features
   ;; "youtube-viewer" ;; search & play YT videos in a native player

   ;; "tesseract-ocr"               ;; OCR Optical character recognition engine
   ;; "tesseract-ocr-tessdata-fast" ;; Fast versions of trained LSTM models
   ;; "gimagereader"                ;; Qt front-end to tesseract-ocr

   ))

(define (other-gui-packages)
  (list
   "gdm"
   "dconf"
   "dconf-editor"
   "evince"
   ;; "gksudo" ;; not available in the Guix package repository

   ;; gparged and mtools are installed system-wide
   #;"gparted"    #| disk partition |#
   #;"mtools"     #| used by gparted |#

   "gsettings-desktop-schemas"
   "gtk"
   "libreoffice"
   ;; Manage encryption keys and passwords in the GNOME keyring
   "seahorse"

   "virt-viewer"

   ;; share the clipboard and guest display resolution scaling on graphical
   ;; console window resize.
   "spice-vdagent"

   ;; https://www.freedesktop.org/wiki/Software/xdg-utils/ - probably not needed
   ;; "xdg-utils"  ;; in gnu/packages/freedesktop.scm

   ;; $ time exa -abghHliS --color=always --time-style=full-iso /gnu/store
   ;;
   ;; GPU-based terminal emulator:
   ;; * Offloads rendering to the GPU for lower system load and buttery smooth scrolling.  Uses threaded rendering to minimize input latency.
   ;; * Supports all modern terminal features: graphics (images), unicode, true-color, OpenType ligatures, mouse protocol, focus tracking, bracketed paste and several new terminal protocol extensions.
   ;; * Supports tiling multiple terminal windows side by side in different layouts without needing to use an extra program like tmux.
   ;; * Can be controlled from scripts or the shell prompt, even over SSH.
   ;; * Has a framework for Kittens, small terminal programs that can be used to extend kitty's functionality.  For example, they are used for Unicode input, hints, and side-by-side diff.
   ;; * Supports startup sessions which allow you to specify the window/tab layout, working directories and programs to run on startup.
   ;; * Allows you to open the scrollback buffer in a separate window using arbitrary programs of your choice.  This is useful for browsing the history comfortably in a pager or editor.
   ;; "kitty"           ;;  5.443s; no drop-down; no splits; in fish no linux icon in the prompt; tabs are strange
   ;;
   ;; "terminator"      ;;  8.916s; no drop-down; has splits
   "alacritty"          ;;  4.393s; no drop-down; no splits; no tabs
   ;; "xfce4-terminal"  ;;  9.905s; has --drop-down; has context menu; already present, no splits
   ;; "yakuake"         ;;        ; doesn't work: The name org.kde.kglobalaccel was not provided by any .service files
   ;; "guake"           ;;        ; not packaged for Guix
   ;; "tilda"           ;;  9.256s; drop down with F1 by default; has tabs; no splits
   "qterminal"          ;;  6.147s; drop down opens new process (no xfce4 integration?); has splits; has tabs; has context-menu
   ;; "tilix"           ;;        ; can't see a shit, the text (foreground color) is too dark
   ;; "xterm"           ;; 17.341s; has nothing, too basic
   ;; "lxterminal"      ;;  9.022s; has context-menu; no drop-down; no splits; has tabs
   ;; "cool-retro-term" ;; 25.256s; is cool!

   "neovim"
   ))

(define (xorg-packages)
  (list
   ;; Xorg XKB configuration files - probably not needed in Xfce
   "xkeyboard-config"

   ;; Modify keymaps and button mappings on X server
   ;; "xmodmap"

   ;; Command line interface to X11 Resize, Rotate, and Reflect (RandR)
   ;; See the command:
   ;;    xrandr --output HDMI-1 --auto --left-of DP-1 --auto
   ;; See under:
   ;;    Xfce Settings Manager -> Session and Startup -> Dual display for two monitors
   ;; the command:
   ;;    xfconf-query --create --type string --channel displays --property /Schemes/Apply --set a93ccfa35c66cf3bc719e997533c55d24167cdc9
   "xrandr"

   ;; Print contents of X events: move, resize, type in, click in, etc. See
   ;; "wev" the Wayland event viewer
   "xev"

   ;; Manipulate X selection, i.e. the clipboard from the command line.
   ;; "xsel" ;; see bstx:...
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
   "thunar-volman" ;; in gnu/packages/xfce.scm

   ;; "xfce4-volumed-pulse" ;;  XFCE volume keys daemon
   "xfce4-pulseaudio-plugin"
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
;;; virt-viewer 7.0 works fine
;;;    (list "virt-viewer"        "87ce7a6f71a0d337e47125ad7e8349f9225c7bf1")

;;; racket 8.8 returns:
;;;     $ racket
;;;     munmap_chunk(): invalid pointer
;;;     Aborted
;;; racket 8.7 works fine:
;;;    (list "racket"             "e1290c0d43cb2916a5908f15b3211911ee257968")

;;; emacs 28.2
    ;; (list "emacs"             "772eaa69f31457aa19ca4dc4ce755c791d722054")
    )))
(testsymb 'inferior-pkgs)

(define (devel-packages)
  (append
   (fennel-devel-packages)
   ;; (chez-scheme-devel-packages)
   (elixir-devel-packages)
   (agda-devel-packages)
   ;; remove packages from this list if their inferior version should be used
   (list
;;; Make sure that emacs is also in the default profile, i.e. installed by
;;;   guix install emacs
;;; otherwise the:
;;;   No such file or directory /home/bost/.guix-profile/share/emacs/site-lisp
;;; gets triggered. See https://issues.guix.gnu.org/issue/52002
    "emacs"
    "emacs-gptel"
    ;; "emacs-next"       ;; 29.0.92
    ;; "emacs-next-pgtk"  ;; 29.0.92
    "emacs-with-editor" ;; for using Emacsclient as EDITOR

    "emacs-geiser"
    "emacs-geiser-guile"
    "emacs-guix"

    "leafpad"           ;; simple editor to use when emacs doesn't work
    "git:send-email"
    "pinentry" ;; needed to sign commits
    "pwclient" ;; CLI client for Patchwork patch tracking tool (*.patch files)
    )))

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
              (let* [(search-space
                      '(
                        "emacs-haskell-snippets"
                        "emacs-yasnippet"
                        "emacs-yasnippet-snippets"
                        ))]
                ((compose
                  (partial
                   filter
                   ;; see also `string=?', `eq?', `equal?', etc.
                   ;; member uses `equal?'
                   (lambda (p)
                     (cond
                      [(list? p)    (member (package-name (car p))
                                            search-space)]
                      [(string? p)  (member (package-name p) search-space)]
                      [(package? p) (member (package-name p) search-space)]
                      [(record? p)  (member (inferior-package-name p)
                                            search-space)]
                      [else         (member (package-name p) search-space)])))
                  ;; we have a colorful mix here:
                  (partial
                   map
                   (lambda (p)
                     (cond
                      [(list? p)    (when (member (package-name (car p))
                                                  search-space)
                                      (format #t "list    ~a\n" p))]
                      [(string? p)  (when #t
                                      (format #t "string  ~a\n" p))]
                      [(package? p) (when #t
                                      (format #t "package ~a\n" p))]
                      [(record? p)  (when #t
                                      (format #t "record  ~a\n" p))]
                      [else         (when #t
                                      (format #t "else    ~a\n" p))])
                     p))
                  identity)
                 pkgs)))
      |#
      ;; (format #t "~a\n" (string-join (map (partial format #f "~a") pkgs) "\n"))
      (format #t "~a packages to install: ~a\n" m (length pkgs))
      pkgs)
    inferior-pkgs
    (lambda (pkgs)
      (if (or (is-system-ecke) (is-system-geek))
          (append
           (list
            bstc:clojure
            bstc:clojure-lsp
            bstc:clojure-tools
            bstx:xsel)
           pkgs)
          pkgs))
    (partial map (compose identity list
;;; TODO difference specification->package+output, specification->package ?
                          specification->package+output))
    ;; (lambda (v) (format #t "1\n~a\n" v) v)
    ;; TODO `eq?' works for "lua" but not for "emacs-popwin". WTF!?
    (partial remove (partial string= "emacs-popwin"))
    ;; (lambda (v) (map (lambda (p) (format #t "~a ~a\n" p (string? p))) v) v)
    ;; (lambda (v) (format #t "0\n~a\n" v) v)
    (partial
       append
       (cond
        [(is-system-lukas)
         (begin
           ;; (format #t "(is-system-lukas)\n")
           (list))]
        [(is-system-ecke)
         (begin
           ;; (format #t "(is-system-ecke)\n")
           (append
            (spguimacs-packages) ;; pulls in ~350 additional packages
            (devel-packages)
            (rest-packages)
            ;; (video-packages)
            (xfce-packages)
            (xorg-packages)
            (other-gui-packages)
            (kde-dependent-packages)
            (large-packages)
            (packages-from-additional-channels)
            (list)))]
        [(is-system-geek)
         (begin
           ;; (format #t "(is-system-geek)\n")
           (append
            (devel-packages)
            (rest-packages)
            (xfce-packages)
            (xorg-packages)
            (other-gui-packages)
            (kde-dependent-packages)
            ;; (large-packages)
            (packages-from-additional-channels-base)
            (list)))]
        [#t (error (format #f "hostname '~a' must be one of the: ~a\n"
                           (hostname-memoized) (string-join hostnames)))])))
   (basic-packages)))
(testsymb 'packages-to-install)

(define (repl)
  (use-modules (cfg packages all))
  (load (string-append (getenv "dotf") "/guix/home/cfg/packages/all.scm")))

;; (format #t "~a module evaluated\n" m)
