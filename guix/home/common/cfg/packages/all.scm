(define-module (cfg packages all)
  #:use-module (settings)
  #:use-module (utils)
  #:use-module (memo)
  ;; provides: use-package-modules
  #:use-module (gnu)
  #:use-module (cfg packages spguimacs all)
  ;; some packages may clash with (rde packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix pkg:)
  ;; provides: specification->package
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  ;; provides: first take remove delete-duplicates append-map etc.
  #:use-module (srfi srfi-1)
  )

(define m (module-name-for-logging))
;; (format #t "~a evaluating module ...\n" m)

(use-package-modules
 rdesktop
 w3m
 dns
 bash
 rust-apps
 shellutils
 admin
 shells
 version-control
 rsync
 tmux
 compression
 vim
 audio
 gnuzilla
 inkscape
 rust
 graphviz
 texlive
 chromium
 kde-systemtools
 kde-utils
 gnome
 gtk
 libreoffice
 spice
 terminals
 lxqt
 xorg
 xdisorg
 xfce

 android
 linux
 aspell
 autotools
 algebra
 llvm
 cmake
 curl
 textutils
 video
 package-management
 fonts
 haskell
 gnupg
 tls
 bootloaders
 guile-xyz
 guile
 gv
 hardware
 samba
 web
 image
 cpp
 libusb
 toys
 lsof
 networking
 ncurses
 node
 disk
 pulseaudio
 perl
 php
 pkg-config
 search
 databases
 pv
 python-xyz
 python
 virtualization
 racket
 readline
 mp3
 texinfo
 freedesktop
 cdrom
 lua
 emacs-xyz
 elixir
 tree-sitter
 agda
 idris
 emacs
 text-editors
 patchutils
 java
 glib
 maven
 mail
 messaging
 irc
 )

(define (email-in-emacs-packages)
  (list
   mu    ;; mu generates autoloads for "mu4e", which is for treemacs-mu4e
   isync ;; isync/mbsync is a command-line tool for two-way synchronization of mailboxes
   emacs-mbsync
   emacs-mu4e-alert
   emacs-helm-mu
   ))
(testsymb 'email-in-emacs-packages)

(define (packages-from-additional-channels)
  "Packages from additional channels?
Including these packages in the `packages-to-install' causes:
   error: <package-name>: unknown package
when called from the Emacs Geiser REPL by ,use or ,load"
  (list
   (@(bost packages clojure) clojure)
   (@(bost packages clojure) clojure-lsp)
   (@(bost packages clojure) clojure-tools)
   (@(bost packages babashka) babashka)

   ;; downloads signal-desktop_6.14.0_amd64.deb 101.9MiB
   (@(nongnu packages messaging) signal-desktop)
   (@(nongnu packages clojure) leiningen)
   (@(nongnu packages mozilla) firefox)
   #|
   (@(games packages factorio) factorio) ;; temporarily disabled, install it using:
   guix package --load-path=$dev/games --install=factorio
   set experimentalVersion @1.1.78 # set --erase experimentalVersion
   guix package --load-path=$dev/games --install=factorio$experimentalVersion
   |#
   ))
(testsymb 'packages-from-additional-channels)

(define (kde-dependent-packages)
  "KDE dependencies are slow to compile"
  (list
   konsole
   krusader))
(testsymb 'kde-dependent-packages)

(define (large-packages-ecke)
  "Large packages, slow to build, graft, download, etc."
  (list
   audacity ;; 35.8MiB

   ;; Rebranded Mozilla Thunderbird email client. Optionally install also
   ;; libotr the Off-the-Record (OTR) Messaging Library and Toolkit. See
   ;; Thunderbird console Ctrl-Shift-j
   icedove  ;; 48.8MiB

   ;; Vector graphics editor
   inkscape ;; ~93MiB

   tectonic ;; embeddable TeX/LaTeX engine

   ;; Graphviz to LaTeX converter
   dot2tex

   ;; complete TeX Live distribution
   texlive                 ; may take too long to graft
   ))
(testsymb 'large-packages-ecke)

(define (large-packages-edge-ecke)
  "Large packages, slow to build, graft, download, etc."
  (list
   ;; default browser
   icecat ;; 68.9MiB

   ;; rust downloads (see below) and then it needs to be build:
   ;;     rust-1.59.0  121.1MiB
   ;;     rust-1.59.0-cargo  3.2MiB
   ;;     rustc-1.60.0-src.tar.xz  63.6MiB
   rust ;; the 1.60 has to be build

   ungoogled-chromium

   ;; openjdk-17.0.3  199.5MiB
   ;; openjdk-17.0.3-doc  9.6MiB
   ;; openjdk-17.0.3-jdk  275.9MiB
   ;; in total ~485 MiB

   ;; specifying only 'openjdk' causes java.lang.ClassNotFoundException:
   ;; jdk.javadoc.doclet.Doclet
   (list openjdk "jdk")

   ;; Provides OpenJDK built with the IcedTea build harness
   ;; icedtea ; ~240MiB
   ))
(testsymb 'large-packages-edge-ecke)

(define (basic-packages)
  (list
   bash
   bat
   direnv
   dmidecode ;; read hardware information from the BIOS
   exa
   fd
   fish
   git
   (list git "gui")

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
   glibc
   glibc-locales

   gnupg
   gnutls ;; transport layer security library, implements SSL, TLS, DTLS

   rsync

   ;; performance monitoring: mpstat iostat tapestat cifsiostat pidstat sar sadc sadf sa
   ;; sysstat

   ;; terminal multiplexer, more popular and modern than 'screen'
   tmux

   ;; tmux-based terminal divider
   ;; * Split tmux window into multiple panes.
   ;; * Build command lines & execute them on the panes.
   ;; * Runnable from outside of tmux session.
   ;; * Runnable from inside of tmux session.
   ;; * Record operation log.
   ;; * Flexible layout arrangement for panes.
   ;; * Display pane title on each pane.
   ;; * Generate command lines from standard input (Pipe mode).
   tmux-xpanes

   unzip
   vim
   zip
   ))
(testsymb 'basic-packages)

(define (agda-devel-packages)
  (list
   agda
   emacs-agda2-mode
   ;; agda-ial ;; broken build
   ;; cedille ;; depends on agda-ial
   idris))
(testsymb 'agda-devel-packages)

(define (fennel-devel-packages)
  "Fennel: Lua + Lisp. For e.g. Factorio modding."
  (list
   lua
   fennel
   emacs-fennel-mode

   ;; Automatic formatting of Fennel code
   ;; fnlfmt ; doesn't compile
   ))
(testsymb 'fennel-devel-packages)

(define (chez-scheme-devel-packages)
  "See https://github.com/mnieper/scheme-macros"
  (list
   "chez-srfi"
   "chez-fmt"
   "chez-scheme"
   "emacs-geiser-chez"
   ;; Portable hygienic pattern matcher for Scheme
   "chez-matchable"))
(testsymb 'chez-scheme-devel-packages)

(define (elixir-devel-packages)
  "Elixir is dynamic, functional language. It leverages the Erlang VM"
  (list
   elixir
   emacs-alchemist         ;; not among the needed
   emacs-elixir-mode       ;; not among the needed
   emacs-eval-in-repl-iex  ;; is needed
   tree-sitter-elixir
   ))
(testsymb 'elixir-devel-packages)

(define (video-packages)
  (list
   ;; OpenGL and Vulkan implementations
   ;; "mesa"
   ;; "mesa:bin"
   ;; "mesa:out"

   ;; contains utility tools for Mesa: eglinfo, glxdemo, glxgears, glxheads,
   ;; glxinfo.
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

   ;; GLU, or OpenGL Utility Library provides some higher-level functionality
   ;; not provided by just OpenGL itself
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
(testsymb 'video-packages)

(define (nix-packages)
  (list
   nix    ;; The Nix package manager
   nixfmt ;; Opinionated formatter for Nix
   emacs-nix-mode ;; Emacs major mode for editing Nix expressions
   ))
(testsymb 'nix-packages)

(define (irc-packages)
  "See https://www.slant.co/topics/1323/~best-irc-clients-for-linux"
  (list
   hexchat ;; Graphical IRC client
   ;; weechat ;; Extensible chat client
   ;; irssi   ;; Extensible terminal-based IRC client
   ))

(define (rest-packages)
  (list
   adb
   alsa-utils
   android-ext4-utils
   android-file-transfer
   android-udev-rules
   asciinema
   aspell
   aspell-dict-de
   aspell-dict-en
   aspell-dict-fr
   autoconf
   bc

   ;; specifying only 'bind' leads to "Wrong type argument in position 1 ..."
   (list isc-bind "utils")

   ;; Contains mkisofs, which can create an hybrid ISO-9660/JOLIET/HFS/UDF
   ;; filesystem-image with optional Rock Ridge attributes. See also xorriso
   ;; cdrtools

   clang

   ;; Use the (bost packages clojure) definitions for clojure-related packages
   ;; clojure
   ;; clojure-tools

   clusterssh
   cmake
   curl
   dos2unix
   ffmpeg
   flatpak
   font-adobe-source-code-pro
   font-gnu-freefont
   font-gnu-unifont
   fuse

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

   ;; In (list <package> <something>) the <something> is a package-output not a
   ;; package-version.
   ;; Can't precisely specify the module and version with e.g.:
   ;;   (specification->package (@(gnu packages commencement) gcc-toolchain) "11.3.0")
   ;;   (specification->package (@(gnu packages gcc) libgccjit) "11.3.0")
   (specification->package "gcc-toolchain@11.3.0")
   (specification->package "libgccjit@11.3.0")

   ghc
   (list glib "bin")
   graphviz
   grub

   ;; specifying only 'guile' leads to "error: guile: unbound variable"
   guile-3.0

   guile-hall ;; to build guile projects
   guile-studio
   gv
   gvfs #| user mounts |#
   gwl
   htop
   hwinfo
   iniparser
   inxi
   ispell
   jmtpfs
   jq ;; json formatting
   libavc1394
   libavif
   libconfini
   libjpeg-turbo
   libmtp
   libtiff
   libtool
   libungif
   libxaw3d
   libxpm
   lolcat
   lshw
   lsof
   (specification->package "make@4.3")
   maven
   mcron
   mercurial
   mlt
   mtr
   ncurses
   network-manager
   nmap

;;; TODO put ~/.npm-packages on PATH only if npm, i.e. node is installed
;;; See also ~/.npm, ~/.npmrc, ~/node_modules
   ;; node ;; this installs only 10.24.1
   node-lts ;; LTS Long Time Support

   openssl
   parted
   pavucontrol
   perl
   php

   ;; GnuPG's interfaces to passphrase input
   pinentry
   pinentry-gtk2
   pinentry-tty

   pkg-config
   plocate
   portaudio
   postgresql
   pulseaudio
   pv
   pybind11

 ;;; `python' should not be installed `python' with `python-wrapper'.
 ;;; `python-wrapper' uses the `python' package as a propagated input
   ;; python

;;; `python-wrapper' enables invocation of python3 under under their usual
;;; names---e.g., `python' instead of `python3' or `pip' instead of `pip3'
   python-wrapper
   ;; specifying only 'python2' leads to "error: python2: unbound variable"
   python-2.7

   qemu
;;; TODO Auto-rebuild `search-notes' every time a new racket-version is build.
;;; This will happen automatically if `search-notes' is a proper Guix package.
   racket
   readline

   ;; Manipulate plain text files as databases
   recutils

   ripgrep
   rlwrap

   scsh ;; Unix shell embedded in Scheme
   strace
   taglib
   tealdeer ;; Rust implementation of the tldr "Too Long Didn't Read"
   texinfo
   tig
   tree
   tzdata
   udiskie
   uniutils
   usbutils

   ;; Interactive viewer for graphviz dot files. Useful to view package dependency graph.
   xdot ;; guix graph coreutils | xdot -

   ;; Create, manipulate, burn ISO-9660 file systems; see also cdrtools
   xorriso

   mps-youtube ;; yewtube forked from mps-youtube. Terminal based YT player and downloader. No API key required.
   youtube-dl
   yt-dlp ;; fork of youtube-dl with a focus on adding new features
   ;; youtube-viewer ;; search & play YT videos in a native player

   ;; tesseract-ocr               ;; OCR Optical character recognition engine
   ;; tesseract-ocr-tessdata-fast ;; Fast versions of trained LSTM models
   ;; gimagereader                ;; Qt front-end to tesseract-ocr

   surfraw ;; Unix command line interface to the www
   w3m     ;; Text-mode web browser
   ))
(testsymb 'rest-packages)

(define (other-gui-packages)
  (list
   gdm
   dconf
   dconf-editor
   evince
   ;; gksudo ;; not available in the Guix package repository

   ;; gparged and mtools are installed system-wide
   #;gparted    #| disk partition |#
   #;mtools     #| used by gparted |#

   gsettings-desktop-schemas
   gtk
   ;; libreoffice ;; replaced by inferior version
   ;; Manage encryption keys and passwords in the GNOME keyring
   seahorse

   virt-viewer

   ;; share the clipboard and guest display resolution scaling on graphical
   ;; console window resize.
   spice-vdagent  ;; shared clipboard works for Ubuntu but doesn't work for NixOS

   ;; https://www.freedesktop.org/wiki/Software/xdg-utils/ - probably not needed
   ;; xdg-utils  ;; in gnu/packages/freedesktop.scm

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
   ;; kitty           ;;  5.443s; no drop-down; no splits; in fish no linux icon in the prompt; tabs are strange
   ;;
   ;; terminator      ;;  8.916s; no drop-down; has splits
   alacritty          ;;  4.393s; no drop-down; no splits; no tabs
   ;; xfce4-terminal  ;;  9.905s; has --drop-down; has context menu; already present, no splits
   ;; yakuake         ;;        ; doesn't work: The name org.kde.kglobalaccel was not provided by any .service files
   ;; guake           ;;        ; not packaged for Guix
   ;; tilda           ;;  9.256s; drop down with F1 by default; has tabs; no splits
   qterminal          ;;  6.147s; drop down opens new process (no xfce4 integration?); has splits; has tabs; has context-menu
   ;; tilix           ;;        ; can't see a shit, the text (foreground color) is too dark
   ;; xterm           ;; 17.341s; has nothing, too basic
   ;; lxterminal      ;;  9.022s; has context-menu; no drop-down; no splits; has tabs
   ;; cool-retro-term ;; 25.256s; is cool!

   neovim
   ))
(testsymb 'other-gui-packages)

(define (xorg-packages)
  (list
   ;; Xorg XKB configuration files - probably not needed in Xfce
   xkeyboard-config

   ;; Modify keymaps and button mappings on X server
   ;; xmodmap

   ;; Command line interface to X11 Resize, Rotate, and Reflect (RandR)
   ;; See the command:
   ;;    xrandr --output HDMI-1 --auto --left-of DP-1 --auto
   ;; See under:
   ;;    Xfce Settings Manager -> Session and Startup -> Dual display for two monitors
   ;; the command:
   ;;    xfconf-query --create --type string --channel displays --property /Schemes/Apply --set a93ccfa35c66cf3bc719e997533c55d24167cdc9
   xrandr

   ;; Print contents of X events: move, resize, type in, click in, etc. See
   ;; wev the Wayland event viewer
   xev

   ;; Manipulate X selection, i.e. the clipboard from the command line.
   xsel
   ))
(testsymb 'xorg-packages)

(define* (remote-desktop-packages #:key is-server)
  (if is-server
      (list
       xrdp ;; Remote Desktop Protocol server; access to the entire desktop
       xpra ;; Remote access to individual applications or full desktops
       xorgxrdp ;; Xorg drivers for xrdp
       )
      (list ;; the clients
       rdesktop)))

(define (xfce-packages)
  (list
   ;; TODO add ~/.config/xfce4/xfconf/xfce-perchannel-xml/ to the home config
   xfce4-clipman-plugin
   xfce4-netload-plugin ;; traffic indicator
   xfce4-notifyd
   xfce4-screensaver
   xfce4-screenshooter
   xfce4-settings
   thunar-volman ;; in gnu/packages/xfce.scm

   ;; xfce4-volumed-pulse ;;  XFCE volume keys daemon
   xfce4-pulseaudio-plugin
   ))
(testsymb 'xfce-packages)

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
(testsymb 'inferior-package-in-guix-channel)

(define (inferior-pkgs pkgs)
  "The original, i.e. non-inferior packages must not be present in the
home-profile. Comment them out."
  ((comp
    (partial append pkgs)
    ;; (lambda (pkgs) (format #t "~a\ninferior-pkgs: ~a\n" m pkgs) pkgs)
    (partial map (partial apply inferior-package-in-guix-channel)))
   (list
    ;; the e18af936ff85442a841886c9434f862fb595a8b2 leads to failing
    ;; compilation: No package 'mdds-2.0'
    (list "libreoffice"           "a4db19d8e07eeb26931edfde0f0e6bca4e0448d3")

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
   ;; (fennel-devel-packages)
   ;; (chez-scheme-devel-packages)
   ;; (elixir-devel-packages)
   ;; (agda-devel-packages)
   (email-in-emacs-packages)
   ;; remove packages from this list if their inferior version should be used
   (list
;;; Make sure that emacs is also in the default profile, i.e. installed by
;;;   guix install emacs
;;; otherwise the:
;;;   No such file or directory /home/bost/.guix-profile/share/emacs/site-lisp
;;; gets triggered. See https://issues.guix.gnu.org/issue/52002
    emacs
    emacs-gptel
    ;; emacs-next       ;; 29.0.92
    ;; emacs-next-pgtk  ;; 29.0.92
    emacs-with-editor ;; for using Emacsclient as EDITOR

    emacs-geiser
    emacs-geiser-guile
    emacs-guix
    ;; Launch and manage detached processes
    emacs-detached
    ;; Real-time collaborative editing environment
    emacs-crdt

    leafpad           ;; simple editor to use when emacs doesn't work
    (list git "send-email")
    pinentry ;; needed to sign commits
    pwclient ;; CLI client for Patchwork patch tracking tool (*.patch files)
    )))
(testsymb 'devel-packages)

(define-public (packages-to-install)
;;; TODO make it support inferior packages
;;; https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html
;;; TODO packages should accept expressions like the -e, e.g.
;;;   guix package                        -e '(@ (bost packages maven) maven)'
;;;   guix package --install-from-expression='(@ (bost packages maven) maven)'
  ((comp
    (lambda (pkgs)
      #|
      (format #t "~a\n~a\n" m
              (let* [(search-space
                      '(
                        "emacs-haskell-snippets"
                        "emacs-yasnippet"
                        "emacs-yasnippet-snippets"
                        ))]
                ((comp
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
    ;; (lambda (p) (format #t "~a 5.\n~a\n" p) p))
    ;; (lambda (p) (format #t "~a 4. (length p): ~a\n" m (length p)) p)
    (lambda (pkgs)
      (if (or (is-system-edge))
          (append
           ;; (map (comp list specification->package) (video-packages))
           (list
            (@(bost packages emacs-xyz) emacs-farmhouse-light-mod-theme)
            (@(bost packages emacs-xyz) emacs-tweaks)
            )
           (remote-desktop-packages #:is-server #t)
           ;; TODO check ‘all-the-icons’ in the  ‘/home/bost/.local/share/fonts/’ and call (all-the-icons-install-fonts) when installing emacs
           ;; pulls-in ~430 additional packages
           ;; (spguimacs-packages)
           pkgs)
          pkgs))
    ;; (lambda (p) (format #t "~a 3. (length p): ~a\n" m (length p)) p)
    (lambda (pkgs)
      (if (or (is-system-ecke))
          (append
           ;; (map (comp list specification->package) (video-packages))
           ;; pulls-in ~430 additional packages
           (spguimacs-packages)
           (large-packages-ecke)
           (remote-desktop-packages #:is-server #f)
           pkgs)
          pkgs))
    ;; (lambda (p) (format #t "~a 2. (length p): ~a\n" m (length p)) p)
    (lambda (pkgs)
      (if (or (is-system-edge) (is-system-ecke))
          (append (large-packages-edge-ecke) pkgs)
          pkgs))
    ;; (lambda (p) (format #t "~a 1. (length p): ~a\n" m (length p)) p)
    (lambda (pkgs)
      (if (or (is-system-ecke) (is-system-geek))
          (append (packages-from-additional-channels) pkgs)
          pkgs))
    (lambda (pkgs)
      (if (or (is-system-edge) (is-system-ecke) (is-system-geek))
          (append
           (devel-packages)
           (kde-dependent-packages)
           (other-gui-packages)
           (irc-packages)
           (rest-packages)
           (xfce-packages)
           (xorg-packages)
           pkgs)
          pkgs))
    ;; (lambda (p) (format #t "~a 0. (length p): ~a\n" m (length p)) p)
    )
   (basic-packages)))
(testsymb 'packages-to-install)

;; (format #t "~a module evaluated\n" m)
