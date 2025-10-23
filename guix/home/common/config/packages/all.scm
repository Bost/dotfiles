(define-module (config packages all)
  #:use-module (utils)
  #:use-module (settings)
  #:use-module (memo)
  #:use-module (gnu)     ; provides use-package-modules
  ;; some packages may clash with (rde packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix pkg:)
  #:use-module ((bost gnu packages emacs-xyz) #:prefix bst:)
  ;; provides: spacemacs-packages
  #:use-module ((bost gnu packages space-needed) #:prefix bst:)
  #:use-module ((bost gnu packages gnome) #:prefix bst:)
  ;; provides: specification->package
  #:use-module (gnu packages)
  #:use-module (bost gnu packages guake)
  #:use-module (config channels channel-defs)

  ;; for inferior-pkg-in-channel : beg
  #:use-module (guix packages)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  ;; #:use-module (guix profiles) ;; probably not needed
  ;; for inferior-pkg-in-channel : end

  ;; Following is needed b/c an inferior version of signal-desktop is used
  #:use-module (nongnu packages messaging)
  #:use-module (nongnu packages firmware) ; fwupd-nonfree

  #:use-module (srfi srfi-1)  ; list-processing procedures
  ;; simple & compact notation for specializing any subset of the parameters of
  ;; a procedure. e.g. cut
  #:use-module (srfi srfi-26)
  )

(define m (module-name-for-logging))
(evaluating-module)

(use-package-modules
 maths base file web-browsers haskell-apps uml vnc rdesktop
 w3m dns bash rust-apps shellutils admin shells version-control rsync tmux
 compression vim audio gnuzilla inkscape rust graphviz texlive chromium
 kde-systemtools kde-utils gnome gtk libreoffice spice terminals lxqt xorg
 xdisorg xfce android linux aspell autotools algebra llvm cmake curl textutils
 video package-management fonts haskell gnupg tls bootloaders guile-xyz guile gv
 hardware samba web image cpp libusb toys lsof networking ncurses node disk
 pulseaudio perl php pkg-config search databases pv python-xyz python
 virtualization racket readline mp3 texinfo freedesktop cdrom lua emacs-xyz
 elixir tree-sitter agda idris emacs text-editors patchutils java glib maven
 mail messaging irc commencement gcc clojure machine-learning cups scanner
 file-systems librewolf libcanberra security-token firmware
 )

(define (email-in-emacs-packages)
  (list
   ;; find & view emails in Maildir format, extract attachments etc.
   mu    ;; mu generates autoloads for "mu4e", which is for treemacs-mu4e

   ;; isync/mbsync: command-line tool for two-way synchronization of mailboxes
   isync

   pkg:emacs-mbsync
   bst:emacs-mu4e-alert
   bst:emacs-helm-mu
   ))
(testsymb 'email-in-emacs-packages)

(define (packages-from-additional-channels)
  "Packages from additional channels?
Including these packages in the `home-packages-to-install' causes:
   error: <package-name>: unknown package
when called from the Emacs Geiser REPL by ,use or ,load"
  (append
   (list
    ;; downloads signal-desktop_6.14.0_amd64.deb 101.9MiB
    (@(nongnu packages messaging) signal-desktop)
    ;; Program launcher for idle X sessions
    xautolock
    ;; Fake keyboard/mouse input, window management, and more
    xdotool

    ;; Display graphical dialog boxes from shell scripts
    zenity)

   (list
    (@(nongnu packages clojure) babashka)
    (@(nongnu packages clojure) clojure-lsp)

    (@(nongnu packages clojure) leiningen)
    ;; guix weather --system=x86_64-linux --substitute-urls='https://substitutes.nonguix.org' firefox
    ;; /var/guix/profiles/per-user/bost/guix-profile-266-link/bin/firefox
    ;; See also module (bost manifest-set-operations)
    (@(nongnu packages mozilla) firefox)
    (@(bost gnu packages clojure) clojure-tools) ;; 1.12.0.1488
    #|
    (@(games packages factorio) factorio) ;; temporarily disabled, install it using:
    guix package --load-path=$dev/games --install=factorio
    set experimentalVersion @1.1.78 # set --erase experimentalVersion
    guix package --load-path=$dev/games --install=factorio$experimentalVersion
    |#
    )))
(testsymb 'packages-from-additional-channels)

(define (kde-dependent-packages)
  "KDE dependencies are slow to compile"
  (list
   konsole
   krusader))
(testsymb 'kde-dependent-packages)

(define (webkitgtk-based-browsers)
  "Simple browsers based on WebKit/GTK+"
  (list
   ;; Micro-browser framework extensible by Lua using the WebKit web content
   ;; engine and the GTK+ toolkit
   luakit

   ;; Mostly keyboard driven and modal like the Vim, easily configurable during
   ;; runtime
   vimb

   ;; Supports the XEmbed protocol which makes embedable it in another
   ;; application. Can be pointed to another URI by setting its XProperties
   surf
   ))

(define (large-packages-ecke)
  "Large packages, slow to build, graft, download, etc. See `guix size <package>`.
TODO implement: Show warning & don't compile if substitutes are not present."
  (append
   (list
    ;; 35.8MiB
    audacity

    ;; Rebranded Mozilla Thunderbird email client. Optionally install also
    ;; libotr the Off-the-Record (OTR) Messaging Library and Toolkit. See
    ;; Thunderbird console Ctrl-Shift-j
    ;; 48.8MiB; ie. thunderbird
    icedove

    ;; Vector graphics editor. ~93MiB
    inkscape
    )
   ;; (list
   ;;  ;; Embeddable TeX/LaTeX engine
   ;;  tectonic

   ;;  ;; Graphviz to LaTeX converter
   ;;  dot2tex

   ;;  ;; Complete TeX Live distribution. May take too long to graft
   ;;  texlive)
   ))
(testsymb 'large-packages-ecke)

(define (rust-development-packages)
  (list
   ccid                 ; PC/SC driver for USB smart card devices, driver for Minilector Evo
   opensc               ; (Some other) Tools and libraries related to smart cards
   pcsc-lite            ; Middleware to access a smart card using PC/SC
   pcsc-tools           ; Smart cards and PC/SC tools; pcsc_scan for debugging
   pkg-config           ; Helper tool used when compiling applications and libraries
   rust                 ; Compiler for the Rust programming language
   (@(past-crates packages crates-io) rust-cargo) ; Package manager for Rust
   rust-pcsc-sys-1      ; Low-level bindings to the PC/SC C API. rust-pcsc-sys not available
   rust-pkg-config-0.3  ; Library to run the pkg-config system tool. rust-pkg-config not available
   rust-pcsc-2          ; Bindings to the PC/SC API for smart card communication. rust-pcsc not available

   emacs-flymake-clippy ; Flymake backend for Clippy
   bst:emacs-rustic     ; Rust development environment for Emacs
   ))

(define (large-packages-edge-ecke)
  "Large packages, slow to build, graft, download, etc."
  (list
   ;; Default browser. GNU version of the Firefox. Includes the `geckodriver'
   ;; command, which can be useful for automated web testing.
   ;; See also icecat-minimal
   icecat ;; 1839.7 MiB

   librewolf ; Custom version of Firefox. Better privacy, security and freedom
   qemu      ; Machine emulator and virtualizer ;; 688 MiB

   ;; ungoogled-chromium ; 285MiB; supports WebUSB; doesn't start

   ;; openjdk-17.0.3  199.5MiB
   ;; openjdk-17.0.3-doc  9.6MiB
   ;; openjdk-17.0.3-jdk  275.9MiB
   ;; in total ~485 MiB

   ;; specifying only 'openjdk' causes java.lang.ClassNotFoundException:
   ;; jdk.javadoc.doclet.Doclet
   (list openjdk "jdk")

   ;; Java development kit. OpenJDK built with IcedTea build harness
   ;; icedtea
   ;; ; ~240MiB
   ))
(testsymb 'large-packages-edge-ecke)

#|
,use (guix scripts size)
(guix-size "firefox")
|#

(define (basic-packages)
  (define f (format #f "~a [basic-packages]" m))
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (p) (format #t "~a done\n" f) p)
    )
   (list
    bash
    bash-completion ; Bash completions for common commands
    bat

    direnv
    ;; read hardware information from the BIOS
    dmidecode

    ;; exa deprecated by eza in the daaedc9ab3; TODO the package is aliased but the binary is not
    eza

    fd
    fish
    git

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

    ;; transport layer security library, implements SSL, TLS, DTLS
    gnutls

    ;; Command-line JSON processor
    jq

    ;; Read-write access to NTFS file systems
    ntfs-3g

    ;; ripgrep@13.0.0 can be used a workaround for:
    ;;   SPC * not working with ripgrep 14
    ;;   https://github.com/syl20bnr/spacemacs/issues/16200
    ;; When using the inferior mechanism to pull in the ripgrep@13.0.0 the build
    ;; process (triggered via `guix pull`'), takes too long.
    ;; To avoid lengthy build times, the bst:ripgrep can be used instead.
    ;; However, the bst:ripgrep doesn't build since the upstream introduced a
    ;; new Rust packaging model.
    ;; https://guix.gnu.org/blog/2025/a-new-rust-packaging-model/
    ;; See also `(setq helm-ag-use-grep-ignore-list nil)' in the Emacs init.el
    ;; bst:ripgrep
    ripgrep

    rsync

    ;; S.M.A.R.T.  harddisk control and monitoring tools
    smartmontools

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

    ;; udisksctl
    udisks

    unzip

    ;; Tools for working with USB devices
    usbutils

    vim
    zip
    zstd ;; Zstandard real-time compression algorithm
    )))
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
   nix                ; The Nix package manager
   nixfmt             ; Opinionated formatter for Nix
   bst:emacs-nix-mode ; Emacs major mode for editing Nix expressions
   ))
(testsymb 'nix-packages)

(define (irc-packages)
  "See https://www.slant.co/topics/1323/~best-irc-clients-for-linux"
  (list
   hexchat     ; Graphical IRC client
   ;; weechat  ; Extensible chat client
   ;; irssi    ; Extensible terminal-based IRC client
   ))

(define (encryption-packages)
  (list
   ;; file-based encryption implemented as a mountable FUSE overlay filesystem
   gocryptfs

   ;; Qt/C++ GUI front end to sshfs, +ecryptfs, cryfs, gocryptfs, encfs,
   ;; fscrypt, securefs +based encrypted folders
   sirikali
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
   automake    ; for `make doc/guix.info`
   bc          ; Arbitrary precision numeric processing language

   ;; specifying only 'bind' leads to "Wrong type argument in position 1 ..."
   (list isc-bind "utils")

   ;; Contains mkisofs, which can create an hybrid ISO-9660/JOLIET/HFS/UDF
   ;; filesystem-image with optional Rock Ridge attributes. See also xorriso
   ;; cdrtools

   clang
   ;; clojure-tools ;; 1.11.1.1413
   clusterssh
   cmake
   curl
   dos2unix
   drawing     ; basic image editor
   fastboot    ; Android image flasher: upload file system images
   ffmpeg
   file        ; File type guesser
   flatpak
   font-adobe-source-code-pro
   font-gnu-freefont
   font-gnu-unifont
   fuse

   ;; See
   ;; https://forum.systemcrafters.net/t/updating-firmware-with-fwupdmgr/1766
   ;; https://issues.guix.gnu.org/60065
   ;; https://www.reddit.com/r/GUIX/comments/xjjmtr/fwupd_gives_another_service_has_claimed_the_dbus/
   fwupd ; Daemon to allow session software to update firmware
   fwupd-nonfree
   fwupd-efi ; EFI executable used by uefi-capsule plugin in fwupd
   ;; dfu-util ; download / upload firmware to / from device

;;; In Emacs the "native-compiler-error (libgccjit.so: error invoking
;;; gcc driver)":
;;;     ld: cannot find crtbeginS.o: No such file or directory
;;;     ld: cannot find -lgcc
;;;     ld: cannot find -lgcc_s
;;;     ld: cannot find -lgcc_s
;;;     libgccjit.so: error: error invoking gcc driver
;;; doesn't come up if the 11.3.0 is used. See also:
;;; - CC and CMAKE_C_COMPILER environment definition
;;; - https://lists.gnu.org/archive/html/guix-devel/2020-03/msg00256.html
;;;   https://gcc.gnu.org/onlinedocs/jit/internals/index.html#environment-variables
;;;   https://issues.guix.gnu.org/57086#9
;;;
;;; In (list <package> <something>) the <something> is a
;;; package-output not a package-version.
;;; Can't precisely specify the module and version with e.g.:
;;;   (specification->package (@(gnu packages commencement) gcc-toolchain) "11.3.0")
;;;   (specification->package (@(gnu packages gcc) libgccjit) "11.3.0")
   #;(specification->package "gcc-toolchain@12.3.0")
   #;(specification->package "libgccjit@12.3.0")
   gcc-toolchain
   libgccjit

   (list git "gui")
   ghc
   (list glib "bin")
   graphviz
   grub

   ;; specifying only 'guile' leads to "error: guile: unbound variable"
   guile-3.0
   (list guile-3.0 "debug")

   guile-hall   ; to build guile projects

   ;; Emacs with CUA mode, Geiser, tool bar icons to evaluate Guile buffers,
   ;; support for Guile's picture language, code completion, a simple mode line,
   ;; etc.
   guile-studio

   gv
   gvfs         ; GIO virtual file system; user mounts

   ;; Guix Workflow Language: Workflow management extension for Guix's language
   ;; for package management.
   ;; "Automate the execution of programs in scientific workflows"
   ;; gwl

   htop
   hwinfo
   iniparser
   inxi
   ispell
   jmtpfs
   libavc1394
   libavif
   libconfini

   ;; guix compilation: `make --jobs=24 check TESTS="tests/guix-daemon.sh"`
   libgcrypt

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
   lynx      ; Text Web Browser
   gnu-make  ; former (specification->package "make@4.3")
   maven
   mcron
   ;; mlt    ; Author, manage, and run multitrack audio/video compositions
   mtr       ; My TraceRoute: Network diagnostic tool combine traceroute + ping
   ncurses
   network-manager
   nmap

;;; TODO put ~/.npm-packages on PATH only if npm, i.e. node is installed
;;; See also ~/.npm, ~/.npmrc, ~/node_modules
   ;; node ;; this installs only 10.24.1
   node-lts ;; LTS Long Time Support

   openssl
   parted
   pavucontrol ; PulseAudio volume control
   perl
   php

   ;; GnuPG's interfaces to passphrase input
   pinentry
   pinentry-gtk2
   pinentry-tty

   pkg-config
   plocate
   portaudio
   ;; postgresql ; object-relational database
   pulseaudio
   inotify-tools ; for $dotf/bin/battery-notifier
   libnotify     ; for $dotf/bin/battery-notifier
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
   python-docx   ; Edit Microsoft Word documents in Python
   racket
   readline
   recutils      ; Manipulate plain text files as databases
   rlwrap
   ;; scsh          ; Unix shell embedded in Scheme
   strace
   taglib
   tealdeer      ; tldr "Too Long Didn't Read" written in Rust
   texinfo
   tig
   tree
   tzdata
   udiskie
   uniutils      ; Find out what is in a Unicode file

   ;; Viewer for graphviz dot files; display package dependency graph.
   xdot          ; guix graph coreutils | xdot -

   ;; Create, manipulate, burn ISO-9660 file systems; see also cdrtools
   xorriso

   netcat          ; Networking tool. Provides nc
   yt-dlp          ; youtube-dl fork focused on adding new features
   youtube-viewer  ; Search & play YT videos in a native player
   vlc             ; Audio and video player and framework

   tesseract-ocr               ; OCR Optical character recognition engine
   tesseract-ocr-tessdata-fast ; Fast versions of trained LSTM models
   gimagereader                ; Qt front-end to tesseract-ocr

   surfraw       ; Unix command line interface to the www
   w3m           ; Text-mode web browser
   wol           ; wakeonlan
   ))
(testsymb 'rest-packages)

(define (other-gui-packages)
  (list
   gdm

   ;; Low-level GNOME configuration system. Backend to GSettings
   dconf
   dconf-editor

   ;; GNOME's document viewer: PDF, PostScript, DjVu, TIFF and DVI
   evince
   ;; gksudo ;; not available in the Guix package repository

   ;; gparged and mtools are installed system-wide
   #;gparted    #| disk partition |#
   #;mtools     #| used by gparted |#

   gsettings-desktop-schemas
   gtk

   ;; On ecke in generation 1661
   ;; /gnu/store/znrni9c6mjx45ps4j0jkrbgl6rvc6s2p-libreoffice-25.2.3.2
   ;; https://ci.guix.gnu.org/search?query=spec%3Amaster+system%3Ax86_64-linux+libreoffice
   ;; doesn't build due to failing build of the firebird-3.0.13 dependency - see https://ci.guix.gnu.org/build/13148158/details
   libreoffice

   ;; Manage encryption keys and passwords in the GNOME keyring
   seahorse

   ;; Graphical console client for virtual machines using SPICE or VNC
   virt-viewer

   ;; share the clipboard and guest display resolution scaling on graphical
   ;; console window resize.
   spice-vdagent  ;; shared clipboard works for Ubuntu but doesn't work for NixOS

   ;; https://www.freedesktop.org/wiki/Software/xdg-utils/ - probably not needed
   ;; xdg-utils  ;; in gnu/packages/freedesktop.scm

   ;; https://issues.guix.gnu.org/59825
   ;; guix shell -NC flatpak-xdg-utils --preserve='^DBUS_SESSION_BUS_ADDRESS$' -- xdg-open "<https://guix.gnu.org>"

   ;; $ time eza -abghHliS --color=always --time-style=full-iso /gnu/store
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

   ;; thread 'main' panicked at /tmp/guix-build-alacritty-0.13.1.drv-0/source/guix-vendor/rust-xkbcommon-dl-0.4.1.tar.gz/src/x11.rs:59:28:
   ;; Library libxkbcommon-x11.so could not be loaded.
   ;; note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
   alacritty          ;;  4.393s; no drop-down; no splits; no tabs; e.g.: alacritty -o font.size=8
   ;; xfce4-terminal  ;;  9,998s; has --drop-down; has context menu; already present, no splits
   ;; yakuake         ;;        ; doesn't work: The name org.kde.kglobalaccel was not provided by any .service files
   guake              ;; 10,176s; has --drop-down; has context menu; already present, has splits
   ;; tilda           ;;  9.256s; drop down with F1 by default; has tabs; no splits
   qterminal          ;;  8,720s; drop down opens new process (no xfce4 integration?); has splits; has tabs; has context-menu
   ;; tilix           ;;        ; can't see a shit, the text (foreground color) is too dark
   ;; xterm           ;; 17.341s; has nothing, too basic
   ;; lxterminal      ;;  9.022s; has context-menu; no drop-down; no splits; has tabs
   ;; cool-retro-term ;; 25.256s; is cool!

   ;; wezterm ; not available in Guix yet 2025-08; sudo flatpak install org.wezfurlong.wezterm
   ;; ghostty ; not available in Guix yet 2025-08

   neovim

   ;; guvcview doesn't compile:
   ;;   uvc_h264.c:1035:17: error: implicit declaration of function ‘avcodec_close’
   ;; guvcview ; control webcam, capture videos and images

   ;; llama-cpp ; Port of Facebook's LLaMA model in C/C++

   sound-theme-freedesktop ; Audio samples for use as a desktop sound theme
   ))
(testsymb 'other-gui-packages)

(define (printer-scanner-packages)
  (list
   bst:simple-scan   ; Document and image scanner
   hplip-minimal ; Hewlett-Packard printer drivers
   ))
(testsymb 'printer-scanner-packages)

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

   ;; Set various user preference options of the X server display.
   ;; xset
   ))
(testsymb 'xorg-packages)

(define* (remote-desktop-packages #:key is-server)
  "Client packages for remote desktop access"
  (if is-server
      (list
       xrdp ;; Remote Desktop Protocol server; access to the entire desktop
       xpra ;; Remote access to individual applications or full desktops
       ;; xorgxrdp ;; Xorg drivers for xrdp; doesn't compile:
;;; https://github.com/neutrinolabs/xorgxrdp/issues/189
;;; rdpClientCon.c: In function ‘rdpClientConProcessMsgClientInfo’:
;;; rdpClientCon.c:981:31: error: ‘struct xrdp_client_info’ has no member named ‘monitorCount’
       )
      (list
       ;; rdesktop
       remmina

       ;; tigervnc-client also provides extensions for advanced authentication
       ;; methods and TLS encryption
       tigervnc-client
       )))

(define (xfce-packages)
  (list
   xfce4-clipman-plugin
   xfce4-netload-plugin ; traffic indicator
   xfce4-notifyd
   xfce4-screensaver
   xfce4-screenshooter
   xfce4-settings

   ;; in gnu/packages/xfce.scm
   thunar-volman        ; Removable media manager for Thunar; USB disks etc.

   ;; xfce4-volumed-pulse ;  XFCE volume keys daemon
   xfce4-pulseaudio-plugin
   ))
(testsymb 'xfce-packages)

(define (inferior-packages)
  "The original, i.e. non-inferior packages must not be present in the
home-profile. Comment them out.

FIXME the inferior-packages are installed on every machine"
  (define f (format #f "~a [inferior-packages]" m))

  (define* (inferior-packages-in-channel #:key channels-fun inferior-packages)
    (map (lambda (pkg-commit)
;;; pattern matching doesn't work for: list cons. It works for (values ... ...)
;;; somehow ???
;;; (let* [(a b (list 1 2))] (+ a b))  ; => Syntax error
;;; (let* [(a b (cons 1 2))] (+ a b))  ; => Syntax error
           (let [(package (car pkg-commit))
                 (commit (cadr pkg-commit))]
             ((comp
               first
               (cut lookup-inferior-packages <> package)
               inferior-for-channels
               channels-fun)
              commit)))
         inferior-packages))

  ((comp
    (partial remove unspecified?)
    flatten
    (partial map (partial apply inferior-packages-in-channel)))
   (list
    (list
     #:channels-fun (comp list (partial channel-guix #:commit)
                          (lambda (p) (format #t "~a 0. ~a\n" f p) p))
     #:inferior-packages
     (list
      ;; (list "icedove" "71f0676a295841e2cc662eec0d3e9b7e69726035")
      ;; (list "virglrenderer" "fec2fb89bb5dacc14ec619cd569278af34867e3d")

      ;; last commit fe60fe4fe0193eec0f66a1c5cf0b7ad6e416c9df containing ripgrep@13.0.0;
      ;; next commit 33313d57b97d3f2567037313133c1b9d565ba042; gnu: ripgrep: Update to 14.0.3.
      ;; (list "ripgrep" "fe60fe4fe0193eec0f66a1c5cf0b7ad6e416c9df")
      ))
    (list
     #:channels-fun (comp (partial cons* (channel-guix))
                          list
                          (partial channel-nonguix #:commit))
     #:inferior-packages
     (list
      ;; (list "signal-desktop" "65d23d2579b54bb5d52609bf6c34d2faafc8a6cf")
      ;; (list "firefox" "24f10c70518ae0eeaf77332bf15f70790e981d84")
      )))))
(testsymb 'inferior-packages)

(define (devel-guile-ide-arei-packages)
  (list
   emacs-arei          ; Guile IDE
   bst:emacs-plantuml-mode ; Edit and preview PlantUML diagrams
   plantuml            ; Draw UML diagrams from simple textual description
   guile-next
   guile-ares-rs       ; Asyncronous Reliable Extensible Sleek RPC Server
   ))
(testsymb 'devel-guile-ide-arei-packages)

(define (devel-packages)
  (append
   ;; (fennel-devel-packages)
   ;; (chez-scheme-devel-packages)
   ;; (elixir-devel-packages)
   ;; (agda-devel-packages)
   (email-in-emacs-packages)
   (devel-guile-ide-arei-packages)
   ;; remove packages from this list if their inferior version should be used
   (list
;;; Make sure that emacs is also in the default profile, i.e. installed by
;;;   guix install emacs
;;; otherwise the:
;;;   No such file or directory /home/bost/.guix-profile/share/emacs/site-lisp
;;; gets triggered. See https://issues.guix.gnu.org/issue/52002
    emacs
    emacs-bash-completion ; Bash completion for the shell buffer
    ;; emacs-native-comp doesn't compile. Ups
    ;; (@(flat packages emacs) emacs-native-comp) ;; version: 28.2.50-205.ae9bfed

    bst:emacs-gptel
    ;; emacs-next       ;; 29.0.92
    ;; emacs-next-pgtk  ;; 29.0.92
    bst:emacs-with-editor ; for using Emacsclient as EDITOR

    emacs-geiser
    emacs-geiser-guile
    bst:emacs-guix
    emacs-detached    ; Launch and manage detached processes
    emacs-crdt        ; Real-time collaborative editing environment
    ;; Used in ? crafted emacs ?
    emacs-elisp-demos ; add Elisp function examples into `*Help*' buffers
    bst:emacs-helpful
    emacs-keycast
    meld              ; compare files, directories and working copies
    leafpad           ; simple editor to use when emacs doesn't work
    (list git "send-email")
    pinentry          ; needed to sign commits
    pwclient          ; CLI client for Patchwork patch tracking tool (*.patch files)
    ;; octave

    ;; Static analysis for shell scripts
    shellcheck ;; prevent "shellcheck not available ..." in *bash-ls::stderr*
    ;; See also emacs-flymake-shellcheck: Flymake backend for Bash/Sh powered by
    ;; ShellCheck

    ;; Vim plugin for structured editing of Lisp S-expressions
    vim-paredit ;; the support for very large files is not great
    )))
(testsymb 'devel-packages)

(define-public (home-packages-to-install)
  (define f (format #f "~a [home-packages-to-install]" m))
  ;; (format #t "~a Starting…\n" f)
  ((comp
    ;; (lambda (p) (format #t "~a done\n" f) p)
    ;; (lambda (p) (format #t "~a 6. (length p): ~a\n" f (length p)) p)
    (partial append (inferior-packages))
    ;; (lambda (p) (format #t "~a 5.\n~a\n" f p) p)
    ;; (lambda (p) (format #t "~a 4. (length p): ~a\n" f (length p)) p)
    (lambda (lst)
      (if (or (is-system-edge))
          (append
           ;; (map (comp list specification->package) (video-packages))
           ;; TODO check ‘all-the-icons’ in the /home/bost/.local/share/fonts/
           ;; and call (all-the-icons-install-fonts) when installing emacs
           (remote-desktop-packages #:is-server #t)
           (list
            acpi    ; Information on ACPI devices: battery & temperature
            blueman ; GTK+ Bluetooth manager

            ;; Linux Bluetooth protocol stack, provides bluetootctl, NOT(!)
            ;; installed via bluetooth-service-type
            bluez
            bluez-alsa ; Bluetooth ALSA backend
            tlp        ; Power management / battery life
            )
           lst)
          lst))
    ;; (lambda (p) (format #t "~a 3. (length p): ~a\n" f (length p)) p)
    (lambda (lst)
      (if (or (is-system-ecke))
          (append
           ;; (map (comp list specification->package) (video-packages))
           ;; (large-packages-ecke)
           (remote-desktop-packages #:is-server #f)
           lst)
          lst))
    ;; (lambda (p) (format #t "~a 2. (length p): ~a\n" f (length p)) p)
    (lambda (lst)
      (if (or (is-system-edge) (is-system-ecke))
          (append
           (nix-packages)
           ;; (rust-development-packages)
           (large-packages-edge-ecke)
           ((comp
             ;; (lambda (p) (format #t "~a 1.\n~a\n" f (pretty-print->string p)) p)
             ;; (lambda (p) (format #t "~a 0. (length p): ~a\n" f (length p)) p)
             )
            (bst:spacemacs-packages) ;; pulls-in ~430 additional packages
            )
           ((comp
             ;; (lambda (p) (format #t "~a 1.\n~a\n" f (pretty-print->string p)) p)
             ;; (lambda (p) (format #t "~a 0. (length p): ~a\n" f (length p)) p)
             )
            (printer-scanner-packages))
           lst)
          lst))
    ;; (lambda (p) (format #t "~a 1. (length p): ~a\n" f (length p)) p)
    (lambda (lst)
      (if (or (is-system-edge) (is-system-ecke) (is-system-geek))
          (append
           (packages-from-additional-channels)
           (devel-packages)
           (kde-dependent-packages)
           (other-gui-packages)
           (irc-packages)
           (encryption-packages)
           (rest-packages)
           (xfce-packages)
           (xorg-packages)
           ;; (webkitgtk-based-browsers)
           lst)
          lst))
    ;; (lambda (p) (format #t "~a 0. (length p): ~a\n" f (length p)) p)
    )
   (basic-packages)))
(testsymb 'home-packages-to-install)

(module-evaluated)
