(define-module (cfg packages-new)
  #:use-module (srfi srfi-1)

  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages android)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages video)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement) #| for gcc-toolchain |#
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages guile-xyz) #| mcron |#
  #:use-module (gnu packages glib)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages image)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libreoffice)
  #|
  #:use-module (gnu packages form)
  #:use-module (gnu packages )
  #:use-module (gnu packages )
  #:use-module (gnu packages )
  #:use-module (gnu packages )
  #:use-module (gnu packages )
  #:use-module (gnu packages )
  |#

  #:export (
            basic-profile-packages
            kde-dependent-packages
            packages-from-additional-channels
            slow-packages
            user-profile-packages
            ))

(define (packages-from-additional-channels)
  "Packages from additional channels?"
  (list
   "leiningen"
   "babashka"
   "firefox"
   ;; "factorio" ;; temporarily disabled, install it using:
   ;; cd ~/dev/games && guix package --load-path=./ --install=factorio
   ))

(define (kde-dependent-packages)
  "KDE dependencies are slow to compile"
  (list
   "konsole"
   "krusader"))

(define (slow-packages)
  "Packages slow to build, graft, download, etc."
  (list
   "audacity" ;; 35.8MiB
   "inkscape" ;; ~93MiB
   "tectonic" ;; embeddable TeX/LaTeX engine
   "texlive"                 ; may take too long to graft
   "texlive-latex-pdfpages"  ; may take too long to graft
   "ungoogled-chromium"

   ;; openjdk-17.0.3  199.5MiB
   ;; openjdk-17.0.3-doc  9.6MiB
   ;; openjdk-17.0.3-jdk  275.9MiB
   ;; in total ~485 MiB
   "openjdk"
   ;; hopefully included in openjdk
   "openjdk:jdk"
   ;; "icedtea" ; ~240MiB; provides OpenJDK built with the IcedTea build harness
   ))

(define (basic-profile-packages)
  (list
   ;; bash ;; is probably included already
   direnv
   emacs
   emacs-with-editor
   fish
   git

   ;; hopefully included in git
   ;; git:gui
   ;; git:send-email

   ;; guile ;; is probably included already
   rsync
   unzip
   vim
   xfce4-clipman-plugin
   xkeyboard-config
   zip
   ))

(define (user-profile-packages)
  (list
   ;; adb ;; Android Debug Bridge
   alsa-utils
   android-ext4-utils
   android-udev-rules
   asciinema
   aspell
   aspell-dict-de
   aspell-dict-en
   aspell-dict-fr
   autoconf
   bat
   bc

   bind
   ;; hopefully included in bind
   ;; bind:utils

   clang
   clojure-tools
   clusterssh
   cmake
   curl
   dconf
   dconf-editor
   dos2unix
   evince
   exa
   fd
   ffmpeg
   flatpak
   font-adobe-source-code-pro
   font-gnu-freefont
   font-gnu-unifont
   fuse
   gcc
   gcc-toolchain
   gdm
   ghc

   glib
   ;; glib:bin ;; hopefully in the glib

   glibc-locales
   gnupg
   gnutls
   gparted
   graphviz
   grub
   gsettings-desktop-schemas
   gtk
   guile-hall ;; to build guile projects
   guile-studio
   gv
   gvfs
   gwl
   htop
   hwinfo
   iniparser
   inxi
   ispell
   jmtpfs
   libavc1394
   libavif
   libconfini
   libjpeg-turbo
   libmtp
   libreoffice
   libtiff
   libtool
   libungif
   libxaw3d
   libxpm
   lolcat
   lshw
   lsof
   make
   ;; maven is required by emacs, however the 3.8.6 from %default-channels is buggy so:
   ;; (A) either the 3.8.5 installed using the inferior mechanism
   ;; https://guix.gnu.org/manual/devel/en/html_node/Inferiors.html
   ;; (B) or the fix proposed by https://issues.guix.gnu.org/57749 is used from
   ;; a local guix repo activated in the ~/.config/guix/channels.scm
   maven
   mcron ;; can't find the mcron module, huh!?!
   mercurial
   mesa-utils
   mlt
   mtr
   ncurses
   network-manager
   nmap
   node
   openssl
   parted
   pavucontrol
   perl
   php
   pinentry
   pkg-config
   plocate
   postgresql@13.6
   portaudio
   pulseaudio
   pv
   pybind11
   python
   python2
   qemu
   readline
   recutils
   ripgrep
   rlwrap
   rust
   screen
   scsh
   seahorse
   spice-vdagent
   taglib
   texinfo
   thunar-volman
   tig
   tree
   tzdata
   udiskie
   uniutils
   usbutils
   virt-viewer
   xdg-utils
   xev
   xfce4-notifyd
   xfce4-screenshooter
   xmodmap
   xmonad
   xrandr
   xsel
   youtube-dl
   ))
