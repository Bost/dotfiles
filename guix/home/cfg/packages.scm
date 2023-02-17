(define-module (cfg packages)
  #:use-module (srfi srfi-1)
  #:use-module (utils)
  #:use-module (cfg spguimacs-packages)
  #:use-module (common settings)
  #:export (
            basic-profile-packages
            kde-dependent-packages
            packages-from-additional-channels
            slow-packages
            user-profile-packages

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

(define (slow-packages)
  "Packages slow to build, graft, download, etc."
  (list
   "audacity" ;; 35.8MiB
   "inkscape" ;; ~93MiB

   ;; rust downloads (see below) and then it needs to be build:
   ;;     rust-1.59.0  121.1MiB
   ;;     rust-1.59.0-cargo  3.2MiB
   ;;     rustc-1.60.0-src.tar.xz  63.6MiB
   "rust" ;; the 1.60 has to be build

   "tectonic" ;; embeddable TeX/LaTeX engine

   ;; texlive downloads:
   ;;    texlive-texmf-20210325  3.24GiB

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

(define (basic-profile-packages)
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

(define (devel-profile-packages)
  (list
   "emacs"
   "emacs-with-editor"
   "pinentry" ;; needed to sign commits
   ))

(define (user-profile-packages)
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
   "racket"
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
   "xdg-utils"
   "xev"
   "xfce4-clipman-plugin"
   "xfce4-netload-plugin" ;; traffic indicator
   "xfce4-notifyd"
   "xfce4-screensaver"
   "xfce4-screenshooter"
   "xkeyboard-config"
   "xmodmap"
   "xmonad"

   ;; Create, manipulate, burn ISO-9660 file systems; see also cdrtools
   "xorriso"

   "xrandr"
   "xsel"
   "youtube-dl"
   ))
(testsymb 'user-profile-packages)

(define packages-to-install
  (cond
   [(home-lukas-config)
    (begin
      ;; (format #t "(home-lukas-config)\n")
      (basic-profile-packages))]
   [(home-ecke-config)
    (begin
      ;; (format #t "(home-ecke-config)\n")
      (append
       (basic-profile-packages)
       (devel-profile-packages)
       (user-profile-packages)
       (kde-dependent-packages)
       (slow-packages)
       (packages-from-additional-channels)
       (spguimacs-packages)))]
   [(home-geek-config)
    (begin
      ;; (format #t "(home-geek-config)\n")
      (append
       (basic-profile-packages)
       (devel-profile-packages)
       (user-profile-packages)
       (kde-dependent-packages)
       ;; (slow-packages)
       (packages-from-additional-channels)
       ;; (spguimacs-packages)
       ))]
   [#t
    (error
     (format #f "hostname '~a' must be one of the: ~a\n"
             (hostname) (string-join hostnames)))]))
(testsymb 'packages-to-install)

(format #t "~a ~a packages-to-install\n" m (length packages-to-install))

;; (format #t "~a module evaluated\n" m)
