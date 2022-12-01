(define-module (cfg packages)
  #:use-module (srfi srfi-1)
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

   ;; rust-1.59.0  121.1MiB
   ;; rust-1.59.0-cargo  3.2MiB
   ;; rustc-1.60.0-src.tar.xz  63.6MiB
   "rust" ;; the 1.60 has to be build

   "tectonic" ;; embeddable TeX/LaTeX engine

   ;; texlive-texmf-20210325  3.24GiB
   ;; "texlive"                 ; may take too long to graft
   ;; "texlive-latex-pdfpages"  ; may take too long to graft

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
   "direnv"
   "emacs"
   "emacs-with-editor"
   "fish"
   "git"
   "git:gui"
   "git:send-email"
   "guile"
   "rsync"
   "unzip"
   "vim"
   "xfce4-clipman-plugin"
   "xfce4-netload-plugin" ;; traffic indicator
   "xkeyboard-config"
   "zip"
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
   "bat"
   "bc"
   "bind:utils"
   "clang"
   "clojure-tools"
   "clusterssh"
   "cmake"
   "curl"
   "dconf"
   "dconf-editor"
   "dos2unix"
   "evince"
   "exa"
   "fd"
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
   "gparted"
   "graphviz"
   "grub"
   "gsettings-desktop-schemas"
   "gtk"
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
   "xfce4-notifyd"
   "xfce4-screenshooter"
   "xmodmap"
   "xmonad"
   "xrandr"
   "xsel"
   "youtube-dl"
   ))
