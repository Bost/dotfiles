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
   "factorio"
   "firefox"
   ))

(define (kde-dependent-packages)
  "KDE dependencies are slow to compile"
  (list
   "konsole"
   "krusader"))

(define (slow-packages)
  "Packages slow to build, graft, download, etc."
  (list
   "inkscape" ;; ~93MiB
   "tectonic" ;; embeddable TeX/LaTeX engine
   "texlive"
   "texlive-latex-pdfpages"
   "ungoogled-chromium"

   ;; openjdk-17.0.3  199.5MiB                                                                                                                                                                          1.8MiB/s 01:53 [##################] 100.0%
   ;; openjdk-17.0.3-doc  9.6MiB                                                                                                                                                                        1.2MiB/s 00:08 [##################] 100.0%
   ;; openjdk-17.0.3-jdk  275.9MiB
   ;; in total ~485 MiB
   "openjdk"
   "openjdk:jdk"
   "icedtea"  ;; ~240MiB; ??? it gets downloaded anyway ???
   "audacity" ;; 35.8MiB
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
   "evince"
   "exa"
   "fd"
   "ffmpeg"
   "flatpak"
   "font-adobe-source-code-pro"
   "font-gnu-freefont"
   "font-gnu-unifont"
   "fuse"
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
   "guile-hall"
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
   "libjpeg"
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
   "pulseaudio"
   "pv"
   "pybind11"
   "python"
   "python2"
   "qemu"
   "readline"
   "recutils"
   "ripgrep"
   "rlwrap"
   "rust"
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
