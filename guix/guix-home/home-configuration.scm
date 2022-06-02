;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(define (ghc s)
  "Prepend 'guix-home-config-directory' to `s'
Note:
(format #t \"~a\" \"foo\") doesn't work"
  (string-append (getenv "HOME") "/dev/dotfiles/guix/guix-home/" s))

(home-environment
 (packages
  (map (compose list specification->package+output)
       (list
        "clang"
        "ungoogled-chromium"
        "firefox"
        "seahorse"
        "rsync"
        "openjdk:jdk"
        "openjdk"
        "nmap"
        "xdg-utils"
        "gvfs"
        "gdm"
        "udiskie"
        "guile-studio"
        "emacs"
        "mlt"
        "flatpak"
        "qemu"
        "audacity"
        "youtube-dl"
        "virt-viewer"
        "gtk"
        "emacs-emacsql-sqlite3"
        "gcc"
        "gcc-toolchain"
        "ffmpeg"
        "ghc"
        "parted"
        "gwl"
        "libreoffice"
        "konsole"
        "krusader"
        "thunar-volman"
        "spice-vdagent"
        "racket"
        "inxi"
        "git"
        "git:send-email"
        "git:gui"
        "inkscape"
        "gparted"
        "graphviz"
        "pavucontrol"
        "pulseaudio"
        "pinentry"
        "evince"
        "dconf-editor"
        "xfce4-notifyd"
        "xfce4-clipman-plugin"
        "xfce4-screenshooter"
        "dconf"
        "bind:utils"
        "emacs-with-editor"
        "glib:bin"
        "node"
        "tectonic"
        "mesa-utils"
        "gv"
        "gnupg"
        "python2"
        "php"
        "python"
        "cmake"
        "clusterssh"
        "asciinema"
        "usbutils"
        "libxpm"
        "libjpeg"
        "libungif"
        "libtiff"
        "gnutls"
        "libxaw3d"
        "autoconf"
        "bat"
        "exa"
        "recutils"
        "htop"
        "fd"
        "libavif"
        "ripgrep"
        "xmonad"
        "hwinfo"
        "maven"
        "vim"
        "ispell"
        "babashka"
        "font-gnu-unifont"
        "perl"
        "libtool"
        "leiningen"
        "texinfo"
        "make"
        "gsettings-desktop-schemas"
        "plocate"
        "mcron"
        "xrandr"
        "direnv"
        "xev"
        "lshw"
        "bc"
        "fish"
        "tdlib"
        "rust"
        "curl"
        "tig"
        "glibc-locales"
        "ncurses"
        "clojure-tools"
        "guile"
        "screen"
        "tree"
        "tzdata"
        "scsh"
        "xmodmap"
        "pkg-config"
        "font-adobe-source-code-pro"
        "font-gnu-freefont"
        "alsa-utils"
        "aspell-dict-fr"
        "aspell-dict-de"
        "aspell-dict-en"
        "aspell"
        "texlive-latex-pdfpages"
        "uniutils"
        "xsel"
        "xkeyboard-config"
        "mtr"
        "portaudio"
        "bash"
        "iniparser"
        "libconfini"
        "libavc1394"
        "lsof"
        "lolcat"
        "rlwrap"
        "unzip"
        "zip"
        )))
 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (aliases
           '(("grep" . "grep --color=auto")
             ("l" . "ls -lA --color=auto")
             ("ll" . "ls -l")
             ("ls" . "ls -p --color=auto")))
          (bashrc
           (list (local-file
                  (ghc "/.bashrc" )
                  "bashrc")))
          (bash-profile
           (list (local-file
                  (ghc "/.bash_profile")
                  "bash_profile"))))))))
