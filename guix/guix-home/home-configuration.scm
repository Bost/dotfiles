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
        ;; fish is sinstalled by (service home-fish-service-type)
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
        ;; bash is installed by home-bash-service-type
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

 ;; https://github.com/clojure-quant/infra-guix/blob/cf67ccfce02f4d1e2441ed9f34b5ec6583ffc1cc/home/config-nuc.scm
 #;
 (define my-config-service
   (simple-service 'test-config
                   home-files-service-type
                   (list `("config/test.conf" ,(plain-file "tmp-file.txt" "the content of ~/.config/test.conf"))
                         `("ssh/config" ,(local-file "./ssh/config"))
                         `("config/alacritty/alacritty.yml" ,(local-file "./alacritty/alacritty.yml"))
                                        ; emacs
                         `("config/emacs/init.el" ,(local-file "./emacs/init.el")) ; does not get loaded
                         `("emacs.d/init.el" ,(local-file "./emacs/init.el"))
                                        ; sway / waybar
                         `(".config/sway/config" ,(local-file "./sway/config"))
                         `(".config/waybar/config" ,(local-file "./waybar/config"))
                         `(".config/waybar/style.css" ,(local-file "./waybar/style.css"))
                                        ; clojure
                         `(".config/clojure/deps.edn" ,(local-file "./clojure/deps.edn"))
                         `(".config/clojure/cljfmt.edn" ,(local-file "./clojure/cljfmt.edn"))
                                        ;`("xsettingsd" ,(local-file "./xsettingsd/xsettingsd.conf"))
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
           (list
            ;; (local-file ".bashrc" "bashrc") should work too
            (local-file (ghc "/.bashrc") "bashrc")))
          (bash-profile
           (list (local-file (ghc "/.bash_profile") "bash_profile")))

          #;
          (environment-variables
           `(("XDG_CURRENT_DESKTOP" . "sway")
             ("XDG_SESSION_TYPE" . "wayland")
             ("MOZ_ENABLE_WAYLAND" . "1")
             ;; ...
             ))
          ))

        ;; emacs-with-native-comp
        ;; https://github.com/flatwhatson/guix-channel/blob/master/flat/packages/emacs.scm

        ;; https://github.com/search?q=home-fish-service-type&type=code
        ;; see https://github.com/babariviere/brycus/blob/e22cd0c0b75c5b4c95369fc95cce95ed299b63ff/guix/brycus/home-service.scm
        (service
         home-fish-service-type
         ;; fish configuration - see gnu/home/services/shells.scm
         #;
         (home-fish-configuration
          (abbreviations '(("gco" . "git checkout")
                           ("gc" . "guix gc")
                           ("gs" . "git status")
                           ("gsr" . "sudo -E guix system reconfigure")
                           ("ghr" . "guix home reconfigure")
                           ("cat" . "bat -pp")))
          (aliases
           '(("l" . "ls -a")))
          (config (list (local-file "config/fish/config.fish")))
          (environment-variables
           `(("TEST" . "test")))
          ))

        ;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
        #;
        (service home-git-service-type
                 (home-git-configuration
                  (config
                   `((user
                      ((name . "Bastien Riviere")
                       (email . "me@babariviere.com")
                       (signingKey . "39035CC0B75D1142")))
                     (github
                      ((user . "babariviere")))
                     (remote
                      ((pushDefault . "origin")))
                     (commit
                      ((gpgSign . #t)))
                     (tag
                      ((gpgSign . #t)))))))

        )))
