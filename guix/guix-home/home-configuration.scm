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
  (gnu home services shells)

  ;; for my-config-service
  (gnu home services)

  ;; for scandir
  (ice-9 ftw)

  ;; for string-match
  (ice-9 regex)

  ;; for take
  ;; (srfi srfi-1)

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  ;; for home-git-service-type
  ;; (gnu home services version-control)
  )

(define (dotfiles-guix-home-dir s)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (string-append (getenv "HOME") "/dev/dotfiles/guix/guix-home" s))

(define packages
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
   "audacity"
   "autoconf"
   "babashka"
   "bash"
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
   "direnv"
   "emacs"
   "emacs-with-editor"
   "evince"
   "exa"
   "factorio"
   "fd"
   "ffmpeg"
   "firefox"
   "fish"
   "flatpak"
   "font-adobe-source-code-pro"
   "font-gnu-freefont"
   "font-gnu-unifont"
   "fuse"
   "gcc"
   "gcc-toolchain"
   "gdm"
   "ghc"
   "git"
   "git:gui"
   "git:send-email"
   "glib:bin"
   "glibc-locales"
   "gnupg"
   "gnutls"
   "gparted"
   "graphviz"
   "grub"
   "gsettings-desktop-schemas"
   "gtk"
   "guile"
   "guile-hall"
   "guile-studio"
   "gv"
   "gvfs"
   "gwl"
   "htop"
   "hwinfo"
   "iniparser"
   "inkscape"
   "inxi"
   "ispell"
   "jmtpfs"
   "konsole"
   "krusader"
   "leiningen"
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
   "openjdk"
   "openjdk:jdk"
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
   "racket"
   "readline"
   "recutils"
   "ripgrep"
   "rlwrap"
   "rsync"
   "rust"
   "screen"
   "scsh"
   "seahorse"
   "spice-vdagent"
   "taglib"
   "tectonic"
   "texinfo"
   "texlive"
   "texlive-latex-pdfpages"
   "thunar-volman"
   "tig"
   "tree"
   "tzdata"
   "udiskie"
   "ungoogled-chromium"
   "uniutils"
   "unzip"
   "usbutils"
   "vim"
   "virt-viewer"
   "xdg-utils"
   "xev"
   "xfce4-clipman-plugin"
   "xfce4-notifyd"
   "xfce4-screenshooter"
   "xkeyboard-config"
   "xmodmap"
   "xmonad"
   "xrandr"
   "xsel"
   "youtube-dl"
   "zip"
   ))

(define fish-funs-my
  ;; (scandir (string-append (getenv "HOME") "/dev/dotfiles/fish/functions/")
  ;;      (lambda (f) (string-match "\\S{1,}\\.fish$" f)))
  (list
   ".....fish"
   "....fish"
   "...fish"
   "armv.fish"
   "b.fish"
   "bin.fish"
   "c.fish"
   "cd-.fish"
   "cdd.fish"
   "cheat.fish"
   "cl.fish"
   "cls.fish"
   "coc.fish"
   "corona.fish"
   "cpr.fish"
   "ctd.fish"
   "cvs-reset.fish"
   "d2u.fish"
   "dec.fish"
   "der.fish"
   "desk.fish"
   "desktop.fish"
   "dev.fish"
   "dir.fish"
   "dos2unix.fish"
   "dotf.fish"
   "down.fish"
   "e.fish"
   "eac.fish"
   "ec.fish"
   "edit-grub.fish"
   "el.fish"
   "ema.fish"
   "emag.fish"
   "envp.fish"
   "ext.fish"
   "extract.fish"
   "f.fish"
   "fc.fish"
   "fclj.fish"
   "fdk.fish"
   "ff.fish"
   "filename.fish"
   "fjar.fish"
   "fjava.fish"
   "foo.fish"
   "fxml.fish"
   "g.fish"
   "ga.fish"
   "gaa.fish"
   "gad.fish"
   "gap.fish"
   "gau.fish"
   "gb.fish"
   "gbb.fish"
   "gbg.fish"
   "gbir.fish"
   "gbis.fish"
   "gbr.fish"
   "gbra.fish"
   "gbrd.fish"
   "gbrm.fish"
   "gc.fish"
   "gci.fish"
   "gcia.fish"
   "gcianoe.fish"
   "gcim.fish"
   "gcl.fish"
   "gco-.fish"
   "gco.fish"
   "gcob.fish"
   "gcod.fish"
   "gcom.fish"
   "gcoo.fish"
   "gcov.fish"
   "gcp.fish"
   "gdd.fish"
   "gdf.fish"
   "genpasswd.fish"
   "getopts.fish"
   "gfe.fish"
   "gfeo.fish"
   "gfeu.fish"
   "gg.fish"
   "gh.fish"
   "ghe.fish"
   "gk.fish"
   "gl.fish"
   "gla.fish"
   "glf.fish"
   "glg.fish"
   "glh.fish"
   "gmv.fish"
   "goodies.fish"
   "gr.fish"
   "gra.fish"
   "grb.fish"
   "grba.fish"
   "grbc.fish"
   "grbi.fish"
   "grbs.fish"
   "grc.fish"
   "grepc.fish"
   "grepo.fish"
   "grepp.fish"
   "grh.fish"
   "gri.fish"
   "grm.fish"
   "grs.fish"
   "gs.fish"
   "gsh.fish"
   "gshp.fish"
   "gss.fish"
   "gst.fish"
   "gstr.fish"
   "gtg.fish"
   "h.fish"
   "h1.fish"
   "h2.fish"
   "h3.fish"
   "h4.fish"
   "h10.fish"
   "h20.fish"
   "h30.fish"
   "h40.fish"
   "he.fish"
   "hib.fish"
   "hibernate.fish"
   "hoch.fish"
   "hrep.fish"
   "ifconfig.fish"
   "inst.fish"
   "k9.fish"
   "kill-buffers.fish"
   "lat.fish"
   "latest.fish"
   "latr.fish"
   "latte.fish"
   "launch.fish"
   "lc.fish"
   "lca.fish"
   "lcad.fish"
   "lcc.fish"
   "lco.fish"
   "lct.fish"
   "lcxa.fish"
   "ldir.fish"
   "lf.fish"
   "lff.fish"
   "lg.fish"
   "lga.fish"
   "lh.fish"
   "li.fish"
   "ll.fish"
   "lock.fish"
   "loff.fish"
   "lr.fish"
   "lT.fish"
   "lt.fish"
   "ltr.fish"
   "lx.fish"
   "m.fish"
   "make-emacs.fish"
   "make-git.fish"
   "math.fish"
   "mkcd.fish"
   "mount-usb.fish"
   "music.fish"
   "netstat.fish"
   "notes.fish"
   "nslookup.fish"
   "nt.fish"
   "owid.fish"
   "png.fish"
   "prep.fish"
   "purge.fish"
   "reboot.fish"
   "reinst.fish"
   "rmrf.fish"
   "rmv.fish"
   "rr.fish"
   "script.clj"
   "server.clj"
   "shut.fish"
   "shutdown.fish"
   "spa.fish"
   "susp.fish"
   "synaptic.fish"
   "t.fish"
   "take.fish"
   "telnet.fish"
   "tf.fish"
   "time.fish"
   "timestamp.fish"
   "tmp.fish"
   "trackle.fish"
   "u.fish"
   "u2d.fish"
   "ufo.fish"
   "umount-usb.fish"
   "unexport.fish"
   "unix2dos.fish"
   "unset.fish"
   "utils.fish"
   "v.fish"
   "vdir.fish"
   "vesmir.fish"
   "wp.fish"
   "y.fish"
   "yas.fish"
   "ys.fish"
   "zark.fish"
   ))

(define fish-funs-plugins
  (list
   "_tide_detect_os.fish"
   "_tide_find_and_remove.fish"
   "_tide_item_character.fish"
   "_tide_item_chruby.fish"
   "_tide_item_cmd_duration.fish"
   "_tide_item_context.fish"
   "_tide_item_git.fish"
   "_tide_item_go.fish"
   "_tide_item_jobs.fish"
   "_tide_item_kubectl.fish"
   "_tide_item_newline.fish"
   "_tide_item_node.fish"
   "_tide_item_os.fish"
   "_tide_item_php.fish"
   "_tide_item_rustc.fish"
   "_tide_item_shlvl.fish"
   "_tide_item_status.fish"
   "_tide_item_time.fish"
   "_tide_item_vi_mode.fish"
   "_tide_item_virtual_env.fish"
   "_tide_print_item.fish"
   "_tide_prompt.fish"
   "_tide_pwd.fish"
   "_tide_remove_unusable_items.fish"
   "_tide_sub_bug-report.fish"
   "_tide_sub_configure.fish"
   "fish_mode_prompt.fish"
   "fish_prompt.fish"
   "fisher.fish"
   "tide.fish"
   ))

(define funs
  (map (lambda (f)
         `(,(string-append (basename (getenv "XDG_CONFIG_HOME")) "/fish/functions/" f)
           ,(local-file (string-append (getenv "HOME") "/dev/dotfiles/fish/functions/" f)
                        ;; fix the 'guix home: error: invalid name: `...fish''
                        (string-append "fish-function-" f))))
       fish-funs-my))

(define confds
  (map (lambda (f)
         `(,(string-append (basename (getenv "XDG_CONFIG_HOME")) "/fish/conf.d/" f)
           ,(local-file (string-append (getenv "HOME") "/dev/dotfiles/fish/conf.d/" f)
                        (string-append "fish-confd-" f))))
       (list
        "_tide_init.fish")))

(define completions
  (map (lambda (f)
         `(,(string-append (basename (getenv "XDG_CONFIG_HOME")) "/fish/completions/" f)
           ,(local-file (string-append (getenv "HOME") "/dev/dotfiles/fish/completions/" f)
                        (string-append "fish-completion-" f))))
       (list
        "fisher.fish"
        "tide.fish")))

(define plugins
  (map (lambda (f)
         `(,(string-append (basename (getenv "XDG_CONFIG_HOME")) "/fish/" f)
           ,(local-file (string-append (getenv "HOME") "/dev/dotfiles/fish/" f)
                        (string-append "fish-plugins-" f))))
       (list
        "fish_plugins"
        ;; "fish_variables" this is changed
        )))

;; https://github.com/clojure-quant/infra-guix/blob/cf67ccfce02f4d1e2441ed9f34b5ec6583ffc1cc/home/config-nuc.scm
(define my-config-service
  (simple-service 'test-config
                  home-files-service-type
                  (cons
                   (list "local-stuff.fish" (local-file (string-append (getenv "HOME") "/local-stuff.fish")))
                   funs
                   #;(append plugins (append funs (append completions confds)))
                   )))

(define abbrevs
  '(
    ;; armv
    ;; b
    ;; bin
    ;; c
    ;; ("cat" . "bat --plain")
    ;; ("cd-" . "cd -")
    ;; ("cdd" . "cd -")
    ;; cheat
    ;; cl
    ;; cls
    ;; coc
    ;; corona
    ;; cpr
    ;; ctd
    ;; cvs-reset
    ;; d2u
    ;; dec
    ;; der
    ;; desk
    ;; desktop
    ;; dev
    ;; dir
    ;; dos2unix
    ;; dotf
    ;; down
    ;; e
    ;; eac
    ;; ec
    ;; edit-grub
    ;; el
    ;; ema
    ;; emag
    ;; envp
    ;; ext
    ;; extract
    ;; f
    ;; fc
    ;; fclj
    ;; fdk
    ;; ff
    ;; fg
    ;; filename
    ;; fjar
    ;; fjava
    ;; foo
    ;; fxml
    ;; g
    ;; ga
    ;; gaa
    ;; gad
    ;; gap
    ;; gau
    ;; gb
    ;; gbb
    ;; gbg
    ;; gbir
    ;; gbis
    ;; gbr
    ;; gbra
    ;; gbrd
    ;; gbrm
    ;; gc
    ;; gci
    ;; gcia
    ;; gcianoe
    ;; gcim
    ;; gcl
    ;; ("gco" . "git checkout")
    ;; ("gco-" . "git checkout -")
    ;; gcob
    ;; ("gcod" . "git checkout -")
    ;; ("gcm"  . "git checkout master")
    ;; ("gcom" . "git checkout master")
    ;; gcoo
    ;; gcov
    ;; gcp
    ;; gdd
    ;; gdf
    ;; genpasswd
    ;; getopts
    ;; gfe
    ;; gfeo
    ;; gfeu
    ;; gg
    ;; gh
    ;; ghe

    ;; gk must be a function because in the 'gitk --all (string escape -- $argv) \&'
    ;; the $argv doesn't come at the end
    ;; gk

    ;; gl
    ;; gla
    ;; glf
    ;; glg
    ;; glh
    ;; gmv
    ;; goodies
    ;; gr
    ;; gra
    ;; grb
    ;; grba
    ;; grbc
    ;; grbi
    ;; grbs
    ;; grc
    ;; grep
    ;; grepc
    ;; grepo
    ;; grepp
    ;; grh
    ;; gri
    ;; grm
    ;; grs
    ;; ("gs"  . "git status")
    ;; gsh
    ;; gshp
    ;; gss
    ;; gst
    ;; gstr
    ;; gtg
    ;; h
    ;; h1
    ;; h10
    ;; h2
    ;; h20
    ;; h3
    ;; h30
    ;; h4
    ;; h40
    ;; he
    ;; help
    ;; hib
    ;; hibernate
    ;; hoch
    ;; hrep
    ;; ifconfig
    ;; inst
    ;; k9
    ;; kill-buffers
    ;; lT
    ;; la
    ;; lat
    ;; latest
    ;; latr
    ;; latte
    ;; launch
    ;; lc
    ;; lca
    ;; lcad
    ;; lcc
    ;; lco
    ;; lct
    ;; lcxa
    ;; ldir
    ;; lf
    ;; lff
    ;; lg
    ;; lga
    ;; lh
    ;; li
    ;; ll
    ;; lock
    ;; loff
    ;; lr
    ;; ls
    ;; lt
    ;; ltr
    ;; lx
    ;; m
    ;; make-emacs
    ;; make-git
    ;; math
    ;; mkcd
    ;; mount-usb
    ;; music
    ;; netstat
    ;; notes
    ;; nslookup
    ;; nt
    ;; open
    ;; owid
    ;; png
    ;; prep
    ;; purge
    ;; reboot
    ;; reinst
    ;; rmrf
    ;; rmv
    ;; rr
    ;; shut
    ;; shutdown
    ;; spa
    ;; susp
    ;; suspend
    ;; synaptic
    ;; t
    ;; take
    ;; telnet
    ;; tf
    ;; timestamp
    ;; tmp
    ;; trackle
    ;; trap
    ;; u
    ;; u2d
    ;; ufo
    ;; umount-usb
    ;; unexport
    ;; unix2dos
    ;; unset
    ;; utils
    ;; v
    ;; vdir
    ;; vesmir
    ;; ("wp" . "printf '\\ec'")
    ;; y
    ;; yas
    ;; ys
    ;; zark
    ("xc"  . "guix gc")
    ("xhr" . "guix home reconfigure")
    ("xp"  . "guix pull")
    ("xse" . "guix search")
    ("xsh" . "guix show")
    ("xsr" . "sudo --preserve-env guix system reconfigure")
    ("xu"  . "guix upgrade")
    ))

(home-environment
 (packages
  (map (compose list specification->package+output)
       packages))

 ;; TODO
 ;; see [PATCH] services: Add udev-rules-service helper. https://issues.guix.gnu.org/40454

 (services
  (list
   (service
    home-bash-service-type
    (home-bash-configuration
     (aliases
      '(("grep" . "grep --color=auto")
        ("l" . "ls -lA --color=auto")
        ("ll" . "ls -l")
        ("ls" . "ls -p --color=auto")))
     (bashrc
      (list
       (local-file
        ;; (local-file ".bashrc" "bashrc") should work too
        (dotfiles-guix-home-dir "/.bashrc")
        ;; prevent 'guix home: error: invalid name: `.bashrc''
        "bashrc")))
     (bash-profile
      (list
       (local-file
        ;; (local-file ".bashrc" "bash_profile") should work too
        (dotfiles-guix-home-dir "/.bash_profile")
        ;; prevent 'guix home: error: invalid name: `.bash_profile''
        "bash_profile")))
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

         ;; see /home/bost/dev/guix/gnu/home/services/shells.scm
         (home-fish-configuration
          (abbreviations abbrevs)
          #;
          (aliases
           '(
             #;("l" . "ls -a")
             ("dev"   . "cd $HOME/dev")
             ("dec"   . "cd $HOME/dec")
             ("der"   . "cd $HOME/der")
             ("bin"   . "cd $HOME/bin")
             ("cheat" . "cd $HOME/dev/cheat")
             ("dotf"  . "cd $HOME/dev/dotfiles")
             ))
          (config (list (local-file
                         (string-append (getenv "HOME")
                                        "/dev/dotfiles/fish/config.fish"))))
          ;; see also home-environment-variables-service-type
          ;; https://guix.gnu.org/manual/devel/en/html_node/Essential-Home-Services.html
          ;; (simple-service 'some-useful-env-vars-service
          ;;                 home-environment-variables-service-type
          ;;                 `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
          ;;                   ("SHELL" . ,(file-append zsh "/bin/zsh"))
          ;;                   ("USELESS_VAR" . #f)
          ;;                   ("_JAVA_AWT_WM_NONREPARENTING" . #t)))
          #;
          (environment-variables
           `(
             ("TEST" . "test")
             ("PATH" . "$PATH:$HOME/bin")
             ))
          ))

        my-config-service
        ;; https://github.com/babariviere/dotfiles/blob/1deae9e15250c86cc235bb7b6e69ea770af7b13a/baba/home/gaia.scm
        ;; (service home-git-service-type
        ;;          (home-git-configuration
        ;;           (config
        ;;            `((user
        ;;               ((name . "Rostislav Svoboda")
        ;;                (email . "Rostislav.Svoboda@gmail.com")
        ;;                #;(signingKey . "...")))
        ;;              (github
        ;;               ((user . "Bost")))
        ;;              (remote
        ;;               ((pushDefault . "origin")))
        ;;              #;(commit ((gpgSign . #t)))
        ;;              #;(tag ((gpgSign . #t)))))))
        )))
