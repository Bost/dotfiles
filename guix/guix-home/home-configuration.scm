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

  (gnu home services mcron) #| home-mcron-service-type |#
  (gnu home services)       #| my-config-service |#
  (ice-9 ftw)               #| scandir |#
  (ice-9 regex)             #| string-match |#
  (guix build utils)        #| invoke |#
  #;(srfi srfi-1)           #| take |#

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  #;(gnu home services version-control) #| home-git-service-type |#
  )

(define (dotfiles-guix-home-dir s)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (string-append (getenv "HOME") "/dev/dotfiles/guix/guix-home" s))

(define own-packages
  (list
   "leiningen"
   "babashka"
   "factorio"
   "firefox"
   ))

(define packages
  (append
   #;own-packages
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
    "fd"
    "ffmpeg"
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
    )))

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
    ("xu"  . "guix upgrade")
    ("xi"  . "guix install")
    ("xp"  . "guix pull")
    ("xhb" . "guix home build")
    ("xhc" . "guix home container")
    ("xhd" . "guix home describe")
    ("xhr" . "guix home reconfigure")
    ;; ("xpu" . "guix pull && guix upgrade --do-not-upgrade='(maven|clojure-tools).*'")
    ("xse" . "guix search")
    ("xsh" . "guix show")
    ("xsr" . "sudo --preserve-env guix system reconfigure")
    ))

;; (define do-job
;;   ;; as user "bost" at 17:05 This runs from the user's home directory.
;;   ;; #~(job
;;   ;;    '(next-minute-from (next-hour '(17)) '(5))
;;   ;;    (invoke "touch"
;;   ;;            (string-append "/tmp/srvc-second-"
;;   ;;                           (number->string (current-time))))
;;   ;;    #:user "bost")

;;   #~(job '(next-second)
;;          ;; (lambda () ...) doesn't work
;;          (list
;;           (invoke "touch"
;;                   (string-append "/tmp/srvc-second-"
;;                                  (number->string (current-time))))))
;;   )

(define srvc-singleton-<time>
  '(lambda ()
     ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
     ;; (list ...) doesn't work when IN a gexp?
     (invoke "touch"
             (string-append "/tmp/srvc-singleton-"
                            (number->string (current-time))))
     ;; get the pid of the parent process and kill that process;
     ;; i.e. effectively kill this job-process
     (kill (getppid) SIGINT)))

(define srvc-multi-<time>
  '(lambda ()
     ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
     ;; (list ...) doesn't work when IN a gexp?
     (invoke "touch"
             (string-append "/tmp/srvc-multi-"
                            (number->string (current-time))))))

(define srvc-touch-once
  '(lambda ()
     (system "touch /tmp/srvc-touch-once")
     ;; get the pid of the parent process and kill that process;
     ;; i.e. effectively kill this job-process
     (kill (getppid) SIGINT)))

;; fish and bash separate elements of a list with a different separator
(define bash-list-separator ":")
(define fish-list-separator " ")
(define scm-bin-dirname "scm-bin")
(define scm-bin-dirpath (string-append "/" scm-bin-dirname))

(define utils
  (scheme-file
   "utils.scm"
   #~(
      (define-module (utils)
        #:use-module (ice-9 rdelim)
        #:use-module (ice-9 regex)
        #:use-module (ice-9 popen)
        #| #:use-module (guix build utils) ;; invoke - not needed |#
        #:export (partial dbg exec))

      (define (partial fun . args)
        (lambda x (apply fun (append args x))))

      (define (dbg prm)
        (format #t "\n~a\n" prm)
        prm)

      (define (read-all port)
        "Return a list of all lines from the PORT."
        (let loop ((res '())
                   (str (read-line port))) ; from (ice-9 popen)
          (if (and str (not (eof-object? str)))
              (loop (append res (list str))
                    (read-line port))
              res)))

      (define (exec command)
        "Usage:
(let* ((ret (exec command)))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#)
        (format #t \"Command failed\")))"
        ((compose
          (lambda (command)
            (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
                   (str  (read-all port)))
              (cons
               (status:exit-val (close-pipe port))
               str)))
          (lambda (s)
            ;; TODO implement pretty-print for bash commands
            ;; ~a - outputs an argument like display
            ;; ~s - outputs an argument like write (i.e. print to string)
            ;; ~% is newline \n
            (format #t "~a~%" s)
            s)
          (lambda (cmd)
            (if (list? cmd)
                (string-join cmd " ")
                cmd)))
         command)))
   #:splice? #t))

(define (chmod-plus modifier)
  "Example:
        chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir"
  `(,(string-append scm-bin-dirname "/p" modifier)
    ,(program-file
      (string-append "chmod-plus-" modifier)
      (with-imported-modules `(((utils) => ,utils))
        #~(begin
            (use-modules (utils))
            ((compose
              exec
              #;(partial apply system*)
              (partial cons* (string-append "chmod +" #$modifier))
              cdr)
             (command-line)))))))

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
     ;; (guix-defaults? #t) ;; Add sane defaults to the top of the .bashrc
     #|
     ;; Aliases will be defined after the contents of the bashrc field has been
     ;; put in the .bashrc
     ;; TODO fix the documentation:
     ;; The aliases are on the top of the .bashrc (b/c of '(guix-defaults? #t)' ???)
     |#
     ;; When using 'bashrc - local-file' then the aliases are added to the
     ;; .bashrc at the bottom.
     ;; When using '(guix-defaults? #t)' then the aliases are on the top of the
     ;; .bashrc.
     (aliases
       ;; aliases for "l" "ll" "ls" come from the .bashrc template and will be
       ;; overridden because see above
      '())

     ;; List of file-like objects, which will be ADDED(!) to .bashrc.
     (bashrc
      (list
       (local-file
        ;; (local-file ".bashrc" "bashrc") should work too
        (dotfiles-guix-home-dir "/.bashrc_additions")
        ;; prevent 'guix home: error: invalid name: `.bashrc''
        "bashrc_additions")))
     ;; List of file-like objects, which will be ADDED(!) to .bash_profile
     (bash-profile
      (list
       (plain-file "bash-profile"
                   (string-append
                    "\n" "export HISTFILE=$XDG_CACHE_HOME/.bash_history"))
       #;
       (local-file
       ;; (local-file ".bashrc" "bash_profile") should work too
       (dotfiles-guix-home-dir "/.bash_profile_additions")
       ;; prevent 'guix home: error: invalid name: `.bash_profile''
       "bash_profile_additions")))
     (environment-variables
      `(("PATH" . ,(string-join
                    (list
                     ;; my own scripts take precedence...
                     (string-append "$HOME" scm-bin-dirpath)
                     ;; TODO create the link
                     ;;     ln -s ~/dev/dotfiles/bin ~/bin
                     ;; using guix home
                     "$HOME/bin"
                     ;; ... over default default PATH, putting...
                     "$PATH"
                     ;; ... bin-directory for for script-based installations of:
                     ;;     babashka heroku clojure
                     ;; at the end of the PATH
                     "/usr/local/bin")
                    bash-list-separator))))))

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
     ("dotf"  . "cd $HOME/dev/dotfiles")))
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

     #| `environment-variables' inherited from bash |#))

   my-config-service

   (simple-service
    'scheme-files home-files-service-type
    (list
     `(,(string-append scm-bin-dirname "/l")
       ,(program-file
         "list-directory-contents"
         (with-imported-modules `(((utils) => ,utils))
           #~(begin
               (use-modules (utils))
               ((compose
                 ;; TODO `exec' doesn't work with exa. WTF?
                 (partial apply system*)
                 #;(lambda (p) (format #t "#t before system*/exec: ~a\n" p) p)
                 (partial
                  cons*
                  "exa" "-abghHliS" "--color=always" "--time-style=full-iso"
                  #|
                  "exa" "-abghHliS" "--color=always"
                  ;; exa has no support for '+%d-%m-%Y %H:%M:%S' time formatters
                  "exa" "-abghHliS" "--color=always" "--time-style=default"
                  "exa" "-abghHliS" "--color=always" "--time-style=iso"
                  "exa" "-abghHliS" "--color=always" "--time-style=long-iso"
                  ;; '--file-type' append indicator (one of /=>@|) to entries
                  ;; TODO consider custom coloring after `ls --color=never`
                  "ls" "-lA" "--file-type" "--color"
                       "--time-style=+%d-%m-%Y %H:%M:%S"
                  |#)
                 cdr)
                (command-line))))))
    (chmod-plus "rw")
    (chmod-plus "x")

    `(,(string-append scm-bin-dirname "/ghog")
      ,(program-file
        "git-push-to-remotes"
        (with-imported-modules
            ;; TODO clarify is source-module-closure needed only for imports of
            ;; guix modules?
            `(((utils) => ,utils))
          #~(begin
              (use-modules (ice-9 rdelim)
                           (ice-9 regex)
                           (ice-9 popen)
                           (utils))
              (let ((args (command-line)))
                ((compose
                  (partial
                   map
                   (compose
                    cdr
                    exec
                    (lambda (remote)
                      (append
                       (list "git" "push" "--follow-tags" "--verbose" remote)
                       (cdr args)))
                    car))
                  (partial filter (lambda (remote-url)
                                    (not (null? (cdr remote-url)))))
                  (partial map
                           (lambda (remote)
                             (cons remote
                                   ((compose
                                     (partial filter
                                              (lambda (url)
                                                (string-match "git@" url)))
                                     cdr
                                     exec
                                     (partial list "git" "remote" "get-url"))
                                    remote))))
                  (partial filter (lambda (remote)
                                    (not (string-match "heroku" remote))))
                  cdr
                  exec)
                 (list
                  "git" "remote")))))))
    ))

   #;
   (simple-service
    'bin-files home-files-service-type
    (map (lambda (f)
           `(,(string-append "bin/" f)
             ,(local-file (string-append (getenv "HOME") "/dev/dotfiles/bin/" f)
                          (string-append "bin-" f))))
         (list "g1" "g1.scm"
               "glo" "glo.scm"
               "guix-os" "guix-os.scm"
               "spag" "spag.scm"
               "ubuntu-os" "ubuntu-os.scm"
              )))

   ;; TODO test if the command-string can be created by string-append
   #;
   (service
    home-mcron-service-type
    (home-mcron-configuration
     (jobs
      (let* (;; every second
             ;; (job-period '(next-second))
             ;; every 5 seconds
             (job-period '(next-second (range 0 60 5))))
        (list
         ;; see '(gexp->derivation "the-thing" build-exp)' in the manual
         ;; Also: #+ vs. #$
         ;; In a cross-compilation context, it is useful to distinguish
         ;; between references to the native build of a package—that can
         ;; run on the host—versus references to cross builds of a package.
         ;; To that end, the #+ plays the same role as #$, but is a
         ;; reference to a native package build

         ;; #~(job '#$job-period '#$srvc-singleton-<time>)
         ;; #~(job '#$job-period '#$srvc-multi-<time>)
         ;; #~(job '#$job-period '#$srvc-touch-once)
         #~(job '#$job-period (lambda ()
                                ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
                                ;; (list ...) doesn't work when IN a gexp?
                                ;; TODO this doesn't get invoked at ALL!!!
                                (invoke "touch"
                                        (string-append "/tmp/srvc-job-singleton-"
                                                       (number->string (current-time))))
                                ;; get the pid of the parent process and kill that process;
                                ;; i.e. effectively kill this job-process
                                (kill (getppid) SIGINT)))
         #~(job '#$job-period (lambda ()
                                ;; (lambda () ...) doesn't work (when NOT IN a gexp?)
                                ;; (list ...) doesn't work when IN a gexp?
                                ;; TODO this doesn't get invoked at ALL!!!
                                (invoke "touch"
                                        (string-append "/tmp/srvc-job-multi-"
                                                       (number->string (current-time))))))
         #~(job '#$job-period (lambda ()
                                ;; TODO this doesn't get killed
                                (system "touch /tmp/srvc-job-lambda-touch-once")
                                ;; get the pid of the parent process and kill that process;
                                ;; i.e. effectively kill this job-process
                                (kill (getppid) SIGINT)))
         #~(job '#$job-period "touch /tmp/srvc-job-string-touch-periodically-0")
         )))))

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
