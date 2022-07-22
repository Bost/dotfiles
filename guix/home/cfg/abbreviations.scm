(define-module (cfg abbreviations)
  #:export (abbrevs))

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
    ("gxe" . "guix search")
    ("gxh" . "guix show")
    ("gxhb" . "guix home build")
    ("gxhc" . "guix home container")
    ("gxhd" . "guix home describe")
    ("gxhr" . "guix home reconfigure")
    ("gxi"  . "guix install")
    ("gxp"  . "guix pull")
    ("gxsd" . "guix system describe")
    ("gxsr" . "sudo --preserve-env guix system reconfigure")
    ("gxu"  . "guix upgrade")
    ;; ("xpu" . "guix pull && guix upgrade --do-not-upgrade='(maven|clojure-tools).*'")
    ))
