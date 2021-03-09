#lang rash

;; 1. rashrc.rkt files are modules, can be in any #lang
;; 2. files that start with #lang are modules

(displayln "=== Loading rashrc.rkt")

(require
 linea/line-macro
 "crep.rkt"
 "git.rkt"
 (for-syntax racket/base syntax/parse))

(provide
 (all-defined-out)
 (all-from-out "crep.rkt")
 (all-from-out "git.rkt"))

;; (lambda (_) 1)
;; (require linea/line-macro)
;; TODO why is lambda undefined in rashrc.rkt and defined in rashrc?
;; (define-line-macro x (lambda (_) #'(+ 1 2)))

(define-simple-pipeline-alias goodies sudo needrestart)
(define-simple-pipeline-alias lg   git lg-20)
;; Sort by modification time, newest first, reversed
(define-simple-pipeline-alias lat  ls --color=auto -lt -all) ;; ls -lat
(define-simple-pipeline-alias latr ls --color=auto --sort=time -l --almost-all --reverse) ;; ls -latr
(define-simple-pipeline-alias l    ls --color=auto -la)
(define-simple-pipeline-alias lr   lein repl)
(define-simple-pipeline-alias lock   xflock4)
(define-simple-pipeline-alias loff   xfce4-session-logout --logout --fast)
(define-simple-pipeline-alias shut   xfce4-session-logout --halt   --fast)
(define-simple-pipeline-alias reboot xfce4-session-logout --reboot --fast)
;; (define-simple-pipeline-alias susp   xfce4-session-logout --suspend)
(define-simple-pipeline-alias susp   systemctl suspend)
(define-simple-pipeline-alias shutdown sudo shutdown -h now)
(define-simple-pipeline-alias ys   youtube-dl --extract-audio)
(define-simple-pipeline-alias y    youtube-dl --write-auto-sub --sub-lang fr)
(define-simple-pipeline-alias gs   git status --short --branch)
(define-simple-pipeline-alias gst  git status)
(define-simple-pipeline-alias gtg  git tag --sort version:refname)
(define-simple-pipeline-alias gb   git brach)
(define-simple-pipeline-alias gba  git brach --all)
(define-simple-pipeline-alias gmv  git mv)
(define-simple-pipeline-alias glo  git pull --rebase)
(define-simple-pipeline-alias gra  git rebase --abort)
(define-simple-pipeline-alias grc  git rebase --continue)
(define-simple-pipeline-alias grs  git rebase --skip)
(define-simple-pipeline-alias grh  git reset  --hard)
(define-simple-pipeline-alias grm  git rm)
(define-simple-pipeline-alias gsh  git stash save)
(define-simple-pipeline-alias gshp git stash pop)
(define-simple-pipeline-alias gbb  git bisect bad)
(define-simple-pipeline-alias gbg  git bisect good)
(define-simple-pipeline-alias gcom git checkout master)
(define-simple-pipeline-alias gcod git checkout -)
(define-simple-pipeline-alias armv  sudo apt autoremove --yes)
(define-simple-pipeline-alias purge sudo apt purge --yes)
;; (define-simple-pipeline-alias cdd)
;; (define-simple-pipeline-alias cd-)
(define-simple-pipeline-alias cw   chmod +w)
(define-simple-pipeline-alias cx   chmod +x)
(define-simple-pipeline-alias d2u  fromdos)
(define-simple-pipeline-alias u2d  todos)
(define-simple-pipeline-alias f    fdfind)
(define-simple-pipeline-alias v    vim)
(define-simple-pipeline-alias tf   tail -f) ;; TODO tail replacement?
(define-simple-pipeline-alias timestamp date "+%F_%H-%M-%S") ;; same as "+%Y-%m-%d_%H-%M-%S"
(define-simple-pipeline-alias ext  extract)
(define-simple-pipeline-alias wp   clear)
