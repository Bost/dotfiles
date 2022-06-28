;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (home-configuration)
  #:use-module (cfg packages)
  #:use-module (cfg fish)
  #:use-module (cfg abbreviations)
  #:use-module (cfg mcron)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services mcron) #| home-mcron-service-type |#
  #:use-module (gnu home services)       #| my-config-service |#
  #:use-module (ice-9 ftw)               #| scandir |#
  #:use-module (ice-9 regex)             #| string-match |#
  #:use-module (guix build utils)        #| invoke |#
  ;; #:use-module (srfi srfi-1)          #| take |#

  ;; the https://issues.guix.gnu.org/51359 has not been merged yet
  #| home-git-service-type |#
  ;; #:use-module (gnu home services version-control)
  )

(define* (xdg-config-home #:rest args)
  (apply string-append (basename (getenv "XDG_CONFIG_HOME")) args))

(define* (user-home #:rest args)
  (apply string-append (getenv "HOME") args))

(define (dotfiles-guix-home s)
  "Note:
(format #t \"~a\" \"foo\") doesn't work"
  (user-home "/dev/dotfiles/guix/home" s))

(define funs
  (map (lambda (f)
         `(,(xdg-config-home "/fish/functions/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/functions/" f)
                        ;; fix the 'guix home: error: invalid name: `...fish''
                        (string-append "fish-function-" f))))
       fish-functions))

(define confds
  (map (lambda (f)
         `(,(xdg-config-home "/fish/conf.d/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/conf.d/" f)
                        (string-append "fish-confd-" f))))
       (list
        "_tide_init.fish")))

(define completions
  (map (lambda (f)
         `(,(xdg-config-home "/fish/completions/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/completions/" f)
                        (string-append "fish-completion-" f))))
       (list
        "fisher.fish"
        "tide.fish")))

(define plugins
  (map (lambda (f)
         `(,(xdg-config-home "/fish/" f)
           ,(local-file (user-home "/dev/dotfiles/fish/" f)
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
                   (list "local-stuff.fish" (local-file (user-home "/local-stuff.fish")))
                   funs
                   #;(append plugins (append funs (append completions confds)))
                   )))

;; fish and bash separate elements of a list with a different separator
(define list-separator-bash ":")
(define list-separator-fish " ")
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

(define (scm-bin name)
  (string-append scm-bin-dirname name))

(define (environment-vars list-separator)
  `(
    ;; used by ghog glog
    ("remotes" . ,(string-join (list "origin" "gitlab")
                               list-separator-bash))
    ("PATH" . ,(string-join
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
                list-separator))))

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
        (dotfiles-guix-home "/.bashrc_additions")
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
       (dotfiles-guix-home "/.bash_profile_additions")
       ;; prevent 'guix home: error: invalid name: `.bash_profile''
       "bash_profile_additions")))
     (environment-variables
      (environment-vars list-separator-bash))))

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
                    (user-home "/dev/dotfiles/fish/config.fish"))))
     ;; see also home-environment-variables-service-type
     ;; https://guix.gnu.org/manual/devel/en/html_node/Essential-Home-Services.html
     ;; (simple-service 'some-useful-env-vars-service
     ;;                 home-environment-variables-service-type
     ;;                 `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
     ;;                   ("SHELL" . ,(file-append zsh "/bin/zsh"))
     ;;                   ("USELESS_VAR" . #f)
     ;;                   ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

     (environment-variables
      (environment-vars list-separator-fish))))

   my-config-service

   (simple-service
    'scheme-files home-files-service-type
    (list
     `(,(scm-bin "/l")
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
     `(,(scm-bin "/spag")
       ,(program-file
         "spacemacs-git-fetch-rebase"
         (with-imported-modules `(((utils) => ,utils))
           #~(begin
               (use-modules (ice-9 rdelim)
                            (ice-9 regex)
                            (ice-9 popen)
                            (utils))

               (define* (git #:rest args)
                 (let ((h (getenv "HOME")))
                   (cons* "git"
                          (string-append "--git-dir=" h "/.emacs.d/.git")
                          (string-append "--work-tree=" h "/.emacs.d")
                          args)))

               (let ((args (command-line)))
                 (map exec
                      (list
                       (git "fetch" "--tags" "origin" "develop")
                       (git "rebase" "origin/develop" "develop")
                       (git "rebase" "develop" "cycle"))))))))

     (chmod-plus "rw")
     (chmod-plus "x")

    `(,(scm-bin "/ghog")
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

    `(,(scm-bin "/glo")
      ,(program-file
        "git-fech-and-rebase-from-origin"
        (with-imported-modules
            ;; TODO clarify is source-module-closure needed only for imports of
            ;; guix modules?
            `(((utils) => ,utils))
          #~(begin
              (use-modules (ice-9 rdelim)
                           (ice-9 regex)
                           (ice-9 popen)
                           (srfi srfi-1) ;; find
                           #;(loops for-loops)
                           ;; (srfi srfi-42)
                           (utils)
                           )

              (define origin-remotes '("origin" "github"))

              (define (string-in? lst string-elem)
                "Return the first element of @var{lst} that equals (string=)
@var{string-elem}, or @code{#f} if no such element is found.
Requires:
  (use-modules (srfi srfi-1))"
                (find (lambda (e) (string= string-elem e)) lst))

              ;; TODO quick and dirty - use global variable
              (define found #f)

              (let ((args (command-line)))
                ((compose
                  (partial
                   map
                   (lambda (remote)
                     (if (not found)
                         ;; TODO if-let
                         (let ((r (string-in? origin-remotes remote)))
                           (if r
                               (let* ((cmd (list "git" "fetch" "--tags" r))
                                      (ret (exec cmd)))
                                 (if (= 0 (car ret))
                                     (let* ((cmd (append (list "git" "rebase")
                                                         (cdr args)))
                                            (ret (exec cmd)))
                                       (if (= 0 (car ret))
                                           (set! found #t)
                                           (begin
                                             #;(format #t "Command failed:\n~a\n"
                                             (string-join cmd " "))
                                             (cdr ret))))
                                     (begin
                                       #;(format #t "Command failed:\n~a\n"
                                       (string-join cmd " "))
                                       (cdr ret)))))))))
                  #;dbg
                  (partial filter (lambda (remote) (not (string-match "heroku" remote))))
                  cdr
                  exec)
                 (list
                  "git" "remote")))))))

    `(,(scm-bin "/qemu-vm")
      ,(program-file
        "qemu-virt-machine"
        (with-imported-modules
            ;; TODO clarify is source-module-closure needed only for imports of
            ;; guix modules?
            `(((utils) => ,utils))
          #~(begin
              (use-modules (ice-9 rdelim)
                           (ice-9 popen)
                           (ice-9 getopt-long)
                           (utils)
                           )
              (define vmRAM "4G")
              (define vmHDDSize "16G")
              (define vmRemoteViewPort "5930")
              (define vmSSHPort "10022")
              (define vmApache2Port "10080")
              (define (vmCPUCores user)
                (number->string (/ (string->number
                                    (exec (list "sudo" (string-append "--user=" user)
                                                "nproc"))) 2)))

              (define ssh
                (format
                 #f
                 "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p ~a"
                 vmSSHPort))

              (define (user-localhost user)
                (format #f "~a@localhost" user))

              (define (usage vmUser)
                (format #f
                        "~a\n~a\n~a"
                        (format #f
                                "
### When: Could not access KVM kernel module: Permission denied
###     sudo usermod --append --groups kvm $USER

# Open a new terminal on the (Ubuntu) host and connect with:
   remote-viewer spice://localhost:~a & disown"
                                vmRemoteViewPort)
                        (format #f
                                "
# or with SSH on Ubuntu:
   sudo apt install openssh-server:
# with SSH on the host:
   ~a ~a" ssh (user-localhost vmUser))
                        (format #f
                                "
# File transfer example:
  set remoteShell \"~a\"
  set noWarn \"Permanently added '\\[localhost\\]:~a'\"
  rsync -avz --rsh=\"$remoteShell\" /tmp/foo ~a:/tmp/ 2>&1 | grep -v \"$noWarn\"
" ssh vmSSHPort (user-localhost vmUser))))

              (define (start-vm user isoFile qcow2File)
                ((compose
                  exec
                  (lambda (args)
                    (list
                     ;; This works, however without shared clipboard:
                     ;; qemu-system-x86_64 \
                     ;;     -nic user,model=virtio-net-pci \
                     ;;     -enable-kvm -m $vmRAM \
                     ;;     -device virtio-blk,drive=myhd \
                     ;;     -drive if=none,file=$qcow2File,id=myhd \
                     ;;     & disown
                     ;;
                     ;; With shared clipboard and SSH access:
                     "qemu-system-x86_64"
                     "-cdrom" isoFile
                     "-nic"
                     (format
                      #f
                      "user,model=virtio-net-pci,hostfwd=tcp::~a-:22,hostfwd=tcp::~a-:80"
                      vmSSHPort vmApache2Port)
                     "-enable-kvm" "-m" vmRAM
                     "-device" "virtio-blk,drive=myhd"
                     "-drive" (string-append "if=none,file=" qcow2File ",id=myhd")
                     ;; spice remote-viewer
                     #;
                     (string-join
                     (list
                     "-device"
                     "virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5"
                     "-chardev" "spicevmc,name=vdagent,id=vdagent"
                     "-device"
                     (string-append
                     "virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent"
                     ",name=com.redhat.spice.0")
                     "-spice" (string-append "port=" vmRemoteViewPort ",disable-ticketing=on")
                     "-vga" "qxl"
                     "-smp" (vmCPUCores user)
                     ) " ")
                     ;; "&" "disown" ;; TODO where is disown located???
                     ))
                  exec
                  (lambda ()
                    (if (access? qcow2File W_OK)
                        (begin
                          (format #t "DBG: qcow2File ~a exists and is writable~%" qcow2File)
                          (list))
                        (list
                         "sudo" (string-append "--user=" user)
                         "qemu-img" "create" "-f" "qcow2" qcow2File vmHDDSize))))))

              (define (main args)
                (let* ((option-spec
                        '((user     (single-char #\u) (value #t))
                          (iso-file (single-char #\i) (value #t))
                          (version  (single-char #\v) (value #f))
                          (help     (single-char #\h) (value #f))))
                       (options (getopt-long args option-spec))
                       (user           (option-ref options 'user #t))
                       (iso-file       (option-ref options 'iso-file #t))
                       (help-wanted    (option-ref options 'help #f))
                       (version-wanted (option-ref options 'version #f)))
                  (if (or version-wanted help-wanted)
                      (begin
                        #;(if version-wanted
                        (display "getopt-long-example version 0.3~%"))
                        (if help-wanted
                            (format #t "
quemu-vm [options]
  -v, --version    Display version
  -h, --help       Display this help
~a
"
                                    (usage user))))
                      (begin
                        (format #t "~a~%" (usage user))
                        (start-vm user iso-file (string-append iso-file ".qcow2"))))))))))
    ))

   #;
   (simple-service
    'bin-files home-files-service-type
    (map (lambda (f)
           `(,(string-append "bin/" f)
             ,(local-file (user-home "/dev/dotfiles/bin/" f)
                          (string-append "bin-" f))))
         (list "g1" "g1.scm"
               "guix-os" "guix-os.scm"
               "ubuntu-os" "ubuntu-os.scm"
              )))

   #;mcron-service

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
