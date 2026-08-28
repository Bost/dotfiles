;;; Public-key-only GnuPG homes backed by a host gpg-agent. The container
;;; imports only public key material and delegates signing to the host agent.

(define-module (dotf gpg-utils)
  #:use-module (bost common utils)
  #:use-module (dotf fs-utils) ; ensure-dir
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define (warn template . arguments)
  (apply format
         (current-error-port)
         (string-append "warning: " template "~%")
         arguments))

(define (gpg-private-keys-dir gpg-home)
  (str gpg-home "/private-keys-v1.d"))

(define-public (public-key-block? value)
  "Return true when VALUE contains an ASCII-armored public-key block."
  (and value (string-match "BEGIN PGP PUBLIC KEY BLOCK" value)))

(define (assert-no-private-keys! gpg-home)
  (let ((private-key-dir (gpg-private-keys-dir gpg-home)))
    (when (directory? private-key-dir)
      (let ((secret-key
             (exec-argv-first-line
              (list "find" private-key-dir
                    "-maxdepth" "1"
                    "-type" "f"
                    "-name" "*.key"
                    "-print"
                    "-quit"))))
        (when secret-key
          (error "Refusing to use container GNUPGHOME with private-key material"
                 secret-key))))))

(define-public (strip-forced-subkey-marker key)
  "Strip git's trailing `!' marker that forces a specific signing subkey;
the marker has no meaning to `gpg --export'."
  (let ((len (string-length key)))
    (if (and (> len 0) (char=? (string-ref key (- len 1)) #\!))
        (substring key 0 (- len 1))
        key)))

(define-public (signing-key)
  "Resolve $gpgPubKey, falling back to git's global user.signingkey.
The value may be a file, an armored public-key block, or a key identifier."
  (let* ((env-value (getenv "gpgPubKey"))
         (raw (if (non-empty-string? env-value)
                  env-value
                  (exec-argv-first-line
                   '("git" "config" "--global" "--get" "user.signingkey")))))
    (and raw (strip-forced-subkey-marker raw))))

(define (import-public-key! gpg-home)
  (let ((gpg-pub-key (signing-key)))
    (when (non-empty-string? gpg-pub-key)
      (let* ((staged-public-key (str gpg-home "/public-key.asc"))
             (public-key-file
              (cond
               ((readable-file? gpg-pub-key) gpg-pub-key)
               ((public-key-block? gpg-pub-key)
                (call-with-output-file staged-public-key
                  (lambda (port)
                    (display gpg-pub-key port)
                    (newline port)))
                staged-public-key)
               (else
                (when (file-exists? staged-public-key)
                  (delete-file staged-public-key))
                (unless (exec-argv-success?
                         (list "gpg"
                               "--batch"
                               "--yes"
                               "--armor"
                               "--output" staged-public-key
                               "--export" gpg-pub-key))
                  (error "Failed to export public GPG key selected by $gpgPubKey or git user.signingkey"
                         gpg-pub-key))
                staged-public-key))))
        (unless (exec-argv-success?
                 (list "gpg"
                       "--batch"
                       "--homedir" gpg-home
                       "--import" public-key-file))
          (error "Failed to import public GPG key into container GNUPGHOME"
                 public-key-file))))))

(define (remove-empty-private-keys-dir! gpg-home)
  "Remove GPG-HOME/private-keys-v1.d after verifying that it has no key
material. GnuPG creates this directory even when importing only public keys."
  (let ((private-key-dir (gpg-private-keys-dir gpg-home)))
    (when (directory? private-key-dir)
      (unless (exec-argv-success? (list "rmdir" private-key-dir))
        (format
         (current-error-port)
         "WRN: could not remove ~a (expected an empty directory); inspect it~%"
         private-key-dir)))))

(define-public (prepare-public-gpg-home! gpg-home)
  "Prepare a mode-0700, public-key-only GPG-HOME while refusing any private
key material before and after importing the configured public key."
  (for-each (cut <> gpg-home)
            (list ensure-dir
                  (lambda (dir)
                    (unless (exec-argv-success? (list "chmod" "700" dir))
                      (error "Failed to secure GPG home permissions" dir)))
                  assert-no-private-keys!
                  import-public-key!
                  assert-no-private-keys!
                  remove-empty-private-keys-dir!)))

(define (existing-gpg-agent-socket name)
  (let ((path (exec-argv-first-line (list "gpgconf" "--list-dirs" name))))
    (and (socket? path) path)))

(define-public (host-gpg-agent-socket)
  "Refresh the host agent and return its extra or regular socket path for
exposure inside the container."
  (exec-argv-success? '("gpg-connect-agent" "updatestartuptty" "/bye"))
  (any existing-gpg-agent-socket '("agent-extra-socket" "agent-socket")))

(define-public (set-gpg-tty-from-current-terminal!)
  "Set $GPG_TTY from `tty' when attached to a terminal."
  (let ((tty (exec-argv-first-line '("tty") #:verbose #f)))
    (when tty (setenv "GPG_TTY" tty))))

(define (write-public-key! path content)
  (call-with-output-file path
    (lambda (port)
      (display content port)
      (newline port)))
  (unless (exec-argv-success? (list "chmod" "0644" path) #:verbose #f)
    (error "Failed to set public-key file permissions" path))
  #t)

(define (materialize-public-key! key target)
  (cond
   ((not (non-empty-string? key))
    (warn "neither gpgPubKey nor git user.signingkey is set; signed commits may fail")
    #f)
   ((readable-file? key)
    (and (exec-argv-success?
          (list "cp" "--no-dereference" key target) #:verbose #f)
         (exec-argv-success? (list "chmod" "0644" target) #:verbose #f)))
   ((public-key-block? key)
    (write-public-key! target key))
   (else
    (if (exec-argv-success?
         (list "gpg" "--batch" "--yes" "--armor"
               "--output" target "--export" key)
         #:verbose #f)
        #t
        (begin
          (warn "could not export public signing key ~a; signed commits may fail" key)
          #f)))))

(define (link-agent-socket! gpg-home socket)
  (and socket
       (socket? socket)
       (exec-argv-success?
        (list "ln" "--symbolic" "--force" socket
              (path-join gpg-home "S.gpg-agent"))
        #:verbose #f)))

(define-public (prepare-ephemeral-gpg-home! gpg-home)
  "Populate ephemeral GPG-HOME with public key material, link the host agent
socket into it, set $GNUPGHOME, and return the host socket (or #f)."
  (let* ((socket (host-gpg-agent-socket))
         (public-key-file (path-join gpg-home "public-signing-key.asc")))
    (when (materialize-public-key! (signing-key) public-key-file)
      (unless (exec-argv-success?
               (list "gpg" "--homedir" gpg-home "--batch" "--quiet"
                     "--import" public-key-file)
               #:verbose #f)
        (warn "could not import the public signing key; signed commits may fail"))
      (when (file-exists? public-key-file)
        (delete-file public-key-file)))
    (unless (link-agent-socket! gpg-home socket)
      (warn "no usable host gpg-agent socket found; signed commits may fail"))
    (setenv "GNUPGHOME" gpg-home)
    socket))
