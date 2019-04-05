(defun my/what-face (pos)
  ;; see also C-u C-x =
  (interactive "d")
  ;; (clojure-mode)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/hilight-duplicate-lines()
  (interactive)
  (let ((count 0)
        line-re)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq count 0
              line-re (concat "^" (regexp-quote
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))
                              "$"))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (if (not (re-search-forward line-re nil t))
                (goto-char (point-max))
              (setq count (1+ count))
              (unless (< count 2)
                (hlt-highlight-region (line-beginning-position)
                                      (line-end-position)
                                      'font-lock-warning-face)
                (forward-line 1)))))
        (forward-line 1)))))

(defun my/close-buffer ()
  (interactive)
  (if (and (fboundp 'cider-repls) ;; is cider loaded?
           (member (current-buffer) (cider-repls)))
      (cider-quit)
    (if server-buffer-clients
        (server-edit)
      (kill-this-buffer))))

(defun my/grep (command-args)
  "Run Grep with user-specified COMMAND-ARGS, collect output in a buffer.
While Grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the *grep* \
buffer, to go to the lines where Grep found
matches.  To kill the Grep job before it finishes, type \\[kill-compilation].

Noninteractively, COMMAND-ARGS should specify the Grep command-line
arguments.

For doing a recursive `grep', see the `rgrep' command.  For running
Grep in a specific directory, see `lgrep'.

This command uses a special history list for its COMMAND-ARGS, so you
can easily repeat a grep command.

A prefix argument says to default the COMMAND-ARGS based on the current
tag the cursor is over, substituting it into the last Grep command
in the Grep command history (or into `grep-command' if that history
list is empty)."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run grep (like this): "
                                 (if current-prefix-arg default
                                   ;; grep-command
                                   (format "%s %s %s %s %s %s"
                                           "grep"
                                           "-nir"
                                           "\"latte \\\"1.0b1-SNAPSHOT\\\"\""
                                           "--exclude-dir={.git,target,LaTTe-upstream,latte-euroclojure-2016}"
                                           "--include=\*.{clj,cljs,cljc}"
                                           (format "%s/dec/latte-central/" (getenv "HOME"))))
                                 'grep-history
                                 (if current-prefix-arg nil default))))))
  (grep--save-buffers)
  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
                         (concat command-args " " null-device)
                       command-args)
                     'grep-mode))

(defun my/kill-buffers--forcefully (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns the count of killed buffers."
  (let* ((buffers (remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and name (not (string-equal name ""))
                            (or internal-too (/= (aref name 0) ?\s))
                            (string-match regexp name))))
                   (buffer-list))))
    (mapc 'kill-buffer buffers)
    (length buffers)))

(defun my/kill-buffers--force (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message "%d buffer(s) killed." (my/kill-buffers--forcefully regexp internal-too)))

(defun my/kill-buffers--magit ()
  "Kill all Magit buffers."
  (interactive)
  ;; (my/kill-buffers--forcefully "\*magit: .*\\|\*magit-.*")
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (find major-mode '(magit-status-mode
                                 magit-log-mode
                                 magit-diff-mode
                                 magit-revision-mode
                                 magit-stash-mode
                                 magit-process-mode))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i Magit buffer(s)." count))))

(defun my/buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer.
Example: (my/buffer-mode (current-buffer))"
  (with-current-buffer buffer-or-string
    major-mode))

(defun my/kill-buffers--unwanted ()
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        ;; find out buffer's major mode: (message "%s" major-mode)
        (when (find major-mode
                    '(magit-status-mode
                      magit-log-mode
                      magit-diff-mode
                      magit-revision-mode
                      magit-stash-mode
                      magit-process-mode
                      bs-mode ; *buffer-selection*
                      ;; *package-build-checkout* is in fundamenatal-mode
                      ;; *cider-refresh-log* is in fundamenatal-mode
                      cider-browse-ns-mode  ; for *cider-ns-browser*
                      cider-stacktrace-mode ; for *cider-error*
                      cider-docview-mode    ; for *cider-doc*
                      cider-inspector-mode  ; for *cider-inspect*
                      help-mode             ; for *Help*
                      dired-mode
                      ediff-meta-mode       ; for *Ediff Registry*
                      Info-mode             ; for *info*
                      spacemacs-buffer-mode ; for *spacemacs*
                      ))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (spacemacs/toggle-maximize-buffer)
      (message "Buffer(s) killed: %i" count))))

(defun my/kill-buffers--dired ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))

(defun my/flash-active-buffer ()
  "Blip background color of the active buffer."
  (interactive)
  (run-at-time "100 millisec" nil
               (lambda (remap-cookie)
                 (face-remap-remove-relative remap-cookie))
               (face-remap-add-relative 'default 'flash-active-buffer-face)))


(defun my/toggle-narrow-to-defun ()
  ;; TODO send to my/toggle-narrow-to-defun spacemacs upstream
  (interactive)
  (if my/narrowed-to-defun
      (widen)
    (narrow-to-defun))
  (setq my/narrowed-to-defun (not my/narrowed-to-defun)))

(defun my/split-other-window-and (f)
  (funcall f)
  (other-window 1))

(defun my/split-other-window-below ()
  (interactive)
  (my/split-other-window-and 'split-window-below))

(defun my/split-other-window-right ()
  (interactive)
  (my/split-other-window-and 'split-window-right))

(defun my/buffer-selection-show ()
  ;; Make a menu of buffers so you can manipulate buffers or the buffer list.
  (interactive)
  (bs-show nil)
  (if (not (evil-insert-state-p))
      (evil-insert 0)))

(defun my/sp-copy-sexp-msg ()
  (interactive)
  (sp-copy-sexp)
  (let* ((sexp (car kill-ring))
         (sexp-lines (split-string sexp "\n"))
         (sexp-len (length sexp))
         (cnt-sexp-lines (length sexp-lines))
         (fst-line (car sexp-lines))
         (fst-line-len (length fst-line))
         (maxchars 40))
    (message
     "sexp (%d chars, %d lines) copied to kill-ring: %s..."
     sexp-len
     cnt-sexp-lines
     fst-line
     ;; (or (>= fst-line-len maxchars) (> (length sexp-lines) 1))
     ;; (if (or (>= fst-line-len maxchars) (> (length sexp-lines) 1))
     ;;     (concat (subseq fst-line 0 (- maxchars 3)) "...")
     ;;   fst-line)
     )))

(defun my/sp-copy-back-sexp-msg ()
  (interactive)
  (let* ((point-pos (point)))
    (sp-backward-sexp)
    (my/sp-copy-sexp-msg)
    (goto-char point-pos)))

(defun my/select-inner (vi-str)
  "Select inner part of a string surrounded by bracket / quotation chars."
  (evil-normal-state)
  (execute-kbd-macro vi-str))

(defun my/disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(defun my/ediff-buffers-left-right (&optional arg)
  "ediff buffers in the left and right panel"
  (interactive "p")
  ;; make the current buffer to be the lef buffer thus prevent ediff swapping
  ;; left and right buffers
  (windmove-left)
  (ediff-buffers (buffer-name) ;; current buffer is the buffer-a
                 (buffer-name (other-window 1))))

(defun my/whitespace-cleanup ()
  (interactive)
  (whitespace-cleanup)
  (message (concat "s-n / s-N : narrow-to-defun / widen;"
                   " s-W : whitespace-cleanup")))

(defun my/whitespace-mode-toggle ()
  (interactive)
  (whitespace-mode 'toggle)
  (message (concat "s-n / s-N : narrow-to-defun / widen;"
                   " s-W : whitespace-cleanup")))

;; Spacemacs search: SPC s
;; search only in certain file-types:
;; 1. ag --list-file-types
;; 2. search only in .el files: TextToFind -G\.el$
;; (global-set-key (kbd "<f3>") 'helm-ag)

(defun my/helm-project-smart-do-search-region-or-symbol (&optional arg)
  "Search for selected text in the project. Even in visual state."
  (interactive "p")
  (if (evil-visual-state-p)
      ;; select text as if done from the insert state
      (let ((sel-text (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
            (mark-pos (mark))
            (point-pos (point)))
        (message (format "sel-text length: %d" (length sel-text)))
        (evil-exit-visual-state) ;; (evil-exit-visual-and-repeat)
        (if (< mark-pos point-pos) ;; can't be executed in the let-block. WTF???
            (exchange-point-and-mark)) ;; moving back
        (set-mark (point))
        (right-char (length sel-text))))
  (spacemacs/helm-project-smart-do-search-region-or-symbol))

(defun my/evil-avy-goto-char ()
  (interactive)
  (evil-avy-goto-char)
  (message "evil-avy-goto-char: SPC j j, <f2>, s-/"))

(defun my/alternate-buffer ()
  (interactive)
  ;; (popwin:switch-to-buffer)
  (spacemacs/alternate-buffer)
  (message "spacemacs/alternate-buffer: SPC TAB, <s-tab>"))

(defun my/evil-paste-after-from-0 ()
  ;; TODO evaluate: paste copied text multiple times
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun my/avy-goto-line ()
  (interactive)
  (avy-goto-line)
  (message "avy-goto-line: SPC j l, M-m j l, <C-f2>, C-s-/"))

(defun my/evil-select-pasted ()
  (interactive)
  (let ((start-marker (evil-get-marker ?[))
                      (end-marker (evil-get-marker ?])))
    (evil-visual-select start-marker end-marker)))

(defun my/toggle-large-file-setting ()
  (interactive)
  (let* ((msg "large-file-settings"))
    (cond
     ((not linum-mode)
      (progn
        ;; fontification is only deferred while there is input pending
        (setq jit-lock-defer-time 0)
        (spacemacs/toggle-line-numbers-on)
        (buffer-enable-undo)
        (font-lock-mode 1)
        (if (> (buffer-size) (* 1024 1024))
            (message "WARN %s disabled on a large file!" msg)
          (message "%s disabled" msg))))

     (t ;; default
      (progn
        (spacemacs/toggle-line-numbers-off)
        (buffer-disable-undo)
        (font-lock-mode -1)
        ;; fontification is not deferred.
        (setq jit-lock-defer-time nil)
        (message "%s enabled" msg))))))

(defun my/insert-sexp (str-sexp n-chars-back)
  (insert str-sexp)
  (left-char n-chars-back))
