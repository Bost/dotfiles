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
  (if my/narrowed-to-defun ;; global variable
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
  (let (;; TODO optionaly reselect last selected text
        ;; (was-normal-state-p (evil-normal-state-p))
        (was-visual-state-p (evil-visual-state-p)))
    (if was-visual-state-p
        ;; select text as if done from the insert state
        (let ((sel-text (buffer-substring-no-properties (region-beginning)
                                                        (region-end)))
              (mark-pos (mark))
              (point-pos (point)))
          (evil-exit-visual-state) ;; (evil-exit-visual-and-repeat)
          (if (< mark-pos point-pos) ;; can't be executed in the let-block. WTF???
              (exchange-point-and-mark)) ;; moving back
          (set-mark (point))
          (right-char (length sel-text))))
    (spacemacs/helm-project-smart-do-search-region-or-symbol)
    ;; (message "was-visual-state-p: %s" was-visual-state-p)
    ))

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
  "See also https://emacs.stackexchange.com/a/21093"
  (interactive)
  ;; TODO this doesn't work properly
  ;; (let ((start-marker (evil-get-marker ?\[))
  ;;                     (end-marker (evil-get-marker ?\])))
  ;;   (evil-visual-select start-marker end-marker))
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\])
  )

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

(defun my/hs-clojure-hide-namespace-and-folds ()
  "Hide the first (ns ...) expression in the file, and also all
the (^:fold ...) expressions."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (goto-char (point-min))
     (when (ignore-errors (re-search-forward "^(ns "))
       (hs-hide-block))

     (while (ignore-errors (re-search-forward "\\^:fold"))
       (hs-hide-block)
       (next-line)))))

;; deving on clojure-mode; WARNING: (getenv "dev") is undefined
(defun load-clojure-mode (file)
  (if (load-file file)
      (if (string= major-mode "clojure-mode")
          (progn
            (clojure-mode)
            (message "File loaded & clojure-mode set: %s" file))
        (message "File loaded: %s" file))
    (message "File loading failed: %s" file)))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
;; start-figwheel can be repeatedly called (is idempotent)
(figwheel-sidecar.repl-api/start-figwheel!)
(figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)
    ;; TODO (rename-buffer "*figwheel-cider*")
    (if (not (evil-insert-state-p))
        (evil-insert 0))))

(defun my/s-X ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (my/cider-figwheel-repl))

(defun my/copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning)
                                   (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!"))))

(defun my/paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active"))
    (insert (shell-command-to-string "xsel -o -b"))))

(defun my/fabricate-subst-cmd (&optional arg)
  "Place prepared subst command to the echo area.
Example 1.:
        :%s#\<\>##gc     - moves the point between '\<' and '\>'
Example 2.:
        :%s#fox#fox#gc   - moves the point after first 'x'"
  (interactive "p")
  (sp-copy-sexp)
  (evil-normal-state)
  (let* (;; Example 1.:
         ;; (sexp-str "%s#\\<\\>##gc")
         ;; (offset 6)
         ;;
         ;; Example 2.:
         (search-regex (format "%s" (car kill-ring)))
         (replace-regex (format "%s" (car kill-ring)))
         (sexp-str (format "%%s#\\<%s\\>#%s#gc" search-regex replace-regex))
         ;; 4 means: jump to the 2nd slash
         (offset (+ (length search-regex) 9)))
    ;; (cons .. offset) moves the point
    (evil-ex (cons sexp-str offset))))

(defmacro my/interactive-lambda (&rest body)
  ;; (defmacro my/interactive-lambda ...) prettyfied to "Λ"
  `(lambda ()
     (interactive)
     ,@body))

(defun my/other-window ()
  (interactive)
  (other-window 1)
  (my/flash-active-buffer))

(defun my/match-occurences-in-current-func-toggle ()
  "Match only occurrences in current function and the comment right above it."
  (interactive)
  (if iedit-mode
      (iedit-mode)  ;; switch off iedit-mode
    ;; 0 means: only occurrences in current ...
    (iedit-mode 0)))

(defun my/eval-current-defun1 (arg)
  "Doesn't work if there's a \"\" or () at the end of the function"
  (interactive "P")
  (let* ((point-pos (point)))
    (while (and (not (my/is-defun))
                (not (= (point) (point-min))))
      (sp-backward-symbol))
    (if t ;; (not (= point-pos (point)))
        (let* ((before-up (point)))
          (sp-up-sexp)
          (if (= before-up (point))
              (sp-forward-sexp))))
    ;; eval-sexp-fu-flash-mode is buggy
    (eval-last-sexp arg)
    (goto-char point-pos)))

;; (defun afoo () (message (format "")))

;; (defun af ()
;;   (defun bf ()
;;     (defun cf ())))

(defun my/eval-current-defun2 (arg)
  (interactive "P")
  (let* ((point-pos (point)))
    ;; (end-of-line)
    (search-backward (format "defun") nil t)
    (if t ;; (not (= point-pos (point)))
        (let* ((before-up (point)))
          (sp-up-sexp)
          (if (= before-up (point))
              (sp-forward-sexp))))
    (eval-last-sexp arg)
    ;; (message (format "search-backward"))
    (goto-char point-pos)))

(defun my/eval-current-defun (arg)
  "Evaluate the current i.e. inner def un.
E.g. in the (def un a () (def un b () (def un c ()))) this function allows
selective evaluation 'c' or 'b' or 'a' according to the point possition in
contrast to `eval-defun' which always evaluates just 'a' no matter where the
point is.
TODO still buggy - when not in a defun it evaluates preceding def un"
  (interactive "P")
  (let* ((point-pos (point)))
    (evil-insert-state nil)
    (goto-char (+ point-pos (length (concat "(def" "un"))))
    ;; separate the bracket from the string enables self-eval this function
    (search-backward (concat "(def" "un") nil t)
    (sp-forward-sexp)
    (eval-last-sexp arg)
    (goto-char point-pos)))

(defun my/elisp-insert-message ()
  (interactive)
  ;; (my/insert-sexp "(message (format \"\"))" 3)
  (my/insert-sexp "(message \"\")" 2))

(defun my/cider-save-and-load-current-buffer ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (cider-load-file (buffer-file-name))
  ;; (cider-switch-to-relevant-repl-buffer nil)
  )

(defun my/clojure-insert-log ()
  (interactive)
  (let* ((msg (if (equal major-mode 'clojurescript-mode)
                  "(.log js/console \"\")"
                "(println \"\")")))
    (my/insert-sexp msg 2)))

(defun my/clojure-insert-let ()
  (interactive)
  ;; (cljr-introduce-let) ; TODO see docu for cljr-introduce-let
  (my/insert-sexp "(let [])" 2))

(defun my/clojure-insert-for ()
  (interactive)
  (my/insert-sexp "(for [])" 2))

(defun my/clojure-insert-defn ()
  (interactive)
  (my/insert-sexp "(defn [])" 3))

(defun my/clojure-insert-doseq ()
  (interactive)
  (my/insert-sexp "(doseq [])" 2))

(defun my/clojure-insert-do ()
  (interactive)
  (my/insert-sexp "(do)" 1))

(defun my/clojure-toggle-reader-comment-fst-sexp-on-line ()
  (interactive)
  (let* ((point-pos1 (point)))
    (evil-insert-line 0)
    (let* ((point-pos2 (point))
           (cmtstr "#_")
           (cmtstr-len (length cmtstr))
           (line-start (buffer-substring-no-properties
                        point-pos2 (+ point-pos2 cmtstr-len))))
      (if (string= cmtstr line-start)
          (progn
            (delete-char cmtstr-len)
            (goto-char point-pos1)
            (left-char cmtstr-len))
        (progn
          (insert cmtstr)
          (goto-char point-pos1)
          (right-char cmtstr-len))))))

(defun my/clojure-toggle-reader-comment-current-sexp ()
  (interactive)
  (newline-and-indent)
  (my/clojure-toggle-reader-comment-fst-sexp-on-line))
