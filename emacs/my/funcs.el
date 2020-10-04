(defun my=what-face (pos)
  ;; see also C-u C-x =
  (interactive "d")
  ;; (clojure-mode)
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my=hilight-duplicate-lines()
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

(defun my=close-buffer ()
  (interactive)
  (if (and (fboundp 'cider-repls) ;; is cider loaded?
           (member (current-buffer) (cider-repls)))
      (cider-quit)
    (if server-buffer-clients
        (server-edit)
      (kill-this-buffer))))

(defun my=grep (command-args)
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

(defun my=kill-buffers--forcefully (regexp &optional internal-too)
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

(defun my=kill-buffers--force (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP.
See the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message "%d buffer(s) killed." (my=kill-buffers--forcefully regexp internal-too)))

(defun my=kill-buffers--magit ()
  "Kill all Magit buffers."
  (interactive)
  ;; (my=kill-buffers--forcefully "\*magit: .*\\|\*magit-.*")
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

(defun my=buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer.
Example: (my=buffer-mode (current-buffer))"
  (with-current-buffer buffer-or-string
    major-mode))

(defun my=kill-buffers--unwanted ()
  "Kill all unwanted buffers and delete other windows so that only one remains
displayed."
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
                      ;; in fundamenatal-mode:
                      ;; *package-build-checkout*
                      ;; *cider-refresh-log*
                      ;; *edn*
                      ;; *Backtrace*
                      ;; *Help*
                      cider-browse-ns-mode  ; for *cider-ns-browser*
                      cider-stacktrace-mode ; for *cider-error*
                      cider-docview-mode    ; for *cider-doc*
                      cider-inspector-mode  ; for *cider-inspect*
                      help-mode             ; for *Help*
                      dired-mode
                      ediff-meta-mode       ; for *Ediff Registry*
                      Info-mode             ; for *info*
                      spacemacs-buffer-mode ; for *spacemacs*
                      compilation-mode      ; for *Compile-Log*
                      ))
          (setq count (1+ count))
          (kill-buffer buffer)))
      (delete-other-windows)
      (message "Buffer(s) killed: %i" count))))

(defun my=kill-buffers--dired ()
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

(defun my=flash-active-buffer ()
  "Blip background color of the active buffer."
  (interactive)
  (run-at-time "100 millisec" nil
               (lambda (remap-cookie)
                 (face-remap-remove-relative remap-cookie))
               (face-remap-add-relative
                ;; 'hl-line ;; doesn't work on the "@@-lines" in magit buffers
                'default
                'flash-active-buffer-face)))

(defmacro my=create-cycle-fun (cycl-fun-name last-fun loop-list)
  "`cycl-fun-name' is an interactive function to be created call
`last-fun' is a variable to be created containing the last executed function from the `loop-list'
`loop-list' is a list of functions to loop-list over

Usage:
 (my=create-cycle-fun my=toggle my=last-fun
                   '((lambda () (message \"foo\") \"foo\")
                     (lambda () (message \"bar\") \"bar\")))

Try: M-x my=toggle or:
 (message \"1. %s, 2. %s 3. %s; last: %s\" (my=toggle) (my=toggle) (my=toggle) my=last-fun)

Note how function advising works - e.g.:
 (advice-add 'my=toggle :after (lambda () (recenter)))
 (advice-remove :after 'my=toggle)"
  `(progn
     (setq ,last-fun nil)
     (defun ,cycl-fun-name ()
       (interactive)
       (setq ,last-fun
             (car (or (cdr (memq ,last-fun ,loop-list))
                      ;; if ,last-fun isn't in cycleable, start over
                      ,loop-list)))
       (funcall ,last-fun))))

;; {{{ toggling "narrow-to-defun"
(my=create-cycle-fun my=cycle-defun-narrow-modes
                     my=last-defun-narrow-mode
                     '(narrow-to-defun widen))

(advice-add 'my=cycle-defun-narrow-modes :after (lambda () (recenter)))
;; (advice-remove :after 'my=cycle-defun-narrow-modes)

;; 's' is the Win-key between Ctrl and Alt
;; (bind-keys :map global-map ("s-n" . my=cycle-defun-narrow-modes))
;; }}}

;; {{{ cycling line-number modes
(my=create-cycle-fun my=cycle-line-number-modes
                     my=last-line-number-mode
                     '(spacemacs/toggle-relative-line-numbers-on
                       spacemacs/toggle-relative-line-numbers-off
                       spacemacs/toggle-line-numbers-on
                       spacemacs/toggle-line-numbers-off))

;; 's' is the Win-key between Ctrl and Alt
;; (bind-keys :map global-map ("s-L" . my=cycle-line-number-modes))
;; }}}

(my=create-cycle-fun my=cycle-large-file-settings
                     my=last-large-file-settings
                     '(my=shenanigans-on my=shenanigans-off))

(defun my=split-other-window-and (f)
  (funcall f)
  (other-window 1))

(defun my=split-other-window-below ()
  (interactive)
  (my=split-other-window-and 'split-window-below))

(defun my=split-other-window-right ()
  (interactive)
  (my=split-other-window-and 'split-window-right))

(defun my=buffer-selection-show ()
  "Make a menu of buffers so you can manipulate buffers or the buffer list."
  (interactive)
  (bs-show nil)
  (if (not (evil-insert-state-p))
      (evil-insert 0)))

(defun my=select-inner (vi-str)
  "Select inner part of a string surrounded by bracket / quotation chars."
  (evil-normal-state)
  (execute-kbd-macro vi-str))

;; use named functions for meaningful shortcuts in the listing
;; M-x which-key-show-top-level / SPC h k
(defun my=select-in-ang-bracket () (interactive) (my=select-inner "vi<"))
(defun my=select-in-sqr-bracket () (interactive) (my=select-inner "vi["))
(defun my=select-in-rnd-bracket () (interactive) (my=select-inner "vi("))
(defun my=select-in-crl-bracket () (interactive) (my=select-inner "vi{"))
(defun my=select-in-string () (interactive) (my=select-inner "vi\""))

(defun my=disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(defun my=ediff-buffers-left-right (&optional arg)
  "ediff buffers in the left and right panel"
  (interactive "p")
  ;; make the current buffer to be the lef buffer thus prevent ediff swapping
  ;; left and right buffers
  (windmove-left)
  (ediff-buffers (buffer-name) ;; current buffer is the buffer-a
                 (buffer-name (other-window 1))))

(defun my=whitespace-cleanup ()
  (interactive)
  (whitespace-cleanup)
  (message "s-n : my=toggle-narrow-to-defun; s-W : whitespace-cleanup"))

(defun my=whitespace-mode-toggle ()
  (interactive)
  (whitespace-mode 'toggle)
  (spacemacs/toggle-fill-column-indicator))

;; Spacemacs search: SPC s
;; search only in certain file-types:
;; 1. ag --list-file-types
;; 2. search only in .el files: TextToFind -G\.el$
;; (global-set-key (kbd "<f3>") 'helm-ag)

(defun my=search-region-or-symbol (&optional arg)
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

(defun my=browse-or-search (&optional arg)
  "'&optional arg' must be declared otherwise the key binding doesn't work"
  (interactive "p")
  (let* ((engine "DuckDuckGo"))
    ;; nil means: no quoted search
    ;; (if (or (evil-visual-state-p) (region-active-p))
    ;;     (google-this-region nil)
    ;;   (google-this nil))
    (cond
     ((string-prefix-p "http" (thing-at-point 'url))
      ;; https://www.google.com
      (browse-url (car (browse-url-interactive-arg "Browse URL: "))))

     ((evil-visual-state-p)
      ;; select text as if done from the insert state
      (let* ((sel-text (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
             (txt (read-string
                   (format "[evil-visual-state-p] Search %s: " engine)
                   sel-text)))
        (engine/search-duck-duck-go txt)))

     ((region-active-p)
      ;; select text as if done from the insert state
      (let* ((sel-text (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
             (txt (read-string
                   (format "[region-active] Search %s: " engine)
                   sel-text)))
        (engine/search-duck-duck-go txt)))

     ;; default
     (t
      (let* ((txt (read-string (format "[default] Search %s: " engine)
                               (thing-at-point 'symbol))))
        (engine/search-duck-duck-go txt))))))

(defun my=evil-avy-goto-char-timer ()
  (interactive)
  (evil-avy-goto-char-timer)
  (message "evil-avy-goto-char-timer: SPC j j, f, <f2>"))

(defun my=alternate-buffer ()
  (interactive)
  ;; (popwin:switch-to-buffer)
  (spacemacs/alternate-buffer)
  (message "spacemacs/alternate-buffer: SPC TAB, <s-tab>"))

(defun my=evil-paste-after-from-0 ()
  ;; TODO evaluate: paste copied text multiple times
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun my=avy-goto-line ()
  (interactive)
  (avy-goto-line)
  (message "avy-goto-line: SPC j l, M-m j l, <C-f2>, C-s-/"))

(defun my=evil-select-pasted ()
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

(defun my=shenanigans-on ()
  "Switch on most of the graphical goodies. Inverse of
`my=shenanigans-off'."
  (interactive)
  ;; fontification is only deferred while there is input pending
  (setq jit-lock-defer-time 0)
  (spacemacs/toggle-line-numbers-on)
  (buffer-enable-undo)
  (font-lock-mode 1)
  (message "Shenanigans enabled"))

(defun my=shenanigans-off ()
  "Switch on most of the graphical goodies. Useful when editing
large files. Inverse of `my=shenanigans-on'."
  (interactive)
  (spacemacs/toggle-line-numbers-off)
  (buffer-disable-undo)
  (font-lock-mode -1)
  ;; fontification is not deferred.
  (setq jit-lock-defer-time nil)
  (message "Shenanigans disabled"))

(defun my=insert-sexp (str-sexp n-chars-back)
  (insert str-sexp)
  (left-char n-chars-back))

(defun my=delete-next-sexp (&optional arg)
  "Delete the sexp (balanced expression) following point w/o
yanking it. See `kill-sexp'."
  (interactive "p")
  (let ((beg (point)))
    (forward-sexp 1)
    (let ((end (point)))
      (delete-region beg end))))

(defun my=delete-prev-sexp (&optional arg)
  "Delete the sexp (balanced expression) following point w/o
yanking it. See `kill-sexp'."
  (interactive "p")
  (let ((beg (point)))
    (forward-sexp -1)
    (let ((end (point)))
      (delete-region end beg)))) ;; beg & end are swapped

(defun my=hs-clojure-hide-namespace-and-folds ()
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

(defun my=switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my=cider-figwheel-repl ()
  "Start figwheel"
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

(defun my=s-X ()
  "Switch to cider repl & start figwheel"
  (interactive)
  (cider-switch-to-repl-buffer)
  (my=cider-figwheel-repl))

(defun my=cider-switch-to-repl-buffer ()
  "Connect (if not connected yet) and switch to cider repl buffer"
  (interactive)
  (unless (cider-connected-p)
    (cider-connect-clj))
  (cider-switch-to-repl-buffer))

(defun my=copy-to-clipboard ()
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

(defun my=paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active"))
    (insert (shell-command-to-string "xsel -o -b"))))

(defun my=fabricate-subst-cmd (&optional arg)
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

(defmacro my=interactive-lambda (&rest body)
  ;; (defmacro my=interactive-lambda ...) prettyfied to "Λ"
  `(lambda ()
     (interactive)
     ,@body))

(defun my/other-window ()
  (interactive)
  (other-window 1)
  (my/flash-active-buffer))

(defun my=iedit-mode-toggle ()
  "Match only occurrences in current function and the comment right above it."
  (interactive)
  ;; TODO when C-g pressed and (= my=iedit-mode t) then (setq my=iedit-mode nil)
  (if my=iedit-mode
      (progn
        (evil-iedit-state/quit-iedit-mode)
        (setq my=iedit-mode nil))
    (progn
      ;; 0 means: only occurrences in current ...
      (evil-iedit-state/iedit-mode 0)
      ;; (evil-iedit-state/iedit-mode) ;; M-H iedit-restrict-function
      (setq my=iedit-mode t))))

(defun my=eval-current-defun1 (arg)
  "Doesn't work if there's a \"\" or () at the end of the function"
  (interactive "P")
  (let* ((point-pos (point)))
    (while (and (not (my=is-defun))
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

(defun my=eval-current-defun2 (arg)
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

(defun my=eval-current-defun (arg)
  "Evaluate the current i.e. inner def un.
E.g. in the (def un a () (def un b () (def un c ()))) this function allows
selective evaluation 'c' or 'b' or 'a' according to the point possition in
contrast to `eval-defun' which always evaluates just 'a' no matter where the
point is.
TODO still buggy - when not in a defun it evaluates preceding defun"
  (interactive "P")
  (let* ((point-pos (point)))
    (evil-insert-state nil)
    (goto-char (+ point-pos (length (concat "(def" "un"))))
    ;; separate the bracket from the string enables self-eval this function
    (search-backward (concat "(def" "un") nil t)
    (sp-forward-sexp)
    (eval-last-sexp arg)
    (goto-char point-pos)))

(defun my=elisp-insert-message ()
  "See `lv-message' for semi-permanent hints, not interfering
with the Echo Area."
  (interactive)
  (my=insert-sexp "(message \"%s\" )" 1))

(defun my=elisp-insert-defun ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "defun")))

(defun my=cider-save-and-load-current-buffer ()
  "TODO call `cider-repl-set-ns' only if `cider-load-file' succeeded"
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (cider-load-file (buffer-file-name))
  (cider-repl-set-ns (cider-current-ns))
  ;; (cider-switch-to-relevant-repl-buffer nil)
  )

(defun my=cider-reload-ns-from-file ()
  "TODO get the filename from (cider-current-ns) and reload it"
  (interactive)
  (message "[%s] cider-current-ns %s"
           'my=cider-reload-ns-from-file
           (cider-current-ns))
  ;; (my=cider-switch-to-repl-buffer)
  ;; (my=cider-save-and-load-current-buffer)
  )

(defun my=clj-insert-log ()
  (interactive)
  (let* ((msg (if (equal major-mode 'clojurescript-mode)
                  "(.log js/console \"\")"
                "(println \"\")")))
    (my=insert-sexp msg 2)))

(defun my=clj-insert-remove-fn ()
  (interactive)
  (my=insert-sexp "(remove (fn []))" 3))

(defun my=clj-insert-filter-fn ()
  (interactive)
  (my=insert-sexp "(filter (fn []))" 3))

(defun my=clj-insert-type ()
  (interactive)
  (my=insert-sexp "(type )" 1))

(defun my=clj-insert-map-fn ()
  (interactive)
  (my=insert-sexp "(map (fn []))" 3))

(defun my=clj-insert-let ()
  (interactive)
  ;; (cljr-introduce-let) ; TODO see docu for cljr-introduce-let
  (my=insert-sexp "(let [])" 2))

(defun my=clj-insert-for ()
  (interactive)
  (my=insert-sexp "(for [])" 2))

(defun my=clj-insert-defn ()
  (interactive)
  (my=insert-sexp "(defn [])" 3))

(defun my=clj-insert-doseq ()
  (interactive)
  (my=insert-sexp "(doseq [])" 2))

(defun my=clj-insert-do ()
  (interactive)
  (my=insert-sexp "(do)" 1))

(defun my=point-max-p () (= (point) (point-max)))
(defalias 'my=end-of-file-p 'my=point-max-p)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun my=toggle-reader-comment-fst-sexp-on-line (cmtstr)
  "If line starts with a line comment, toggle the comment.
Otherwise toggle the reader comment"
  (if (and (current-line-empty-p) (my=end-of-file-p))
      (progn
        (message "Point at the end-of-file. Doing nothing."))
    (let* ((point-pos1 (point)))
      ;; Switch to insert state at beginning of current line.
      ;; 0 means: don't insert any line
      (evil-insert-line 0)
      (let* ((point-pos2 (point))
             (is-comment-only (comment-only-p point-pos2
                                              (save-excursion
                                                (move-end-of-line 1)
                                                (point)))))
        (if is-comment-only
            ;; (evilnc-comment-or-uncomment-lines 1)
            (spacemacs/comment-or-uncomment-lines 1)
          (let* ((cmtstr-len (length cmtstr))
                 (line-start (buffer-substring-no-properties
                              point-pos2 (+ point-pos2 cmtstr-len))))
            ;; (message "line-start %s" line-start)
            (if (string= cmtstr line-start)
                (progn
                  (delete-char cmtstr-len)
                  (goto-char (- point-pos1 cmtstr-len)))
              (progn
                (insert cmtstr)
                (goto-char (+ point-pos1 cmtstr-len))))))))))

(defun my=racket-toggle-reader-comment-fst-sexp-on-line ()
  (interactive)
  (my=toggle-reader-comment-fst-sexp-on-line "#;"))

(defun my=clj-toggle-reader-comment-fst-sexp-on-line ()
  (interactive)
  (my=toggle-reader-comment-fst-sexp-on-line "#_"))

(defun my=racket-toggle-reader-comment-current-sexp ()
  (interactive)
  (newline-and-indent)
  (my=racket-toggle-reader-comment-fst-sexp-on-line))

(defun my=clj-toggle-reader-comment-current-sexp ()
  (interactive)
  (newline-and-indent)
  (my=clj-toggle-reader-comment-fst-sexp-on-line))

(defun my=helm-mini ()
  ;; (define-key helm-map (kbd "s-a") nil)
  ;; (unbind-key (kbd "s-a") helm-map)
  (if (boundp 'helm-map)
      (define-key helm-map (kbd "s-a") 'helm-next-line)))

(defun my=cider-clear-compilation-highlights ()
  (interactive)
  (cider-clear-compilation-highlights t))

(defun my=repl-insert-cmd (s)
  (cider-switch-to-repl-buffer)
  (insert s))

(setf my=bot-ns "corona") ;; "corona.bot"

(defun my=telegram-restart ()
  (interactive)
  (my=repl-insert-cmd (format "(%s.telegram/restart)" my=bot-ns)))

(defun my=web-restart ()
  (interactive)
  (my=repl-insert-cmd "(corona.web/restart)"))

(defun my=show-pic ()
  (interactive)
  (let* ((case ":i")
         (prm (format
               "{:day %s :threshold %s :threshold-increase %s :case %s :stats %s}"
               "(count (%s.api.expdev07/raw-dates-unsorted))"
               my=bot-ns
               (format "(%s.common/min-threshold %s)" my=bot-ns case)
               (format "(%s.common/threshold-increase %s)" my=bot-ns case)
               case
               (format "(%s.api.v1/pic-data)" my=bot-ns))))
    (my=repl-insert-cmd
     (format "(cljplot.core/show (%s.plot/plot-all-by-case %s))"
             my=bot-ns
             prm))))

(defun my=show-pic-for-pred ()
  (interactive)
  (let* ((prm (format "{:day %s :cc %s :stats %s}"
                      (format "(count (%s.api.expdev07/raw-dates-unsorted))" my=bot-ns)
                      "\"ZZ\""
                      (format "(%s.api.v1/pic-data)" my=bot-ns))))
    (my=repl-insert-cmd
     (format "(cljplot.core/show (%s.plot/plot-country %s))"
             my=bot-ns
             prm))))

(defun my=stop-synths-metronoms ()
  (interactive)
  (my=repl-insert-cmd "(stop)")
  (cider-repl-return))

(defun my=cider-unmap-current-namespace ()
  (interactive)
  (my=repl-insert-cmd "(map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))")
  ;; (cider-repl-return)
  )
