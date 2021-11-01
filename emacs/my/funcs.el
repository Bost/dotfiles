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

(defun my=buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer.
Example: (my=buffer-mode (current-buffer))"
  (with-current-buffer buffer-or-string
    major-mode))

(defun my=split-other-window-and (f)
  (funcall f)
  (other-window 1))

(defun my=split-other-window-below ()
  (interactive)
  (my=split-other-window-and 'split-window-below))

(defun my=split-other-window-right ()
  (interactive)
  (my=split-other-window-and 'split-window-right))

(defun my=evil-insert ()
  "Switch to evil insert mode."
  ;; (interactive)
  (if (not (evil-insert-state-p))
      (evil-insert 0)))

(defun my=buffer-selection-show ()
  "Make a menu of buffers so you can manipulate buffers or the buffer list."
  (interactive)
  (bs-show nil)
  (my=evil-insert))

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
  ;; left and right buffers; `windmove-left' signals an error if no window is at
  ;; the desired location(, unless <not my case>)
  (condition-case nil
      (windmove-left)
    (error nil))
  (ediff-buffers (buffer-name) ;; current buffer is the buffer-a
                 (buffer-name (other-window 1))))

(defun my=whitespace-cleanup-msg ()
  (message "whitespace-cleanup"))

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
  "Search for selected text in the project. Even in visual state.
See `spacemacs/helm-project-smart-do-search-region-or-symbol'"
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

(defun my=evil-avy-goto-char-timer-msg ()
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

(defun my=avy-goto-line-msg ()
  (message "avy-goto-line: SPC j l, M-m j l, <C-f2>, C-s-/"))

(defun my=evil-select-pasted ()
  "See also https://emacs.stackexchange.com/a/21093"
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\])
  (message "my=evil-select-pasted - does the same as the macro under: SPC g p")
  )

(defun my=yank-and-select ()
  (interactive)
  (let ((point-begin (point)))
    ;; (clipboard-yank)
    (yank)
    ;; (evil-visual-make-selection)
    (evil-visual-select point-begin (- (point) 1))))

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

(defun my=insert-str (s &optional n-chars-back)
  (interactive "p")
  (insert s)
  (left-char n-chars-back)
  (my=evil-insert))

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
    (my=evil-insert)))

(defun my=switch-to-repl-start-figwheel ()
  "Switch to cider repl & start figwheel"
  (interactive)
  (cider-switch-to-repl-buffer)
  (my=cider-figwheel-repl))

(defun my=cider-switch-to-repl-buffer ()
  "Connect (if not connected yet) and switch to cider repl buffer"
  (interactive)
  (unless (cider-connected-p)
    ;; previously (cider-connect-clj)
    (cider-jack-in-clj nil))
  (cider-switch-to-repl-buffer))

(defun my=copy-to-clipboard ()
  "Copy selection to x-clipboard or clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (call-interactively 'clipboard-kill-ring-save)
        (message "%s %s"
                 "The DISPLAY is graphic."
                 "Region yanked to the x-clipboard!"))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning)
                                   (region-end) "xsel -i -b")
          (deactivate-mark)
          (message "%s %s"
                   "The DISPLAY not is graphic."
                   "Region yanked to the clipboard!"))
      (message "%s %s"
               "The DISPLAY not is graphic and no region active."
               "Can't yank to the clipboard!"))))

(defun my=paste-from-clipboard ()
  "Paste from the x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; (clipboard-yank)
        (yank)
        (message "The DISPLAY is graphic."))
    (insert (shell-command-to-string "xsel -o -b"))))

(defun my=fabricate-subst-cmd (&optional args)
  "Place prepared subst command to the echo area. Must be declared with
`&optional args'. Otherwise it wont work.
E.g.:
     :%s#\\=\\<\\=\\>##gc     - places the point between `\<' and `\>'
     :%s#fox#fox#gc   - places the point after the first `x'"
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

(defun my=search-namespace (&optional args)
  (interactive "p")
  (sp-copy-sexp)
  ;; (message "%s" kill-ring)
  (evil-normal-state)
  (let* ((sexp-str (format "%s\\/" (car kill-ring))))
    ;; (evil-ex-search-forward)
    ;; (insert sexp-str)
    ;; (evil-ex-search-full-pattern sexp-str 1 'forward)
    (evil-ex-start-word-search t 'forward 0 sexp-str)
    ;; (evil-ex-search-start-session)
    ;; (exit-minibuffer)
    ))

(global-set-key (kbd "<s-f9>") 'my=search-namespace)

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
  (my=insert-str "(message \"%s\" )" 1))

(defun my=elisp-insert-defun ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "defun")))

(defun my=cider-save-and-load-current-buffer ()
  "TODO call `cider-repl-set-ns' only if `cider-load-file' succeeded"
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  ;; Set the ns in the first step...
  (cider-repl-set-ns (cider-current-ns))
  ;; ... so if there's an error in the buffer being loaded then the repl is
  ;; ready to be used for the problem analysis.
  (cider-load-file (buffer-file-name))
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
                "(debugf \"\")"
                ;; "(println \"\")"
                )))
    (my=insert-str msg 2)))

(defun my=racket-insert-log ()
  (interactive)
  (let* ((msg "(printf \"\\n\")"))
    (my=insert-str msg 4)))

(defun my=clj-insert-remove-fn ()
  (interactive)
  (my=insert-str "(remove (fn []))" 3))

(defun my=clj-insert-filter-fn ()
  (interactive)
  (my=insert-str "(filter (fn []))" 3))

(defun my=clj-insert-type ()
  (interactive)
  (my=insert-str "(type )" 1))

(defun my=clj-insert-map-fn ()
  (interactive)
  (my=insert-str "(map (fn []))" 3))

(defun my=clj-insert-let ()
  (interactive)
  ;; (cljr-introduce-let) ; TODO see docu for cljr-introduce-let
  (my=insert-str "(let [])" 2))

(defun my=elisp-insert-let ()
  (interactive)
  (my=insert-str "(let (()))" 3))

(defun my=clj-insert-for ()
  (interactive)
  (my=insert-str "(for [])" 2))

(defun my=insert-clojuredocs ()
  (interactive)
  (my=insert-str "clojuredocs"))

(defun my=clj-insert-comp ()
  (interactive)
  (my=insert-str "((comp ))" 2))

(defun my=insert-partial ()
  (interactive)
  (my=insert-str "partial " 1))

(defun my=racket-insert-fn ()
  (interactive)
  (my=insert-str "(lambda ())" 2))

(defun my=clj-insert-fn ()
  (interactive)
  (my=insert-str "(fn [])" 2))

(defun my=clj-insert-def ()
  (interactive)
  (my=insert-str "(def )" 1))

(defun my=clj-insert-defn ()
  (interactive)
  (my=insert-str "(defn [])" 3))

(defun my=clj-insert-doseq ()
  (interactive)
  (my=insert-str "(doseq [])" 2))

(defun my=clj-insert-do ()
  (interactive)
  (my=insert-str "(do)" 1))

(defun my=point-max-p () (= (point) (point-max)))
(defalias 'my=end-of-file-p 'my=point-max-p)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

;; TODO Implement using the `spacemacs/toggle'
(defun my=toggle-reader-comment-fst-sexp-on-line (cmtstr)
  "If line starts with a line comment, toggle the comment.
Otherwise toggle the reader comment."
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

(defun my=clj-toggle-reader-comment-fst-sexp-on-line (&optional arg)
  "When invoked with prefix <C u 2> it toggles two forms - for key-value pair"
  (interactive "p")
  (my=toggle-reader-comment-fst-sexp-on-line
   (if (eq 2 arg)
       "#_#_"
     "#_")))

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
  (when (boundp 'helm-map)
    (define-key helm-map (kbd "s-a") 'helm-next-line)
    (define-key helm-map (kbd "s-]") 'helm-next-line)))

(defun my=cider-clear-compilation-highlights ()
  (interactive)
  (cider-clear-compilation-highlights t))

(defun my=repl-insert-cmd (s)
  (cider-switch-to-repl-buffer)
  (insert s))

(setf my=bot-ns "corona") ;; "corona.bot"

(defun my=telegram-restart ()
  (interactive)
  ;; (cider-switch-to-repl-buffer)
  ;; (cider-switch-to-last-clojure-buffer)
  (cider-ns-refresh)
  (my=repl-insert-cmd
   (format
    "(System/gc) (swap! %s.api.cache/cache (fn [_])) (%s.telegram/restart)"
    my=bot-ns my=bot-ns)))

(defun my=web-restart ()
  (interactive)
  (my=repl-insert-cmd (format "(%s.web.core/webapp-restart)" my=bot-ns)))

(defun my=show-pic ()
  (interactive)
  (let* ((case ":a")
         (prm (format
               "{:day %s :threshold %s :threshold-increase %s :case %s :stats %s}"
               (format "(count (%s.api.expdev07/raw-dates))" my=bot-ns)
               my=bot-ns
               (format "(%s.common/min-threshold %s)" my=bot-ns case)
               (format "(%s.common/threshold-increase %s)" my=bot-ns case)
               case
               (format "(%s.api.v1/pic-data)" my=bot-ns))))
    (my=repl-insert-cmd
     (format "(cljplot.core/show (%s.plot/calc-aggregation-img %s))"
             my=bot-ns prm))))

(defun my=show-pic-for-pred ()
  (interactive)
  (my=repl-insert-cmd
   (format
    "(cljplot.core/show (%s.plot/calc-plot-country-img \"ZZ\" %s.plot/stats %s.plot/report))"
    my=bot-ns my=bot-ns my=bot-ns my=bot-ns)))

(defun my=stop-synths-metronoms ()
  (interactive)
  (my=repl-insert-cmd "(stop)")
  (cider-repl-return))

(defun my=magit-status ()
  (interactive)
  (my=save-all-buffers)
  (magit-status))

(defun my=cider-insert-and-format (form)
  (interactive)
  (my=repl-insert-cmd (concat (mapconcat 'identity form "\n")))
  (evil-normal-state)
  (evil-jump-item)
  (dolist (line (cdr form))
    (evil-next-visual-line)
    (cider-repl-tab))
  (evil-append-line 0))

(defun my=cider-unmap-this-ns ()
  (interactive)
  (my=cider-insert-and-format
   `(
     ;; "(map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))"
     "(->> [*ns*]"
     "     (map (fn [nspace]"
     "              (->> (keys (ns-interns nspace))"
     "                   (map (fn [symb] (ns-unmap nspace symb)))))))"
     )))

(defun my=cider-browse-this-ns ()
  (interactive)
  (my=cider-insert-and-format
   `(
     "(->> [*ns*]"
     "     (map (fn [nspace]"
     "              (->> (keys (ns-interns nspace))"
     "                   ))))"
     )))

(defun my=cider-browse-all-ns (namespace)
  "E.g.:
(my=cider-browse-all-ns \"corona\")

Evil substitute / replace command:
  \\='<,\\='>s/\(.*\)/\"\\1\"/
  "
  (interactive)
  (let* ((nspace (list (concat "\"" namespace "\""))))
    (my=cider-insert-and-format
     `(
       "(let [ns-prefix " ,@nspace "]"
       "  (->> (all-ns)"
       "       (filter (fn [nspace] (.startsWith (str nspace) ns-prefix)))"
       "       #_(take 1)"
       "       (map (fn [nspace]"
       "                (assoc {} nspace"
       "                (->> (ns-interns nspace)"
       "                     (keys)"
       "                     #_(map (fn [symb] (ns-unmap nspace symb)))))))))"
       ))))

(defun my=cider-browse-all-ns-corona ()
  (interactive)
  (my=cider-browse-all-ns "corona"))

(defun my=cider-unmap-all-ns (namespace)
  "Substitute / replace:
\\='<,\\='>s/\(.*\)/\"\\1\"/
"
  (interactive)
  (let* ((nspace (list (concat "\"" namespace "\""))))
    (my=cider-insert-and-format
     `(
       "(let [ns-prefix " ,@nspace "]"
       "  (->> (all-ns)"
       "       (filter (fn [nspace] (.startsWith (str nspace) ns-prefix)))"
       "       #_(take 1)"
       "       (map (fn [nspace]"
       "                (->> (ns-interns nspace)"
       "                     (keys)"
       "                     (map (fn [symb] (ns-unmap nspace symb))))))))"
       ))))

(defun my=cider-unmap-all-ns-corona ()
  (interactive)
  (my=cider-unmap-all-ns "corona"))

(defun my=save-all-buffers ()
  "Thanks to https://stackoverflow.com/a/30468232"
  (interactive)
  (save-some-buffers
   'no-confirm
   (lambda ()
     (cond
      ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
      ((and buffer-file-name (eq major-mode 'clojure-mode)))
      ((and buffer-file-name (eq major-mode 'latex-mode)))
      ((and buffer-file-name (eq major-mode 'markdown-mode)))
      ((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
      ((and buffer-file-name (derived-mode-p 'org-mode)))))))

(defun my=buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer.
Thanks to https://stackoverflow.com/a/2238589"
  (with-current-buffer buffer-or-string
    major-mode))

(defun all-major-mode-variants (symb-name)
  (let (
        ;; The `symbol-name' returns a string. Convert it to symbol
        (s-sym (intern symb-name))
        )
    (if (get s-sym 'derived-mode-parent) s-sym)))

;; TODO have a look at the `fundamental-mode'
;; (setq last-edit-tracked-modes-list
;;       (append '(text-mode prog-mode)
;;               (remove nil
;;                       (mapcar (lambda (mode)
;;                                 (if (provided-mode-derived-p
;;                                      mode 'prog-mode 'text-mode)
;;                                     mode))
;;                               (remove nil
;;                                       (mapcar 'all-major-mode-variants
;;                                               (loop for x being the symbols
;;                                                     if (fboundp x)
;;                                                     collect (symbol-name x))))))))

;; for all derivatives of 'prog-mode 'text-mode :
;; https://emacs.stackexchange.com/questions/21406/find-all-modes-derived-from-a-mode

;; 1. list all symbols
;; 2. check that a given symbol a mode-symbol

;; add the 'my=save-last-edited-buffer to the hooks of the given mode

;; (dolist (mode (buffer-list))
;;   (message "%s; relevant %s"
;;            mode
;;            (if (provided-mode-derived-p (my=buffer-major-mode mode) 'prog-mode 'text-mode)
;;                ;; (add-hook 'after-change-functions 'feng-buffer-change-hook)
;;                (add-hook (get-hook (my=buffer-major-mode mode))
;;                          'my=save-last-edited-buffer))))

;; https://github.com/bbatsov/projectile/issues/442#issuecomment-59659969
;; (require 'dash)
(defun set-local-keymap (&rest bindings)
  "For project-specific keybindings"
  (dolist (binding (-partition-in-steps 2 2 bindings))
    (lexical-let* ((key (car binding))
                   (cmd (cadr binding))
                   (is-interactive (interactive-form cmd))
                   (local-map (or (current-local-map) (make-keymap))))
      (define-key local-map key
        (lambda ()
          (interactive)
          (if is-interactive
              (call-interactively cmd)
            (eval cmd)))))))

;; From https://www.emacswiki.org/emacs/DiredOmitMode
(defun my=dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun my=spacemacs-light--highlight-current-line ()
  "Highlight current line of the `spacemacs-light' theme.
Thanks to:
https://stackoverflow.com/a/19555234
https://github.com/emacsorphanage/helm-themes/issues/5#issue-210637069"
  (when (find spacemacs--cur-theme
              '(spacemacs-light
                heroku))
    (global-hl-line-mode 1)
    (set-face-background 'hl-line
                         (case spacemacs--cur-theme
                           ('spacemacs-light "#dacecb")
                           ('heroku "black")))
    ;; keep syntax highlighting in the current line:
    (set-face-foreground 'highlight nil)))
