;;; zark.el --- Alternative string displayment. -*- lexical-binding: t -*-
;;; Code:

;; (setq zark-pattern nil)
(defgroup zark nil "Settings for alternative string displayment")

;; "Pattern for identifying a keyword (must contain 1 capturing group)"
(defcustom zark-pattern1 "\\(\\<Move\\>\\)"       "" :type 'regexp :group 'zark)
(defcustom zark-mask1    "░░░░"                   "" :type 'string :group 'zark)
(defcustom zark-pattern2 "\\(\\<PlayerName\\>\\)" "" :type 'regexp :group 'zark)
(defcustom zark-mask2    "▓▓▓▓"                   "" :type 'string :group 'zark)
(defcustom zark-pattern3 "\\(\\<PlayerMove\\>\\)" "" :type 'regexp :group 'zark)
(defcustom zark-mask3    "████"                   "" :type 'string :group 'zark)

(defun zark-font-lock-keywords ()
  `(
    (,zark-pattern1 1 (zark-render1))
    (,zark-pattern2 1 (zark-render2))
    (,zark-pattern3 1 (zark-render3))
    ))

(defun zark-render1 () `(face font-lock-doc-face display ,zark-mask1))
(defun zark-render2 () `(face font-lock-doc-face display ,zark-mask2))
(defun zark-render3 () `(face font-lock-doc-face display ,zark-mask3))

(defun zark-turn-on ()
  "Turn on zark-mode."
  (interactive)
  (let ((props (make-local-variable 'font-lock-extra-managed-props)))
    (add-to-list props 'display))
  (font-lock-add-keywords nil (zark-font-lock-keywords)))

(defun zark-turn-off ()
  "Turn off zark-mode."
  (interactive)
  ;; (insert " ")
  (font-lock-remove-keywords nil (zark-font-lock-keywords)))

(setq zarking 1)

(defun zarking-str (zarking-val)
  (concat ":zarking-new-val " (number-to-string zarking-val) " "
          ":timestamp \"" (timestamp) "\""))

(defun zark-toggle ()
  (if (eq 1 zarking)
      (zark-turn-on)
    (zark-turn-off))
  (setq zarking (* -1 zarking))
  (font-lock-flush)
  (message (zarking-str zarking)))

(defun zark-symbols ()
  (interactive)
  (save-buffer)
  (switch-to-buffer "rps_async.clj")
  ;; (push '("t/defalias " . ?​) prettify-symbols-alist); 'ZERO WIDTH SPACE' (U+200B)
  ;; ⊢⊣⊤⊥
  ;; ⏩⏪⏫⏬
  ;; ❏❐❑❒
  ;; ⬖⬗⬘⬙
  ;; ☗⛊⛔🟆⚑
  ;; ◰ ◱ ◲ ◳
  ;; ▲▶▼◀
  ;; ◐ ◑ ◒ ◓
  ;; 🢀 🢁 🢂 🢃 🢄 🢅 🢆 🢇 🠈🠉🠊🠋
  ;; 1F650🙐🙑🙒🙓🙔🙕🙖🙗🙨🙩🙪🙫🙬🙭🙮🙯1F670🙰🙱🙲🙳🙴🙵🙶🙷🙸🙹🙺🙻🙼🙽

  (zark-toggle)
  (switch-to-buffer "zark.el"))

(provide 'zark)
