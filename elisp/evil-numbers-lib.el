(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(global-set-key (kbd "s-+") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "s--") 'evil-numbers/dec-at-pt)

(global-set-key (kbd "<C-kp-add>")      'evil-numbers/inc-at-pt)
(global-set-key (kbd "<C-kp-subtract>") 'evil-numbers/dec-at-pt)
(global-set-key (kbd "<s-kp-add>")      'evil-numbers/inc-at-pt)
(global-set-key (kbd "<s-kp-subtract>") 'evil-numbers/dec-at-pt)

