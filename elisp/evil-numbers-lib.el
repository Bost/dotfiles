(use-package evil-numbers
  :bind (((kbd "C-c +")  . evil-numbers/inc-at-pt)
         ((kbd "C-c -")  . evil-numbers/dec-at-pt)

         ((kbd "s-+")  . evil-numbers/inc-at-pt)
         ((kbd "s--")  . evil-numbers/dec-at-pt)

         ((kbd "<C-kp-add>")       . evil-numbers/inc-at-pt)
         ((kbd "<C-kp-subtract>")  . evil-numbers/dec-at-pt)
         ((kbd "<s-kp-add>")       . evil-numbers/inc-at-pt)
         ((kbd "<s-kp-subtract>")  . evil-numbers/dec-at-pt)
         ;; or only in evil’s normal state:
         ;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
         ;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
         ))
