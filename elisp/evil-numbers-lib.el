(use-package evil-numbers
  :bind (((kbd "C-c +")  . evil-numbers/inc-at-pt)
         ((kbd "C-c -")  . evil-numbers/dec-at-pt)

         ((kbd "s-+")  . evil-numbers/inc-at-pt)
         ((kbd "s--")  . evil-numbers/dec-at-pt)

         ((kbd "<C-kp-add>")       . evil-numbers/inc-at-pt)
         ((kbd "<C-kp-subtract>")  . evil-numbers/dec-at-pt)
         ((kbd "<s-kp-add>")       . evil-numbers/inc-at-pt)
         ((kbd "<s-kp-subtract>")  . evil-numbers/dec-at-pt)))
