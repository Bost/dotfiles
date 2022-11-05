#|
;; No real content should get commited to repository!
;; Example
(make-gaming-config
 `((game1-name ((key1-name "<key1-value>")
                ...
                (keyN-name "<key2-value>")))
   ...
   (game1-name ((key1-name "<key1-value>")
                ...
                (keyN-name "<key2-value>")))))

;; or try as cons cells, with dots '.':
(make-gaming-config
 `((game1-name . ((key1-name . "<key1-value>")
                  ...
                  (keyN-name . "<key2-value>")))
   ...
   (game1-name . ((key1-name . "<key1-value>")
                  ...
                  (keyN-name . "<key2-value>")))))
|#
