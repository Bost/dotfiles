(make-gaming-config
 (let [(user (getenv "STEAM_GAMING_USER"))
       (token (getenv "STEAM_GAMING_TOKEN"))]
   `((factorio
      ((username ,user)
       (token ,token)))
     (gunpoint
      ((username ,user)
       (token ,token))))))
