# -*- mode: fish -*-

## fish -n png.fish
## fish_indent --check png.fish

function png --description "mtr google.com …"
    set cmd mtr 1.1.1.1        (string escape -- $argv)  # Cloudflare DNS
    # set cmd mtr 104.20.23.154  (string escape -- $argv  # example.com
    # set cmd mtr 9.9.9.9        (string escape -- $argv  # Quad9 DNS
    # set cmd mtr 8.8.8.8        (string escape -- $argv  # Google DNS
    # set cmd mtr 208.67.222.222 (string escape -- $argv  # OpenDNS
    echo $cmd
    eval $cmd
end
