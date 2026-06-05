# -*- mode: fish -*-

## fish -n png.fish
## fish_indent --check png.fish

function png --description "Ping/route-trace to google.com (mtr)"
    # trace mtr 104.20.23.154  $argv # example.com
    # trace mtr 9.9.9.9        $argv # Quad9 DNS
    # trace mtr 8.8.8.8        $argv # Google DNS
    # trace mtr 208.67.222.222 $argv # OpenDNS
    trace mtr 1.1.1.1        $argv  # Cloudflare DNS
end
