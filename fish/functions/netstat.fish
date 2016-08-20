function netstat
    echo "# 'netstat' is obsolete. Using: 'ss' - socket statistics"
    echo ss $argv
         ss $argv
end
