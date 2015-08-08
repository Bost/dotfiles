function lx
    ls -la $(find . -maxdepth 1 -perm -111 -type f) $argv
end