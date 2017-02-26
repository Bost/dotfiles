function lff
  echo "ls -lrt -d -1 $PWD/*,. * $argv"
        ls -lrt -d -1 $PWD/*,.* $argv
end
