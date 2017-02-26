function l
  # Parameter expansion (Globbing) causes trouble
  # set cmd "ls -lA  --color --time-style='+%d-%m-%Y %H:%M:%S' $argv"
  # echo $cmd
  # eval $cmd
  echo "ls -lA  --color --time-style='+%d-%m-%Y %H:%M:%S' $argv"
        ls -lA  --color --time-style='+%d-%m-%Y %H:%M:%S' $argv
end
