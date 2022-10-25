function hoch
  set cmd cd $dev/hochzeit (string escape -- $argv)
  echo $cmd
  eval $cmd
end
