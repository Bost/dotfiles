function take
  # string delims needed - '; and'
  set cmd "mkdir -p $argv; and cd $argv"
  echo $cmd
  eval $cmd
end
