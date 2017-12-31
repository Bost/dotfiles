function synaptic
  set cmd "gksudo synaptic $argv"
  echo $cmd
  eval $cmd
end
