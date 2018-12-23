function synaptic
  set cmd gksudo synaptic (string escape -- $argv)
  echo $cmd
  eval $cmd
end
