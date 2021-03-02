function spag --description="Update spacemacs"
  set cmd cd ~/.emacs.d/
  echo $cmd
  eval $cmd

  set cmd git checkout develop
  echo $cmd
  eval $cmd

  set cmd git fetch --tags origin
  echo $cmd
  eval $cmd

  set cmd git rebase
  echo $cmd
  eval $cmd

  set cmd git rebase develop cycle
  echo $cmd
  eval $cmd
end
