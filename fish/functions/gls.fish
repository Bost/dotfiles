function gls
  echo "cd ~/.emacs.d/"
        cd ~/.emacs.d/
  echo "git pull --rebase origin $argv"
        git pull --rebase origin $argv
  echo "git fetch --tags"
        git fetch --tags
  echo "gitk --all &"
        gitk --all &
end
