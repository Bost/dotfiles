function gstr
  # # Search for a string in all revisions of entire git history
  # git rev-list --all | (
  # while read revision; do
  #   #git grep -F 'Your search string' $revision
  #   git grep -F '${1}' $revision
  #   done
  #   )
end
