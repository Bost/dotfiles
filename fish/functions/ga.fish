function ga
  # ; and git fetch --tags
  echo "cd $dev/cheatsheet; and git pull --rebase origin $argv"
        cd $dev/cheatsheet; and git pull --rebase origin $argv

  # ; and git fetch --tags
  echo "cd $dev/dotfiles; and git pull --rebase origin $argv"
        cd $dev/dotfiles; and git pull --rebase origin $argv
end
