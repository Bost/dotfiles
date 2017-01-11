function hrep
  # grep history in reversed order
  echo "history | tac | grep $argv"
        history | tac | grep $argv
end
