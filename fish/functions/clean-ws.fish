function clean-ws
  echo "Cleaning up eclipse workspace"
  echo "rm -rf .metadata/.plugins/org.eclipse.core.resources/.history"
        rm -rf .metadata/.plugins/org.eclipse.core.resources/.history
  echo "rm .metadata/.plugins/org.eclipse.jdt.core/*.index"
        rm .metadata/.plugins/org.eclipse.jdt.core/*.index
  echo "rm .metadata/.plugins/org.eclipse.jdt.core/savedIndexNames.txt"
        rm .metadata/.plugins/org.eclipse.jdt.core/savedIndexNames.txt
end
