function extract
  # TODO loop for every arg
  set file $argv[1]
  switch $file
    case "*.tar.bz2"
      echo "tar xjf $file"
            tar xjf $file
    case "*.tar.gz"
      echo "tar xzf $file"
            tar xzf $file
      if test $status -eq 2
        echo "WARN: The archive is propably is a tar, not a GZip. Trying alterternative:"
        echo "tar xvf $file"
              tar xvf $file
      end
    case "*.bz2"
      echo "bunzip2 $file"
            bunzip2 $file
    case "*.rar"
      echo "unrar e $file"
            unrar e $file
    case "*.gz"
      echo "gunzip $file"
            gunzip $file
    case "*.tar"
      echo "tar xf $file"
            tar xf $file
    case "*.tbz2"
      echo "tar xjf $file"
            tar xjf $file
    case "*.tgz"
      echo "tar xzf $file"
            tar xzf $file
    case "*.jar"
      echo "unzip $file"
            unzip $file
    case "*.war"
      echo "unzip $file"
            unzip $file
    case "*.zip"
      echo "unzip $file"
            unzip $file
    case "*.Z"
      echo "uncompress $file"
            uncompress $file
    case "*.7z"
      echo "7z x $file"
            7z x $file
    case "*.tar.xz"
      echo "tar xvfJ $file"
            tar xvfJ $file
    case "'*'"
      echo "ERROR: $file cannot be extracted."
  end
end
