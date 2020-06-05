function extract
  # TODO see climate extract

  # This alias does not loop over args
  # set argv_rest $argv[2..(count $argv)] # argv starts indexing with 1

  # set file $argv[1]
  set file (string escape -- $argv[1])
  switch $file
    case "*.tar.bz2"
      set cmd tar xjf $file
    case "*.tar.gz"
      set cmd tar xzf $file
      if test $status -eq 2
        echo "WARN: The archive is propably is a tar, not a GZip. Trying alterternative:"
        set cmd tar xvf $file
      end
    case "*.bz2"
      set cmd bunzip2 $file
    case "*.rar"
      set cmd unrar e $file
    case "*.gz"
      set cmd gunzip $file
    case "*.tar"
      set cmd tar xf $file
    case "*.tbz2"
      set cmd tar xjf $file
    case "*.tgz"
      set cmd tar xzf $file
    case "*.jar"
      set cmd unzip $file
    case "*.war"
      set cmd unzip $file
    case "*.zip"
      set cmd unzip $file
    case "*.Z"
      set cmd uncompress $file
    case "*.7z"
      set cmd 7z x $file
    case "*.tar.xz"             # LZMA/LZMA2 algorithms
      set cmd tar xvfJ $file
    case "*.pax"
      set cmd pax -r < $file
    case "*.dmg"                # Apple Mac image
        # set cmd dmg2img $file "$file.img"
        set cmd dmg2img $file
        # set cmd 7z x $file
    case "\"*\""
       echo "ERROR: double quotes \"...\" shouldn't match the file extension"
    case "'*'"
      echo "ERROR: single quotes '...' shouldn't match the file extension"
    case "*"
      echo "ERROR: Unknown file extension:" $file
  end
  # check if it's set/empty - https://stackoverflow.com/a/47743269
  if test -n "$cmd"
      echo $cmd
      eval $cmd
  end
end
