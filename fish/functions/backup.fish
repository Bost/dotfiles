function backup
    # set _pwd (pwd)
    # echo "_pwd" $_pwd

    if test $backupDest
        set backupDir $backupDest  # echo "var set to '$var'";
    else
        set backupDir "."  # echo "var unset";
    end

    set dirName (dirname $argv[1])
    set baseName (basename $argv[1])
    set orig $dirName/$baseName # get rid of last '/'
    set dirNameOrig (dirname $orig)
    set baseNameOrig (basename $orig)
    cd $dirNameOrig
    set tstp (eval timestamp)
    set backupCopy $backupDir"/"$baseNameOrig"__"$tstp
    set dirToBackup $baseNameOrig
    # set cmd rsync --archive --verbose $orig $backupCopy
    # set cmd rsync --archive --info=stats2,misc1,flist0 $orig $backupCopy
      set cmd rsync --archive --info=progress2 $orig $backupCopy
    echo $cmd
    eval $cmd

    # cd $_pwd
end

