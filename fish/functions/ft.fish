function ft -d "findTextInAnyFile"
    set cmd "find . -name \""$argv[2]"\" -exec grep -il \""$argv[1]"\" '{}' \;"
    # alternative implementation
    # set cmd "find . -name \""$argv[2]"\" | xargs grep -l \""$argv[1]"\""
    echo "cmd: $cmd"
    # eval $cmd
end
