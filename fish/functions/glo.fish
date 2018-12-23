function glo
    # i.e. git pull --origin (string escape -- $argv); and git fetch --tags
    # where:
    #      git fetch --tags
    # Fetch all tags from the remote (i.e., fetch remote tags refs/tags/* into
    # local tags with the same name), in addition to whatever else would
    # otherwise be fetched.
    set remote origin
    set cmd git fetch --tags $remote (string escape -- $argv); and git rebase (string escape -- $argv)
    echo $cmd
    eval $cmd
end
