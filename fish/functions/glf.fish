function glf
    # i.e. git pull --origin $argv; and git fetch --tags
    # where:
    #      git fetch --tags
    # Fetch all tags from the remote (i.e., fetch remote tags refs/tags/* into
    # local tags with the same name), in addition to whatever else would
    # otherwise be fetched.
    set remote franzi
    set cmd "git fetch --tags $remote $argv; and git rebase $argv"
    echo $cmd
    eval $cmd
end