function glo
    # i.e. git pull --origin $argv; and git fetch --tags
    # where:
    #      git fetch --tags
    # Fetch all tags from the remote (i.e., fetch remote tags refs/tags/* into
    # local tags with the same name), in addition to whatever else would
    # otherwise be fetched.
    set cmd "git fetch --tags origin $argv; and git rebase origin $argv"
    echo $cmd
    eval $cmd
end
