function filename
    # see https://stackoverflow.com/a/965072
    set escArgv (string escape -- $argv)
    set rootname (echo $escArgv | sed 's/\.[^.]*$//')
    echo $rootname
end
