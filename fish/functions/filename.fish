function filename
    set escArgv (string escape -- $argv)
    set rootname (echo $escArgv | sed 's/\.[^.]*$//')
    echo $rootname
end
