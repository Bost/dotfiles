# -*- mode: fish -*-

## fish -n filename.fish
## fish_indent --check filename.fish

function filename --description "Get filename w/o path and extension"
    # see https://stackoverflow.com/a/965072
    set escArgv (string escape -- $argv)
    set rootname (echo $escArgv | sed 's/\.[^.]*$//')
    echo $rootname
end
