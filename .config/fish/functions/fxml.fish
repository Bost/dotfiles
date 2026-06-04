# -*- mode: fish -*-

## fish -n fxml.fish
## fish_indent --check fxml.fish

function fxml --description "fd --extension xml …"
    set cmd fd --extension xml (string escape -- $argv)
    echo $cmd
    eval $cmd
end
