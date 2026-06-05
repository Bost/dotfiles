# -*- mode: fish -*-

## fish -n fxml.fish
## fish_indent --check fxml.fish

function fxml --description "Find .xml files (fd --extension xml)"
    trace fd --extension xml $argv
end
