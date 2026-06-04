# -*- mode: fish -*-

## fish -n tf.fish
## fish_indent --check tf.fish

function tf --description "tail --follow …"
    set cmd tail --follow (string escape -- $argv)
    echo $cmd
    eval $cmd
end
