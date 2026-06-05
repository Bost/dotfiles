# -*- mode: fish -*-

## fish -n tf.fish
## fish_indent --check tf.fish

function tf --description "Follow a file as it grows (tail -f)"
    trace tail --follow $argv
end
