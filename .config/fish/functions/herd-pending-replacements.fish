# -*- mode: fish -*-

## fish -n herd-pending-replacements.fish
## fish_indent --check herd-pending-replacements.fish

function herd-pending-replacements --description "List services pending restart"
    sudo herd detailed-status \
        | awk '
    /^Status of / {
    service = $3
    sub(/:$/, "", service)
    }
    /Replacement pending \(restart to upgrade\)\./ {
    print service
    }'
end
