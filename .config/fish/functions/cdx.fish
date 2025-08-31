function cdx --description "Change into a project shortcut directory"
    set -l key $argv[1]

    if test -z "$key"
        echo "Usage: cdx <shortcut>"
        echo "Available: (string join ' ' (cdx --list))"
        return 1
    end

    switch $key
        # case latest
        #     cd "$HOME/.cache/guix/checkouts/$latestRepo"
        case owid
            cd "$dec/owid"
        case trackle
            cd "$dev/trackle"
        case ufo
            cd "$dec/ufo"
        case utils
            cd "$dec/utils"
        case zark
            cd "$dec/zark"
        case '*'
            echo "cdx: unknown shortcut '$key'" >&2
            return 1
    end
end
