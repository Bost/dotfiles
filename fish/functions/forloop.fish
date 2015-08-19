function forloop
    set list \
    "foo" \
    "bar" \
    "zee"

    for item in $list
        echo "item: $item"
    end
end
