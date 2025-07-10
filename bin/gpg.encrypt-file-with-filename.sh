#!/bin/sh

check_file () {
    if [ -z "$1" ]; then
        printf "File not found: %s\n" "$1"
        exit 1
    else
        :
        # printf "File found: %s\n" "$1"
    fi
}

original_file="$1"
check_file "$original_file"

# Generate a random name for the encrypted file
random_name=$(mktemp XXXXXXXXXX)
fname="${random_name}.filename.gpg"
fcontent="${random_name}.content_.gpg"

# -c --symmetric
# -o --output
echo "$original_file" | \
    gpg -c --cipher-algo AES256 -o "$fname" && \
    gpg -c --cipher-algo AES256 -o "$fcontent" "$original_file" && \
    rm "${random_name}" "$original_file" # Clean up
