#!/bin/sh

random_name="$1"

check_file () {
    if [ -z "$1" ]; then
        printf "File not found: %s\n" "$1"
        exit 1
    else
        :
        # printf "File found: %s\n" "$1"
    fi
}

fname="${random_name}.filename.gpg"
fcontent="${random_name}.content_.gpg"

check_file "$fname"
check_file "$fcontent"

gpg --decrypt "$fname"    > decrypted_filename.txt
gpg --decrypt "$fcontent" > "$(cat decrypted_filename.txt)"

rm decrypted_filename.txt
