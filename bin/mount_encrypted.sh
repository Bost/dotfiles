#!/bin/sh

# - Use "$VAR" in 99% of cases - it's simpler and safer.
# - Use "${VAR}" when:
# -- Concatenating inside longer expressions (e.g., "prefix_${VAR}_suffix")
# -- For disambiguation
#   echo "${HOME}dir"   # correct
#   echo "$HOMedir"     # incorrect, expands $HOMedir which likely doesn't exist

# Configuration
ENCRYPTED_DIR="/tmp/.encrypted_texts"
MOUNT_POINT="/tmp/texts"  # Must not end with '/'

# Create MOUNT_POINT if needed
if [ ! -d "$MOUNT_POINT" ]; then
    printf "[INF] Creating MOUNT_POINT: %s\n" "$MOUNT_POINT"
    mkdir -p "$MOUNT_POINT" || {
        printf "[ERR] Failed to create MOUNT_POINT\n"
        exit 1
    }
fi

# Initialize encrypted dir if needed
if [ ! -d "$ENCRYPTED_DIR" ]; then
    printf "[INF] Creating encrypted storage: %s\n" "$ENCRYPTED_DIR"
    mkdir -p "$ENCRYPTED_DIR" || {
        printf "[ERR] Failed to create ENCRYPTED_DIR\n"
        exit 1
    }
    set -x                                # Start command tracing
    gocryptfs -init "$ENCRYPTED_DIR"
    { retval="$?"; set +x; } 2>/dev/null  # End command tracing
    [ $retval -ne 0 ] && {
        printf "[ERR] gocryptfs -init failed\n"
        exit $retval
    }
fi

# Check if already mounted
if mount | grep --quiet "on $MOUNT_POINT "; then
    printf "[INF] \"%s\" already mounted\n" "$MOUNT_POINT"
    exit 0
fi

# Prevent mounting if target is non-empty
if [ "$(ls -A "$MOUNT_POINT")" ]; then
    printf "[ERR] Can't mount: MOUNT_POINT not empty: %s\n" "$MOUNT_POINT"
    exit 1
fi

# Mount
printf "Enter password to mount encrypted directory: %s\n" "$ENCRYPTED_DIR"
set -x                                # Start command tracing
gocryptfs "$ENCRYPTED_DIR" "$MOUNT_POINT"
{ retval="$?"; set +x; } 2>/dev/null  # End command tracing
[ $retval -ne 0 ] && {
    exit $retval
}

printf "[INF] Mounted successfully. To unmount: fusermount -u \"%s\"\n" "$MOUNT_POINT"
exit $retval
