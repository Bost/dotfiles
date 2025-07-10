#!/bin/sh

# Configuration
SRC_DIR="/path/to/your/data"           # The directory you want to encrypt
TAR_FILE="/tmp/secure_data.tar.gz"     # Temporary archive path
OUT_FILE="/tmp/secure_data.tar.gz.gpg" # Final encrypted file
GPG_RECIPIENT="user@example.com"       # GPG recipient (must be in your keyring)

# Check input directory
if [ ! -d "$SRC_DIR" ]; then
    printf "[ERR] Source directory does not exist: %s\n" "$SRC_DIR"
    exit 1
fi

# Create tar.gz archive
printf "[INF] Archiving directory: %s\n" "$SRC_DIR"
tar --create \
    --gzip \
    --file="$TAR_FILE" \
    --directory="$(dirname "$SRC_DIR")" \
    "$(basename "$SRC_DIR")" || {
        printf "[ERR] Failed to create archive\n"
        exit 1
    }

# Encrypt the archive using recipient's public key
printf "[INF] Encrypting archive for recipient: %s\n" "$GPG_RECIPIENT"
gpg --encrypt \
    --recipient "$GPG_RECIPIENT" \
    --output "$OUT_FILE" \
    "$TAR_FILE" || {
        printf "[ERR] GPG encryption failed\n"
        rm -f "$TAR_FILE"
        exit 1
    }

# Clean up
rm -f "$TAR_FILE"
printf "[INF] Encryption complete: %s\n" "$OUT_FILE"
