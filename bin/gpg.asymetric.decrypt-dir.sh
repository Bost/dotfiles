#!/bin/sh

# Configuration
IN_FILE="/tmp/secure_data.tar.gz.gpg"  # Encrypted input file
TAR_FILE="/tmp/secure_data.tar.gz"     # Decrypted archive
OUT_DIR="/tmp/decrypted_data"          # Directory to extract archive into

# Check input file
if [ ! -f "$IN_FILE" ]; then
    printf "[ERR] Encrypted file not found: %s\n" "$IN_FILE"
    exit 1
fi

# Decrypt the archive
printf "[INF] Decrypting file: %s\n" "$IN_FILE"
gpg --decrypt \
    --output "$TAR_FILE" \
    "$IN_FILE" || {
        printf "[ERR] GPG decryption failed\n"
        exit 1
    }

# Create output directory if needed
if [ ! -d "$OUT_DIR" ]; then
    mkdir -p "$OUT_DIR" || {
        printf "[ERR] Failed to create output directory: %s\n" "$OUT_DIR"
        exit 1
    }
fi

# Extract the archive
printf "[INF] Extracting archive to: %s\n" "$OUT_DIR"
tar --extract \
    --gzip \
    --file="$TAR_FILE" \
    --directory="$OUT_DIR" || {
        printf "[ERR] Failed to extract archive\n"
        rm -f "$TAR_FILE"
        exit 1
    }

# Clean up
rm -f "$TAR_FILE"
printf "[INF] Decryption and extraction complete: %s\n" "$OUT_DIR"
