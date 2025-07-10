#!/bin/sh

# Encrypt a directory using GPG (archived and encrypted)
# Usage: gpg.passphrase.encrypt-dir.sh /path/to/source_dir /path/to/output_file.gpg

SRC_DIR="$1"
OUT_FILE="$2"

if [ -z "$SRC_DIR" ] || [ -z "$OUT_FILE" ]; then
    echo "Usage: $0 <source_dir> <output_file.gpg>"
    exit 1
fi

if [ ! -d "$SRC_DIR" ]; then
    echo "[ERR] Source directory does not exist: $SRC_DIR"
    exit 1
fi

TAR_FILE="$(basename "$SRC_DIR").tar.gz"

echo "[INF] Archiving and compressing..."
tar -czf "$TAR_FILE" -C "$(dirname "$SRC_DIR")" "$(basename "$SRC_DIR")"

echo "[INF] Encrypting with GPG..."
gpg -c --output "$OUT_FILE" "$TAR_FILE"

echo "[INF] Cleaning up..."
rm "$TAR_FILE"

echo "[INF] Encrypted archive saved as: $OUT_FILE"
