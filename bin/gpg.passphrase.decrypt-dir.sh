#!/bin/sh

# Decrypt a GPG-encrypted archive and extract it
# Usage: gpg.passphrase.decrypt-dir.sh /path/to/encrypted_file.gpg /path/to/target_dir

ENC_FILE="$1"
OUT_DIR="$2"

if [ -z "$ENC_FILE" ] || [ -z "$OUT_DIR" ]; then
    echo "Usage: $0 <encrypted_file.gpg> <output_directory>"
    exit 1
fi

if [ ! -f "$ENC_FILE" ]; then
    echo "[ERR] Encrypted file does not exist: $ENC_FILE"
    exit 1
fi

TMP_TAR="decrypted_archive.tar.gz"

echo "[INF] Decrypting..."
gpg --output "$TMP_TAR" -d "$ENC_FILE"

echo "[INF] Extracting to $OUT_DIR..."
mkdir -p "$OUT_DIR"
tar -xzf "$TMP_TAR" -C "$OUT_DIR"

echo "[INF] Cleaning up..."
rm "$TMP_TAR"

echo "[INF] Decryption and extraction completed."
