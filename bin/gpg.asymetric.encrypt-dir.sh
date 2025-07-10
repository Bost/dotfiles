#!/bin/sh

# Configuration
ARCHIVE_NAME="secure_archive.tar.gz"
ENCRYPTED_ARCHIVE="${ARCHIVE_NAME}.gpg"
INPUT_DIR="/tmp/texts"         # Directory to encrypt
OUTPUT_DIR="/tmp/encrypted"    # Where to store the .gpg file
RECIPIENT="user@example.com"   # Replace with your actual GPG key ID or email

mkdir -p "${OUTPUT_DIR}"

# Step 1: Create the archive
tar -czf "${OUTPUT_DIR}/${ARCHIVE_NAME}" -C "$(dirname "${INPUT_DIR}")" "$(basename "${INPUT_DIR}")" || {
    echo "[ERR] Failed to create archive"
    exit 1
}

# Step 2: Encrypt the archive with the public GPG key
gpg --yes --encrypt --recipient "${RECIPIENT}" --output "${OUTPUT_DIR}/${ENCRYPTED_ARCHIVE}" "${OUTPUT_DIR}/${ARCHIVE_NAME}" || {
    echo "[ERR] GPG encryption failed"
    rm -f "${OUTPUT_DIR:?}/${ARCHIVE_NAME}"
    exit 2
}

# Optional: Remove the unencrypted archive
rm -f "${OUTPUT_DIR}/${ARCHIVE_NAME}"

echo "[INF] Encrypted archive saved to ${OUTPUT_DIR}/${ENCRYPTED_ARCHIVE}"
exit 0
