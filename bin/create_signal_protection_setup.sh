#!/usr/bin/env bash
# -*- mode: sh; sh-shell: bash -*-

## bash -n create_signal_protection_setup.sh
## shellcheck create_signal_protection_setup.sh

# SYNOPSIS:
#   create_signal_protection_setup.sh
#
# DESCRIPTION:
#   Sets up a lightweight idle-protection wrapper for Signal Desktop.
#
#   The script prompts for a protection password, encrypts it with GPG, and
#   stores the encrypted password under the user's configuration directory.
#   It then generates an executable helper script in ~/.local/bin and creates
#   a desktop autostart entry that starts the helper script automatically on
#   login.
#
#   The generated helper starts an xautolock watcher. After the configured idle
#   timeout, Signal Desktop is minimized and reactivation is guarded by a Zenity
#   password prompt. The helper can also be invoked manually to protect or kill
#   Signal Desktop.
#
# REQUIREMENTS:
#   gpg, xdotool, zenity, xautolock, pkill
#
# GUIX:
#   guix install gnupg xdotool zenity xautolock procps
#
# CREATED FILES:
#   ~/.config/signal-protect/.signal-protect-password.gpg
#   ~/.local/bin/protect-signal-desktop.sh
#   ~/.config/autostart/xautolock-signal.desktop
#
# NOTE:
#   This is not a true application-level lock for Signal Desktop. It minimizes
#   the Signal window and gates reactivation through the generated helper
#   script, but it cannot prevent access through all possible desktop/session
#   mechanisms.
#
# Test with
# $ shellcheck create_signal_protection_setup.sh
#
# -n Read commands but do not execute them. This may be used to check a shell
#    script for syntax errors. This is ignored by interactive shells.
# $ bash -n create_signal_protection_setup.sh

set -euo pipefail

# guix install gnupg xdotool zenity xautolock procps
for cmd in gpg xdotool zenity xautolock pkill; do
    if ! command -v "$cmd" > /dev/null 2>&1; then
        printf '[ERROR] Missing required command: %s\n' "$cmd" >&2
        exit 1
    fi
done

# Ensure that files created by this script are readable only by the user
umask 077

APP_PROTECT_DIR="$HOME/.config/signal-protect"
BIN_DIR="$HOME/.local/bin"
AUTOSTART_DIR="$HOME/.config/autostart"

mkdir --parents "$APP_PROTECT_DIR" "$BIN_DIR" "$AUTOSTART_DIR"

SECRET_FILE="$APP_PROTECT_DIR/.signal-protect-password.gpg"
# Adjust SYNOPSIS and start_watcher function when changing PROTECT_APP_SCRIPT
PROTECT_APP_SCRIPT="$BIN_DIR/protect-signal-desktop.sh"
DESKTOP_AUTOSTART_FILE="$AUTOSTART_DIR/xautolock-signal.desktop"

# -p prompt	output the string PROMPT
# -r	do not allow backslashes to escape any characters
# -s	do not echo input coming from a terminal
read -r -s -p "Enter protection password for Signal Desktop: " password
printf '\n'

if [[ -z "$password" ]]; then
    printf '[ERROR] Empty password is not allowed\n' >&2
    exit 1
fi

# --yes  Assume "yes" on most questions
printf '%s\n' "$password" \
    | gpg \
          --yes \
          --encrypt \
          --recipient 'Rostislav Svoboda' \
          --output "$SECRET_FILE"

unset password

escape_for_double_quotes() {
    local string

    string="$1"
    string="${string//\\/\\\\}"
    string="${string//\"/\\\"}"
    string="${string//\$/\\\$}"
    string="${string//\`/\\\`}"

    printf '%s' "$string"
}

# Quoted heredocs delimiters 'EOF_...' prevent setup-time variable expansion.
# Unquoted delimiters expand variables, that however may require '\$'.
# Pay attention! `cat > ...` overwrites the file.
cat > "$PROTECT_APP_SCRIPT" <<'EOF_PROTECT_APP_SCRIPT1'
#!/usr/bin/env bash
# SYNOPSIS:
#   protect-signal-desktop.sh [--protect|--kill]
#
# DESCRIPTION:
#   Starts an xautolock watcher for Signal Desktop when run without arguments.
#   After the configured idle timeout, Signal is minimized and reactivation is
#   guarded by a Zenity password prompt. With --protect, the protection action
#   is run once. With --kill, running Signal Desktop processes are terminated.
#
# NOTE:
#   This is not a true application-level lock. It only minimizes the Signal
#   window and gates reactivation through this script.

set -euo pipefail

LOCK_TIMEOUT_MINUTES=1
EOF_PROTECT_APP_SCRIPT1

# Inject the setup-time secret path into the generated script. (Variables in
# quoted heredocs can't be expanded.)
printf 'SECRET_FILE="%s"\n\n' \
       "$(escape_for_double_quotes "$SECRET_FILE")" \
       >> "$PROTECT_APP_SCRIPT"

# Pay attention! `cat >> ...` appends to the file.
cat >> "$PROTECT_APP_SCRIPT" <<'EOF_PROTECT_APP_SCRIPT2'
PRM_PROTECT="--protect"
PRM_KILL="--kill"

get_correct_password() {
    gpg --quiet --for-your-eyes-only --decrypt "$SECRET_FILE" 2> /dev/null
}

protect_app() {
    local win_id
    local correct_pw
    local password

    win_id=$(xdotool search --class "Signal" | head --lines=1 || true)

    if [[ -z "$win_id" ]]; then
        return 0
    fi

    xdotool windowminimize "$win_id"
    printf '[INFO] Signal window minimized.\n'

    correct_pw=$(get_correct_password)

    while true; do
        if ! password=$(zenity --password --title="Unlock Signal"); then
            return 0
        fi

        if [[ "$password" == "$correct_pw" ]]; then
            xdotool windowactivate "$win_id"
            printf '[INFO] Signal unlocked.\n'
            break
        fi

        zenity --error --text="Incorrect password"
    done
}

kill_app() {
    pkill --exact --signal TERM signal-desktop
}

start_watcher() {
    local action
    local locker_command

    action="$PRM_PROTECT"

    # Prevent starting multiple xautolock instances.
    pkill --full --signal TERM "xautolock .*protect-signal-desktop.sh" || true

    printf -v locker_command \
        'bash -c %q bash %q %q' \
        'exec "$1" "$2"' \
        "$0" \
        "$action"

    xautolock \
        -time "$LOCK_TIMEOUT_MINUTES" \
        -detectsleep \
        -locker "$locker_command" &
}

case "${1:-}" in
    "")             start_watcher ;;
    "$PRM_PROTECT") protect_app ;;
    "$PRM_KILL")    kill_app ;;
    *)
        printf '[ERROR] %s: unknown parameter: %s. Expected one of: %s %s\n' \
            "$0" "${1:-}" "$PRM_PROTECT" "$PRM_KILL" >&2
        exit 1 ;;
esac
EOF_PROTECT_APP_SCRIPT2

# -n Read commands but do not execute them. This may be used to check a shell
#    script for syntax errors. This is ignored by interactive shells.
bash -n "$PROTECT_APP_SCRIPT"
chmod +x "$PROTECT_APP_SCRIPT"
printf '%s created\n' "$PROTECT_APP_SCRIPT"

cat > "$DESKTOP_AUTOSTART_FILE" <<EOF_CREATE_AUTOSTART
[Desktop Entry]
Type=Application
Exec=$PROTECT_APP_SCRIPT
Hidden=false
X-GNOME-Autostart-enabled=true
Name=Lock Signal
Comment=Protect Signal after 1 minute of idle time
EOF_CREATE_AUTOSTART

printf '%s created\n' "$DESKTOP_AUTOSTART_FILE"
printf 'Done\n'
