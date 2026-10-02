#!/usr/bin/env -S fish --no-config
# Select channel revisions at least DAYS * 24 hours old and rebuild local Guix.
# Requirements: fish, git, guix, jq, python3, date, nproc.
# Usage: fish --no-config guix-age-channels.fish [--days 5] [--no-build] [--offline]
# Optional: --guix-source PATH --channels-file PATH --channels-json PATH
#           --cache PATH --jobs N
# --channels-json takes an exported `guix describe --format=json` channel list.
# --offline uses previously fetched history. It still checks out Guix and writes
# comments; combine with --no-build to avoid invoking Guix shell.

argparse 'h/help' 'days=' 'guix-source=' 'channels-file=' 'channels-json=' 'cache=' 'jobs=' 'no-build' 'offline' -- $argv
or exit 2
if set -q _flag_help
    head -n 9 (status filename)
    exit 0
end
if test (count $argv) -gt 0
    echo 'Unexpected positional arguments.' >&2
    exit 2
end
set -l days 5
set -l source /home/bost/dev/guix
set -l channels_file /home/bost/dev/dotfiles/guix/home/common/home-channels.scm
set -l cache "$HOME/.cache/guix-age-channels"
set -l jobs (nproc)
set -q _flag_days; and set days $_flag_days
set -q _flag_guix_source; and set source $_flag_guix_source
set -q _flag_channels_file; and set channels_file $_flag_channels_file
set -q _flag_cache; and set cache $_flag_cache
set -q _flag_jobs; and set jobs $_flag_jobs
for value in $days $jobs
    if not string match -qr '^[1-9][0-9]*$' -- $value
        echo '--days and --jobs must be positive integers.' >&2
        exit 2
    end
end
test -f "$channels_file"; or exit 1
# Refuse tracked edits before checking out an older Guix tree. Untracked files
# are retained; Git itself refuses checkout if one would be overwritten.
set -l dirty (git -C "$source" status --porcelain --untracked-files=no)
or exit 1
if test -n "$dirty"
    echo "Tracked changes in $source; commit or stash them first." >&2
    exit 1
end
mkdir -p "$cache"; or exit 1
set -l channel_json
if set -q _flag_channels_json
    set channel_json (cat "$_flag_channels_json" | string collect)
else
    set channel_json (guix describe --format=json | string collect)
end
test -n "$channel_json"; or exit 1
printf '%s\n' "$channel_json" | jq -e 'type == "array"' >/dev/null; or exit 1
set -l rows (printf '%s\n' "$channel_json" | jq -r '.[] | select(.name != "guix" and .name != "bstx") | [.name, .url, (.branch // "master")] | @tsv')
or exit 1
set -l cutoff (math (date +%s) - $days \* 86400)
set -l pins
set -l guix_commit
set -l repositories "$source"
set -l refs refs/remotes/upstream/master
set -l names guix
if not set -q _flag_offline
    # Fetch rather than pull: no merge into the current branch is needed.
    git -C "$source" fetch upstream '+refs/heads/master:refs/remotes/upstream/master'; or exit 1
end
for row in $rows
    set -l fields (string split \t -- "$row")
    set -l name $fields[1]
    set -l url $fields[2]
    set -l branch $fields[3]
    if not string match -qr '^[a-zA-Z0-9][a-zA-Z0-9_-]*$' -- "$name"
        echo "Invalid channel name: $name" >&2
        exit 1
    end
    set -l repo "$cache/$name.git"
    if not set -q _flag_offline
        if not test -d "$repo"
            git init --bare "$repo"; or exit 1
        end
        git -C "$repo" fetch -- "$url" "+refs/heads/$branch:refs/heads/selected"; or exit 1
    end
    set -a repositories "$repo"
    set -a refs refs/heads/selected
    set -a names "$name"
end
for index in (seq (count $names))
    # Walk the branch's first-parent history, checking committer timestamps.
    # Unlike --before alone, this does not stop at a clock-skewed commit.
    set -l commit_history (git -C "$repositories[$index]" log --first-parent --format='%H %ct' "$refs[$index]")
    or exit 1
    set -l commit
    for entry in $commit_history
        set -l parts (string split ' ' -- "$entry")
        if test "$parts[2]" -le "$cutoff"
            set commit "$parts[1]"
            break
        end
    end
    if test -z "$commit"
        echo "No commit at least $days days old for $names[$index]." >&2
        exit 1
    end
    set -a pins "$names[$index]=$commit"
    echo "$names[$index]: $commit"
    if test "$names[$index]" = guix
        set guix_commit "$commit"
    end
end
git -C "$source" checkout --detach "$guix_commit"; or exit 1
if not set -q _flag_no_build
    pushd "$source"; or exit 1
    guix shell direnv gnupg help2man git strace glibc-locales sudo --development guix --pure -- make --jobs="$jobs"
    set -l build_status $status
    popd
    test "$build_status" -eq 0; or exit $build_status
end
# Insert into the final invocation, not after its closing parenthesis.
# Replace only our marked comment block on repeated runs.
python3 -c '
import datetime, pathlib, re, sys
path = pathlib.Path(sys.argv[1])
days = int(sys.argv[2])
text = path.read_text()
begin = " ;; BEGIN guix-age-channels"
end = " ;; END guix-age-channels"
text = re.sub(r"^" + re.escape(begin) + r"\n.*?^" + re.escape(end) + r"\n", "", text, flags=re.M | re.S)
calls = list(re.finditer(r"^\(home-channels\s*$", text, re.M))
if len(calls) != 1:
    sys.exit("Expected exactly one final (home-channels invocation")
block = [begin, f" ;; Selected {datetime.datetime.now(datetime.timezone.utc).isoformat()}; at least {days} days old."]
for pin in sys.argv[3:]:
    name, commit = pin.split("=", 1)
    keyword = name + "-commit"
    block.append(f" ;; #:{keyword:<22} \"{commit}\"")
block.append(end)
position = text.find("\n", calls[0].start()) + 1
text = text[:position] + "\n".join(block) + "\n" + text[position:]
path.write_text(text)
' "$channels_file" "$days" $pins
or exit 1
echo "Commented pins written to $channels_file. Guix checkout: $guix_commit (detached HEAD)."
