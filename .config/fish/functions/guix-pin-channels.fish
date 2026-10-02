#!/usr/bin/env fish
# -*- mode: fish -*-

## fish -n guix-pin-channels
## fish_indent --check guix-pin-channels

# Find channel commits which are at least N days old, so that substitutes for
# them are more likely to be available, and record them (commented out) in
# home-channels.scm:
# 1. Update the Guix source checkout, check out the guix commit N days old and
#    rebuild it.
# 2. For every other channel used, except bstx, find a commit N days old.
#
# Usage:
#     guix-pin-channels [--days=N] [--no-build] [--dry-run]

set --local options h/help 'd/days=!_validate_int --min 0' n/no-build \
    D/dry-run
argparse $options -- $argv; or exit 1

if set --query _flag_help
    echo "Usage: "(status basename)" [-d|--days=N] [-n|--no-build] [-D|--dry-run]"
    echo "  -d, --days=N    Use channel commits at least N days old (default 5)"
    echo "  -n, --no-build  Don't rebuild the Guix source checkout"
    echo "  -D, --dry-run   Only print the commits; don't change any files"
    exit 0
end

set --local days 5
set --query _flag_days; and set days $_flag_days

# $dgx and $dtf are normally exported by the shell
set --query dgx; or set --local dgx /home/bost/dev/guix
set --query dtf; or set --local dtf /home/bost/dev/dotfiles
set --local home_channels $dtf/guix/home/common/home-channels.scm
# Treeless clones of the channel repositories; commit dates are all we need
set --local cache_dir $HOME/.cache/guix-pin-channels
# Channels which are not pinned by this script
set --local skipped_channels guix bstx

set --local now (date +%s)
set --local cutoff (math $now - $days \* 86400)
set --local cutoff_iso (date --date=@$cutoff --iso-8601=seconds)

function abort
    echo "Error: $argv" >&2
    exit 1
end

# Print the last commit on the branch made before the cutoff
function commit_before --argument-names repo ref cutoff_iso
    git -C $repo rev-list --max-count=1 --first-parent \
        --before=$cutoff_iso $ref
end

# Print "name<TAB>url<TAB>branch" for every channel of the current guix
function used_channels
    guix describe --profile=$HOME/.config/guix/current --format=json |
        jq --raw-output '.[] | [.name, .url, (.branch // "master")] | @tsv'
end

### 1.1 Guix commit N days old
if test -n "$(git -C $dgx status --porcelain --untracked-files=no)"
    abort "$dgx has uncommitted changes"
end
git -C $dgx fetch upstream; or abort "git fetch upstream failed"
set --local guix_commit (commit_before $dgx upstream/master $cutoff_iso)
test -n "$guix_commit"; or abort "No guix commit before $cutoff_iso"
echo "guix: $guix_commit"

set --local keys guix
set --local commits $guix_commit

### 2. Commits N days old of the other channels used
set --local channels (used_channels)
test -n "$channels"; or abort "Can't list the channels used (guix describe)"
mkdir --parents $cache_dir
for line in $channels
    set --local fields (string split \t -- $line)
    set --local name $fields[1]
    set --local url $fields[2]
    set --local branch $fields[3]
    contains -- $name $skipped_channels; and continue

    set --local repo $cache_dir/$name.git
    if test -d $repo
        git -C $repo remote set-url origin $url
        git -C $repo fetch --quiet origin "+refs/heads/*:refs/heads/*"
    else
        git clone --quiet --bare --filter=tree:0 $url $repo
    end
    or begin
        echo "Warning: $name: fetching $url failed; skipping" >&2
        continue
    end

    set --local commit (commit_before $repo $branch $cutoff_iso)
    if test -z "$commit"
        echo "Warning: $name: no commit on $branch before $cutoff_iso" >&2
        continue
    end
    echo "$name: $commit"
    set --append keys $name
    set --append commits $commit
end

if set --query _flag_dry_run
    exit 0
end

### 1.1 Check out the guix commit and rebuild guix
git -C $dgx switch --detach $guix_commit; or abort "git switch failed"
if not set --query _flag_no_build
    pushd $dgx
    guix shell direnv gnupg help2man git strace glibc-locales sudo \
        --development guix --pure -- make --jobs=(nproc)
    or abort "Building guix failed"
    popd
end

### 1.2, 2.2 Add the commits, commented out, to home-channels.scm
# The block is inserted in front of the first active keyword argument of the
# (home-channels ...) call, i.e. after the previously recorded blocks.
set --local block " ;; "(date --date=@$cutoff "+%-d %B %Y %H:%M:%S")" ($days days old)"
for i in (seq (count $keys))
    set --append block \
        (printf ' ;; %-22s "%s"' "#:$keys[$i]-commit" $commits[$i])
end

set --local tmp (mktemp)
block=(string join \n -- $block | string collect --no-trim-newlines) awk '
    /^\(home-channels$/ { in_call = 1 }
    in_call && !done && /^ #:/ { print ENVIRON["block"]; done = 1 }
    { print }
    END { if (!done) exit 1 }
' $home_channels >$tmp
or begin
    rm $tmp
    abort "No place for the commits found in $home_channels"
end
cat $tmp >$home_channels
rm $tmp
echo "Commits added to $home_channels"
