#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd)"
HOME_DIR="$HOME"

# Mapping: repo_relative_path -> system_absolute_path
declare -a MAPPINGS=(
    "git/.gitconfig:${HOME_DIR}/.gitconfig"
    "git/.gitconfig-cartrack:${HOME_DIR}/.gitconfig-cartrack"
    "git/.gitconfig-github:${HOME_DIR}/.gitconfig-github"
    "nvim/init.vim:${HOME_DIR}/.config/nvim/init.vim"
    "tmux/.tmux.conf:${HOME_DIR}/.tmux.conf"
    "zsh/.zshrc:${HOME_DIR}/.zshrc"
    "c++/.clang-format:${HOME_DIR}/.clang-format"
    "c++/.clang-tidy:${HOME_DIR}/.clang-tidy"
    "rust/.ctags:${HOME_DIR}/.ctags"
    "bat/config:${HOME_DIR}/.config/bat/config"
)

DRY_RUN=false
VERBOSE=false
DIRECTION="" # "", "push", or "pull"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Sync config files between this repo and the system.

Options:
    --push       Sync repo -> system only
    --pull       Sync system -> repo only
    -n, --dry-run  Show what would be done without making changes
    -v, --verbose  Show detailed output
    -h, --help     Show this help message

Without --push or --pull, syncs bidirectionally (newer file wins).
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --push)    DIRECTION="push"; shift ;;
        --pull)    DIRECTION="pull"; shift ;;
        -n|--dry-run) DRY_RUN=true; shift ;;
        -v|--verbose) VERBOSE=true; shift ;;
        -h|--help) usage ;;
        *) echo "Unknown option: $1"; usage ;;
    esac
done

log() {
    if $VERBOSE; then
        echo "  $*"
    fi
}

sync_file() {
    local repo_file="$1"
    local sys_file="$2"

    local repo_exists=false
    local sys_exists=false
    [[ -f "$repo_file" ]] && repo_exists=true
    [[ -f "$sys_file" ]] && sys_exists=true

    if ! $repo_exists && ! $sys_exists; then
        log "SKIP (neither exists): $repo_file"
        return
    fi

    # Determine sync direction
    local action="" # "to_sys", "to_repo", or ""
    if [[ "$DIRECTION" == "push" ]]; then
        if $repo_exists; then
            action="to_sys"
        else
            log "SKIP (not in repo): $repo_file"
            return
        fi
    elif [[ "$DIRECTION" == "pull" ]]; then
        if $sys_exists; then
            action="to_repo"
        else
            log "SKIP (not on system): $sys_file"
            return
        fi
    else
        # Bidirectional: newer wins
        if $repo_exists && ! $sys_exists; then
            action="to_sys"
        elif ! $repo_exists && $sys_exists; then
            action="to_repo"
        elif $repo_exists && $sys_exists; then
            if diff -q "$repo_file" "$sys_file" > /dev/null 2>&1; then
                log "OK (in sync): $repo_file"
                return
            fi
            local repo_mtime sys_mtime
            repo_mtime=$(stat -f %m "$repo_file" 2>/dev/null || stat -c %Y "$repo_file")
            sys_mtime=$(stat -f %m "$sys_file" 2>/dev/null || stat -c %Y "$sys_file")
            if [[ "$repo_mtime" -ge "$sys_mtime" ]]; then
                action="to_sys"
            else
                action="to_repo"
            fi
        fi
    fi

    local repo_rel="${repo_file#"$REPO_DIR"/}"
    if [[ "$action" == "to_sys" ]]; then
        echo "-> $repo_rel  =>  $sys_file"
        if ! $DRY_RUN; then
            mkdir -p "$(dirname "$sys_file")"
            cp "$repo_file" "$sys_file"
        fi
    elif [[ "$action" == "to_repo" ]]; then
        echo "<- $sys_file  =>  $repo_rel"
        if ! $DRY_RUN; then
            mkdir -p "$(dirname "$repo_file")"
            cp "$sys_file" "$repo_file"
        fi
    fi
}

if $DRY_RUN; then
    echo "=== DRY RUN ==="
fi

if [[ -n "$DIRECTION" ]]; then
    echo "Direction: $DIRECTION"
else
    echo "Direction: bidirectional (newer wins)"
fi
echo ""

synced=0
for mapping in "${MAPPINGS[@]}"; do
    repo_rel="${mapping%%:*}"
    sys_path="${mapping#*:}"
    repo_path="${REPO_DIR}/${repo_rel}"

    sync_file "$repo_path" "$sys_path"
    synced=$((synced + 1))
done

echo ""
echo "Checked $synced config mappings."
if $DRY_RUN; then
    echo "(dry run — no files were modified)"
fi
