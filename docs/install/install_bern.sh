#!/usr/bin/env bash
# Bern Language Installer
# Installs Bern from GitHub to /opt/bern and adds it to PATH

set -euo pipefail

# Version of THIS installer script. Compared against the manifest so we can
# tell the user when a newer installer is available.
readonly SCRIPT_VERSION="1.0.0"

# Single source of truth for what to install. Served by GitHub Pages, so a new
# Bern release only needs the manifest bumped - not this script.
readonly MANIFEST_URL="https://bern-lang.github.io/Bern/install/manifest.json"
readonly INSTALLER_URL="https://bern-lang.github.io/Bern/install/install_bern.sh"

readonly INSTALL_DIR="/opt/bern"

# Defaults, overridden by the manifest when reachable (see resolve_manifest).
REPO="bern-lang/Bern"
ZIP_NAME="Bern_linux_2.0.0.zip"
VERSION="2.0.0"
INSTALLER_LATEST=""

# Colors
readonly PURPLE='\033[35m'
readonly GRAY='\033[90m'
readonly GREEN='\033[32m'
readonly CYAN='\033[36m'
readonly YELLOW='\033[33m'
readonly RESET='\033[0m'

# Print colored message
log() {
    local color=$1
    shift
    echo -e "${color}$*${RESET}"
}

# Read a value from the cached manifest. Uses jq when available and falls back
# to a grep/sed scan of the pretty-printed JSON otherwise. $1 is a jq filter,
# $2 is the raw JSON snippet to scan in the no-jq path.
manifest_value() {
    local jq_filter="$1" raw="$2"
    if command -v jq >/dev/null 2>&1; then
        printf '%s' "$MANIFEST" | jq -r "$jq_filter // empty" 2>/dev/null
    else
        printf '%s' "$raw" | grep -o '"[^"]*"[[:space:]]*$' | head -1 | sed 's/^"\(.*\)"[[:space:]]*$/\1/'
    fi
}

# Fetch the manifest and override the defaults. Degrades silently to the
# built-in defaults if anything fails (offline, Pages down, bad JSON).
resolve_manifest() {
    local manifest bern_block installer_block linux_block
    manifest=$(curl -fsSL --max-time 10 "$MANIFEST_URL" 2>/dev/null) || return 0
    [[ -n "$manifest" ]] || return 0
    MANIFEST="$manifest"

    # Flattened single-line blocks for the no-jq fallback.
    local flat
    flat=$(printf '%s' "$manifest" | tr '\n' ' ')
    bern_block=$(printf '%s' "$flat" | grep -o '"bern"[^}]*')
    installer_block=$(printf '%s' "$flat" | grep -o '"installer"[^}]*')
    linux_block=$(printf '%s' "$flat" | grep -o '"linux"[^}]*}')

    local v
    v=$(manifest_value '.bern.version'      "$(printf '%s' "$bern_block"      | grep -o '"version"[^,]*' | head -1)"); [[ -n "$v" ]] && VERSION="$v"
    v=$(manifest_value '.bern.repo'         "$(printf '%s' "$bern_block"      | grep -o '"repo"[^,]*')");              [[ -n "$v" ]] && REPO="$v"
    v=$(manifest_value '.bern.linux.zip'    "$(printf '%s' "$linux_block"     | grep -o '"zip"[^,}]*')");             [[ -n "$v" ]] && ZIP_NAME="$v"
    v=$(manifest_value '.installer.version' "$(printf '%s' "$installer_block" | grep -o '"version"[^,]*' | head -1)"); [[ -n "$v" ]] && INSTALLER_LATEST="$v"
    return 0
}

# Return 0 if $1 is strictly newer than $2 (semantic-ish version compare).
version_gt() {
    [[ "$1" != "$2" ]] && [[ "$(printf '%s\n%s\n' "$1" "$2" | sort -V | tail -1)" == "$1" ]]
}

# Warn when the installer script itself is out of date.
check_installer_update() {
    [[ -n "$INSTALLER_LATEST" ]] || return 0
    if version_gt "$INSTALLER_LATEST" "$SCRIPT_VERSION"; then
        log "$YELLOW" "[bern] A newer installer is available (v$INSTALLER_LATEST, you have v$SCRIPT_VERSION)."
        log "$YELLOW" "[bern] Re-run to get the latest: curl -fsSL $INSTALLER_URL | bash"
        echo
    fi
}

# Print banner
print_banner() {
    log "$PURPLE" "______                  "
    log "$PURPLE" "| ___ \\                      The Bern Language Installer "
    log "$PURPLE" "| |_/ / ___ _ __ _ __       "
    log "$PURPLE" "| ___ \/ _ \\ '__| '_ \\     This script will install Bern on"
    log "$PURPLE" "| |_/ /  __/ |  | | | |                 your machine"
    log "$PURPLE" "\\____/ \\___|_|  |_| |_|         [ v.$VERSION ]"
    log "$GRAY" "--------------------------------------------------------------\n"
}

# Download and extract Bern
install_bern() {
    log "$CYAN" "[bern] Installing to $INSTALL_DIR..."

    # Report whether this is a fresh install, a repair, or an update.
    local version_file="$INSTALL_DIR/version.txt"
    if [[ -f "$version_file" ]]; then
        local installed
        installed=$(tr -d '[:space:]' < "$version_file")
        if [[ "$installed" == "$VERSION" ]]; then
            log "$GRAY" "[bern] Bern v$VERSION is already installed - reinstalling/repairing."
        else
            log "$CYAN" "[bern] Updating Bern v$installed -> v$VERSION."
        fi
    fi

    sudo mkdir -p "$INSTALL_DIR"
    
    # Fetch latest release
    log "$CYAN" "[bern] Fetching latest release from GitHub..."
    local release_url="https://api.github.com/repos/$REPO/releases/latest"
    local download_url
    download_url=$(curl -sL "$release_url" | grep -o "https://.*/$ZIP_NAME" | head -1)
    
    if [[ -z "$download_url" ]]; then
        log "$YELLOW" "[bern] Error: Could not find $ZIP_NAME in latest release" >&2
        exit 1
    fi
    
    # Download to temp directory
    local temp_dir
    temp_dir=$(mktemp -d)
    trap 'rm -rf "$temp_dir"' EXIT
    
    log "$CYAN" "[bern] Downloading $ZIP_NAME..."
    curl -sL "$download_url" -o "$temp_dir/$ZIP_NAME"
    
    # Extract
    log "$CYAN" "[bern] Extracting..."
    unzip -q "$temp_dir/$ZIP_NAME" -d "$temp_dir"
    
    # Install binary
    if [[ ! -f "$temp_dir/Bern" ]]; then
        log "$YELLOW" "[bern] Error: Bern binary not found in archive" >&2
        exit 1
    fi
    
    sudo cp "$temp_dir/Bern" "$INSTALL_DIR/Bern"
    sudo chmod +x "$INSTALL_DIR/Bern"
    
    # Create lowercase wrapper
    sudo tee "$INSTALL_DIR/bern" > /dev/null << 'EOF'
#!/usr/bin/env bash
exec "$(dirname "$0")/Bern" "$@"
EOF
    sudo chmod +x "$INSTALL_DIR/bern"
    
    # Install lib folder if present
    if [[ -d "$temp_dir/lib" ]]; then
        sudo rm -rf "$INSTALL_DIR/lib"
        sudo cp -r "$temp_dir/lib" "$INSTALL_DIR/"
        log "$GREEN" "[bern] lib folder installed"
    fi
    
    # Record the installed version so future runs can detect updates.
    printf '%s' "$VERSION" | sudo tee "$INSTALL_DIR/version.txt" > /dev/null

    log "$GREEN" "[bern] Bern binary installed successfully"
}

# Configure PATH
setup_path() {
    # Try system-wide installation first
    if [[ -w /etc/profile.d ]] || [[ $EUID -eq 0 ]]; then
        log "$CYAN" "[bern] Configuring system-wide PATH..."
        
        # Bash/sh/zsh
        sudo tee /etc/profile.d/bern.sh > /dev/null << 'EOF'
export PATH="/opt/bern:$PATH"
EOF
        sudo chmod 644 /etc/profile.d/bern.sh
        
        # Fish
        sudo mkdir -p /etc/fish/conf.d
        sudo tee /etc/fish/conf.d/bern.fish > /dev/null << 'EOF'
set -gx PATH /opt/bern $PATH
EOF
        sudo chmod 644 /etc/fish/conf.d/bern.fish
        
        log "$YELLOW" "[bern] PATH configured system-wide. Restart your shell or run:"
        log "$YELLOW" "       source /etc/profile.d/bern.sh"
    else
        # User-level installation
        local user_home="${SUDO_USER:+$(eval echo "~$SUDO_USER")}"
        user_home="${user_home:-$HOME}"
        local shell_name
        shell_name=$(basename "${SHELL:-bash}")
        
        case "$shell_name" in
            fish)
                local rc="$user_home/.config/fish/config.fish"
                local line="set -gx PATH /opt/bern \$PATH"
                ;;
            zsh)
                local rc="$user_home/.zshrc"
                local line='export PATH="/opt/bern:$PATH"'
                ;;
            *)
                local rc="$user_home/.bashrc"
                local line='export PATH="/opt/bern:$PATH"'
                ;;
        esac
        
        if ! grep -qF "/opt/bern" "$rc" 2>/dev/null; then
            log "$CYAN" "[bern] Adding /opt/bern to $rc..."
            echo "$line" >> "$rc"
            log "$YELLOW" "[bern] PATH updated. Restart your terminal or run:"
            log "$YELLOW" "       source $rc"
        else
            log "$GRAY" "[bern] /opt/bern already in PATH"
        fi
    fi
}

# Main installation
main() {
    resolve_manifest
    print_banner
    check_installer_update
    install_bern
    setup_path
    echo
    log "$GREEN" "[bern] Installation complete! Bern v$VERSION is installed."
    log "$CYAN" "[bern] Run 'bern' or 'Bern' to get started"
}

main "$@"