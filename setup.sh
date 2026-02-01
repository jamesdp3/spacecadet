#!/bin/bash
# spacecadet setup script
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== spacecadet setup ==="
echo ""

# Check emacs
if ! command -v emacs &> /dev/null; then
    echo "ERROR: emacs not found in PATH."
    echo "Install emacs first:"
    echo "  brew install emacs              # macOS"
    echo "  sudo apt install emacs-nox      # Ubuntu/Debian/WSL"
    echo "  sudo dnf install emacs-nox      # Fedora"
    echo "  sudo pacman -S emacs-nox        # Arch"
    exit 1
fi
echo "[ok] emacs found: $(emacs --version | head -1)"

# Check emacsclient
if ! command -v emacsclient &> /dev/null; then
    echo "ERROR: emacsclient not found in PATH (should come with emacs)."
    exit 1
fi
echo "[ok] emacsclient found: $(emacsclient --version | head -1)"

# Check python3
if ! command -v python3 &> /dev/null; then
    echo "ERROR: python3 not found in PATH."
    exit 1
fi
echo "[ok] python3 found: $(python3 --version)"

# Install Python dependencies
echo ""
echo "Installing Python dependencies..."
pip3 install -e "$SCRIPT_DIR" 2>&1 | tail -1
echo "[ok] Python dependencies installed"

# Verify emacs can load our config
echo ""
echo "Verifying emacs configuration..."
if emacs --batch -Q --load "$SCRIPT_DIR/emacs-config/init.el" --eval "(princ \"ok\")" 2>/dev/null | grep -q "ok"; then
    echo "[ok] Emacs configuration loads correctly"
else
    echo "WARNING: Emacs configuration may have issues"
fi

# Task storage configuration
echo ""
DEFAULT_ORG_DIR="$HOME/spacecadet-tasks"
if [ -n "$SPACECADET_ORG_DIR" ]; then
    ORG_DIR="$SPACECADET_ORG_DIR"
    echo "[ok] Using SPACECADET_ORG_DIR: $ORG_DIR"
elif [ -f "$SCRIPT_DIR/.spacecadet.conf" ]; then
    ORG_DIR="$(cat "$SCRIPT_DIR/.spacecadet.conf" | tr -d '[:space:]')"
    echo "[ok] Using configured directory: $ORG_DIR"
else
    ORG_DIR="$DEFAULT_ORG_DIR"
    echo "$ORG_DIR" > "$SCRIPT_DIR/.spacecadet.conf"
    echo "[ok] Task directory set to: $ORG_DIR"
fi

mkdir -p "$ORG_DIR"
if [ ! -f "$ORG_DIR/tasks.org" ]; then
    cat > "$ORG_DIR/tasks.org" << 'ORGEOF'
#+TITLE: Spacecadet Tasks
#+STARTUP: overview

ORGEOF
    echo "[ok] Created default tasks file: $ORG_DIR/tasks.org"
else
    echo "[ok] Tasks file exists: $ORG_DIR/tasks.org"
fi

echo ""
echo "=== Setup complete ==="
echo ""
echo "Tasks are stored in: $ORG_DIR"
echo ""
echo "To use with Claude Desktop, add to your MCP settings:"
echo ""
echo "  {"
echo "    \"mcpServers\": {"
echo "      \"spacecadet\": {"
echo "        \"command\": \"python3\","
echo "        \"args\": [\"$SCRIPT_DIR/server.py\"]"
echo "      }"
echo "    }"
echo "  }"
echo ""
echo "To use with Claude Code:"
echo ""
echo "  claude mcp add spacecadet python3 $SCRIPT_DIR/server.py"
echo ""
echo "To change the task directory:"
echo ""
echo "  export SPACECADET_ORG_DIR=~/your/org/folder"
echo ""
echo "For cloud sync, point SPACECADET_ORG_DIR at a synced folder"
echo "(Dropbox, iCloud, Google Drive, OneDrive, Syncthing, etc.)"
echo ""
