#!/bin/bash
# spacecadet setup script
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== spacecadet setup ==="
echo ""

# Check emacs
if ! command -v emacs &> /dev/null; then
    echo "ERROR: emacs not found in PATH."
    echo "Install emacs first: https://www.gnu.org/software/emacs/"
    exit 1
fi
echo "[ok] emacs found: $(emacs --version | head -1)"

# Check python3
if ! command -v python3 &> /dev/null; then
    echo "ERROR: python3 not found in PATH."
    exit 1
fi
echo "[ok] python3 found: $(python3 --version)"

# Install Python dependencies
echo ""
echo "Installing Python dependencies..."
pip3 install -r "$SCRIPT_DIR/requirements.txt" 2>&1 | tail -1
echo "[ok] Python dependencies installed"

# Create default tasks directory and file
TASKS_DIR="$SCRIPT_DIR/tasks"
mkdir -p "$TASKS_DIR"
if [ ! -f "$TASKS_DIR/tasks.org" ]; then
    cat > "$TASKS_DIR/tasks.org" << 'ORGEOF'
#+TITLE: Spacecadet Tasks
#+STARTUP: overview

ORGEOF
    echo "[ok] Created default tasks file: $TASKS_DIR/tasks.org"
else
    echo "[ok] Tasks file exists: $TASKS_DIR/tasks.org"
fi

# Verify emacs can load our config
echo ""
echo "Verifying emacs configuration..."
if emacs --batch -Q --load "$SCRIPT_DIR/emacs-config/init.el" --eval "(princ \"ok\")" 2>/dev/null | grep -q "ok"; then
    echo "[ok] Emacs configuration loads correctly"
else
    echo "WARNING: Emacs configuration may have issues"
fi

echo ""
echo "=== Setup complete ==="
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
echo "To use with Claude Code, add to your MCP settings:"
echo ""
echo "  claude mcp add spacecadet python3 $SCRIPT_DIR/server.py"
echo ""
echo "To use a custom org directory, set SPACECADET_ORG_DIR:"
echo ""
echo "  export SPACECADET_ORG_DIR=~/org"
echo ""
