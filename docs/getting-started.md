# Getting Started

## Prerequisites

- **Emacs** -- any recent version with built-in org-mode
- **Python 3.10+**
- **Linux or macOS** -- Windows works under WSL; native Windows has no file locking

### Installing Emacs

Org-mode ships with Emacs, so there's nothing extra to install beyond Emacs itself.

**macOS:**

```bash
brew install emacs
```

**Ubuntu / Debian / WSL:**

```bash
sudo apt install emacs-nox
```

**Fedora:**

```bash
sudo dnf install emacs-nox
```

**Arch:**

```bash
sudo pacman -S emacs-nox
```

The `-nox` variant is recommended since spacecadet doesn't need a GUI.

## Installation

```bash
git clone https://github.com/jamesdp3/spacecadet.git
cd spacecadet
./setup.sh
```

The setup script:

1. Checks that `emacs` and `python3` are in your PATH
2. Installs the `mcp` Python package
3. Verifies the Emacs configuration loads correctly
4. Prints configuration instructions

## Configuration

### Claude Desktop

Add to your MCP settings file (`~/.config/claude/claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "spacecadet": {
      "command": "python3",
      "args": ["/path/to/spacecadet/server.py"]
    }
  }
}
```

Replace `/path/to/spacecadet` with the actual path where you cloned the repo.

### Claude Code

```bash
claude mcp add spacecadet python3 /path/to/spacecadet/server.py
```

## Task storage

On first run, spacecadet automatically creates `~/spacecadet-tasks/tasks.org` and saves this path to `.spacecadet.conf` (in the project root). Tasks persist across restarts.

The directory is resolved in this order:

1. **`SPACECADET_ORG_DIR` environment variable** -- highest priority
2. **`.spacecadet.conf` file** -- persisted from previous run
3. **`~/spacecadet-tasks/`** -- default on first run

### Using a custom directory

To use an existing org directory (e.g. your personal org-mode files):

```bash
export SPACECADET_ORG_DIR=~/org
```

Or pass it in your MCP client config:

```json
{
  "mcpServers": {
    "spacecadet": {
      "command": "python3",
      "args": ["/path/to/spacecadet/server.py"],
      "env": {
        "SPACECADET_ORG_DIR": "/home/you/org"
      }
    }
  }
}
```

You can also edit `.spacecadet.conf` directly -- it contains a single line with the absolute path to the org directory.

### Cloud sync

Point `SPACECADET_ORG_DIR` at a cloud-synced folder to access your tasks from any machine:

| Service | Example path |
|---------|-------------|
| **iCloud** | `~/Library/Mobile Documents/com~apple~CloudDocs/org` |
| **Dropbox** | `~/Dropbox/org` |
| **Google Drive** | `~/Google Drive/My Drive/org` |
| **OneDrive** | `~/OneDrive/org` |
| **Syncthing** | `~/Sync/org` |

Since tasks are plain `.org` text files, any file-syncing service works. If you already use org-mode, point spacecadet at your existing org directory and it will read your files directly.

!!! tip "Multiple machines"
    Install spacecadet on each machine and set `SPACECADET_ORG_DIR` to the same synced folder. Each instance runs its own Emacs daemon independently -- there are no server-side dependencies or databases to coordinate.

## Task states

Tasks follow this workflow:

```
TODO -> NEXT -> WAITING -> DONE
                        -> CANCELLED
```

## Priorities

| Priority | Meaning |
|----------|---------|
| **A** | Highest priority |
| **B** | High priority |
| **C** | Default priority |
| **D** | Low priority |
