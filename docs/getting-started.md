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

The `-nox` variant is recommended for spacecadet since it runs in batch mode and doesn't need a GUI.

## Installation

```bash
git clone https://github.com/jamesdp3/spacecadet.git
cd spacecadet
./setup.sh
```

The setup script:

1. Checks that `emacs` and `python3` are in your PATH
2. Installs the `mcp` Python package
3. Creates a default `tasks/tasks.org` file
4. Verifies the Emacs configuration loads correctly
5. Prints configuration instructions

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

### Custom org directory

By default, tasks are stored in `spacecadet/tasks/`. To use an existing org directory:

```bash
export SPACECADET_ORG_DIR=~/org
```

Set this in your shell profile or pass it as an environment variable in your MCP client config:

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
