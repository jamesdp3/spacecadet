# spacecadet

An MCP (Model Context Protocol) server that gives AI assistants reliable task management through Emacs org-mode.

Instead of relying on LLMs to track tasks in conversation context (where they fail at counting, temporal reasoning, and filtering), spacecadet delegates task storage and retrieval to org-mode -- a deterministic, structured system with 100% accuracy.

## Prerequisites

- **Emacs** (any recent version with built-in org-mode)
- **Python 3.10+**
- **Linux or macOS** (Windows works under WSL; native Windows has no file locking)

Org-mode ships with Emacs. If you don't have Emacs installed:

```bash
brew install emacs              # macOS
sudo apt install emacs-nox      # Ubuntu/Debian/WSL
sudo dnf install emacs-nox      # Fedora
sudo pacman -S emacs-nox        # Arch
```

The `-nox` variant is recommended since spacecadet doesn't need a GUI.

## Setup

```bash
git clone https://github.com/jamesdp3/spacecadet.git
cd spacecadet
./setup.sh
```

The setup script installs dependencies and prints configuration instructions for your MCP client.

## Configuration

### Claude Desktop

Add to your MCP settings (`~/.config/claude/claude_desktop_config.json`):

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

### Claude Code

```bash
claude mcp add spacecadet python3 /path/to/spacecadet/server.py
```

### Task storage

On first run, spacecadet creates a `tasks.org` file in `~/spacecadet-tasks/` and saves this path to `.spacecadet.conf`. Tasks persist across restarts.

To change the task directory, set the `SPACECADET_ORG_DIR` environment variable:

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

You can also edit `.spacecadet.conf` directly -- it contains a single line with the path.

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

## Tools

Every task is assigned a unique ID on creation. Tools that operate on a specific task accept either `id` (preferred) or `heading` to identify the target task.

### Task CRUD

| Tool | Description |
|------|-------------|
| `add_task` | Create a new task with optional priority, tags, deadline, scheduled date. Returns the task's `id`. |
| `update_task` | Update a task's state (e.g. mark DONE), priority, or deadline |
| `delete_task` | Remove a task |
| `list_tasks` | List all tasks, optionally filtered by state, priority, or tag. Each task includes its `id`. |
| `get_task` | Get full details of a specific task |

### Querying

| Tool | Description |
|------|-------------|
| `get_agenda` | Get the org-mode agenda view for a date or date range |
| `search_tasks` | Search using full org-mode match syntax |

### Time tracking

| Tool | Description |
|------|-------------|
| `clock_in` | Start the clock on a task |
| `clock_out` | Stop the clock on the currently clocked task |
| `clock_report` | Get a time report showing hours logged per task |

### Organization

| Tool | Description |
|------|-------------|
| `add_note` | Add a timestamped note to a task's LOGBOOK |
| `set_property` | Set a custom property on a task (Effort, Assignee, URL, etc.) |
| `refile_task` | Move a task under a different heading or into a different file |

## How it works

```
AI Client (Claude Desktop, Claude Code, etc.)
    |  MCP protocol (stdio)
    v
server.py  (Python, FastMCP)
    |  emacsclient --eval
    v
Emacs daemon  (persistent, started on first request)
    |  org-mode API
    v
~/spacecadet-tasks/*.org  (plain text)
```

On the first tool call, spacecadet starts a dedicated Emacs daemon with an isolated configuration (no interference with your personal Emacs setup). Subsequent calls use `emacsclient` to evaluate elisp against the running daemon, keeping response times under 500ms. The daemon is automatically shut down when the server exits.

## Task IDs

Every task gets a UUID assigned automatically when created via `add_task`. The ID is stored as an org-mode `ID` property and returned in all tool responses. Use the `id` parameter instead of `heading` for reliable lookups -- headings can be ambiguous if duplicated, but IDs are always unique.

## Task states

Tasks follow this workflow:

```
TODO -> NEXT -> WAITING -> DONE
                        -> CANCELLED
```

## Priorities

- **A** - Highest priority
- **B** - High priority
- **C** - Default priority
- **D** - Low priority

## Running tests

```bash
pip install -e ".[dev]"
pytest
```

Unit tests (path validation) run without emacs. Integration tests are skipped automatically if emacs is not installed.

## Documentation

Full documentation: [https://jamesdp3.github.io/spacecadet](https://jamesdp3.github.io/spacecadet)

## License

MIT
