# spacecadet

An MCP (Model Context Protocol) server that gives AI assistants reliable task management through Emacs org-mode.

Instead of relying on LLMs to track tasks in conversation context (where they fail at counting, temporal reasoning, and filtering), spacecadet delegates task storage and retrieval to org-mode -- a deterministic, structured system with 100% accuracy.

## Prerequisites

- **Emacs** (any recent version with built-in org-mode)
- **Python 3.10+**

## Setup

```bash
git clone <repo-url> spacecadet
cd spacecadet
./setup.sh
```

The setup script installs dependencies, creates a default tasks directory, and prints configuration instructions for your MCP client.

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

### Custom org directory

By default, tasks are stored in `spacecadet/tasks/`. To use your own org directory:

```bash
export SPACECADET_ORG_DIR=~/org
```

## Tools

### Task CRUD

| Tool | Description |
|------|-------------|
| `add_task` | Create a new task with optional priority, tags, deadline, scheduled date |
| `update_task` | Update a task's state (e.g. mark DONE), priority, or deadline |
| `delete_task` | Remove a task |
| `list_tasks` | List all tasks, optionally filtered by state, priority, or tag |
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
    |  subprocess
    v
emacs --batch -Q --load init.el --eval "(elisp-fn ...)"
    |  reads/writes
    v
tasks/*.org
```

Each tool call invokes Emacs in batch mode with an isolated configuration (no interference with your personal Emacs setup). Emacs reads/writes org files directly, providing deterministic accuracy for all task operations.

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

## License

MIT
