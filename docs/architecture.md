# Architecture

## System overview

```
AI Client (Claude Desktop, Claude Code, etc.)
    |
    |  MCP protocol (stdio, JSON-RPC)
    |
    v
server.py  (Python 3.10+, FastMCP)
    |
    |  subprocess: emacs --batch -Q --load init.el --eval "(fn ...)"
    |  user data passed via environment variables
    |
    v
emacs-config/init.el  (Emacs Lisp)
    |
    |  org-mode API: read/write/search
    |
    v
tasks/*.org  (plain text org files)
```

## Components

### server.py -- MCP server

The Python layer handles:

- **MCP protocol** via FastMCP (stdio transport)
- **Tool definitions** with parameter validation and docstrings
- **Input sanitization** -- path traversal prevention, query character filtering
- **Environment variable injection** -- user data is never inlined into elisp expressions
- **File locking** -- `fcntl` advisory locks prevent concurrent write corruption (Unix only)
- **Subprocess management** -- each tool call spawns a fresh `emacs --batch` process with a 30-second timeout

### emacs-config/init.el -- Elisp backend

The Emacs layer handles:

- **Org-mode configuration** -- TODO states, priorities, logging, agenda settings
- **Task CRUD** -- creating, reading, updating, deleting org headings
- **Task lookup** -- by heading (exact match) or by org-id (UUID)
- **Agenda and search** -- date-based views and org-mode match queries
- **Clocking** -- time tracking with CLOCK entries in LOGBOOK drawers
- **JSON output** -- all responses are JSON-encoded for server.py to parse

### tasks/*.org -- Data storage

Plain text org-mode files. The default file is `tasks.org`. Tasks can span multiple files by using the `file` parameter in `add_task` or `SPACECADET_ORG_DIR` pointing to a directory with multiple `.org` files.

## Security model

### Input injection prevention

User-supplied data (task headings, notes, property values) is passed to Emacs via environment variables (`SC_HEADING`, `SC_NOTE`, etc.), never inlined into elisp expressions. The elisp code reads these with `(getenv "SC_HEADING")`.

This prevents injection attacks where a malicious heading like `") (delete-file "/etc/passwd") ("` could execute arbitrary elisp.

### Path traversal prevention

The `file` and `target_file` parameters are validated by `validate_org_path()` in server.py. It resolves the candidate path and checks that it falls within `ORG_DIR` using `Path.relative_to()`. Attempts to escape with `../` or absolute paths are rejected.

### Query sanitization

Org-mode match queries in `list_tasks` and `search_tasks` are filtered to allow only alphanumeric characters and a small set of operators (`+`, `-`, `_`, `/`, `=`).

## Batch mode isolation

Every tool call runs `emacs --batch -Q --load init.el`. The flags mean:

- `--batch` -- non-interactive, no window system
- `-Q` -- ignore the user's `~/.emacs`, `~/.emacs.d/init.el`, and site-start files
- `--load init.el` -- load only the spacecadet configuration

This ensures spacecadet never interferes with or depends on the user's personal Emacs setup. Each invocation is a fresh process with no persistent state.

## File locking

On Unix systems, write operations acquire an advisory lock on `.spacecadet.lock` via `fcntl.flock()`. This prevents corruption if two MCP tool calls try to write simultaneously. The lock is released when the subprocess completes.

On Windows (without WSL), file locking is skipped. This is acceptable for single-client usage but means concurrent writes are not protected.
