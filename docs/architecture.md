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
    |  emacsclient --eval "(progn (setenv ...) (fn ...))"
    |  user data passed via (setenv ...) calls
    |
    v
Emacs daemon  (persistent, started on first request)
    |
    |  org-mode API: read/write/search
    |
    v
~/spacecadet-tasks/*.org  (plain text org files)
```

## Components

### server.py -- MCP server

The Python layer handles:

- **MCP protocol** via FastMCP (stdio transport)
- **Tool definitions** with parameter validation and docstrings
- **Input sanitization** -- path traversal prevention, query character filtering
- **Environment variable injection** -- user data is passed via `(setenv ...)` calls, never inlined into elisp expressions
- **File locking** -- `fcntl` advisory locks prevent concurrent write corruption (Unix only)
- **Daemon lifecycle** -- starts an Emacs daemon on first request, shuts it down on exit via `atexit`
- **Configuration** -- resolves org directory from env var, config file, or default

### emacs-config/init.el -- Elisp backend

The Emacs layer handles:

- **Org-mode configuration** -- TODO states, priorities, logging, agenda settings
- **Task CRUD** -- creating, reading, updating, deleting org headings
- **Task lookup** -- by heading (exact match) or by org-id (UUID)
- **Agenda and search** -- date-based views and org-mode match queries
- **Clocking** -- time tracking with CLOCK entries in LOGBOOK drawers
- **JSON output** -- all responses are JSON-encoded for server.py to parse

### Org files -- Data storage

Plain text org-mode files stored in the configured org directory (default: `~/spacecadet-tasks/`). The default file is `tasks.org`. Tasks can span multiple files by using the `file` parameter in `add_task` or pointing `SPACECADET_ORG_DIR` at a directory with multiple `.org` files.

Since org files are plain text, they work with any file-syncing service (iCloud, Dropbox, Google Drive, OneDrive, Syncthing) for cross-machine access.

## Security model

### Input injection prevention

User-supplied data (task headings, notes, property values) is passed to Emacs via environment variables (`SC_HEADING`, `SC_NOTE`, etc.), never inlined into elisp expressions. The elisp code reads these with `(getenv "SC_HEADING")`.

This prevents injection attacks where a malicious heading like `") (delete-file "/etc/passwd") ("` could execute arbitrary elisp.

### Path traversal prevention

The `file` and `target_file` parameters are validated by `validate_org_path()` in server.py. It resolves the candidate path and checks that it falls within `ORG_DIR` using `Path.relative_to()`. Attempts to escape with `../` or absolute paths are rejected.

### Query sanitization

Org-mode match queries in `list_tasks` and `search_tasks` are filtered to allow only alphanumeric characters and a small set of operators (`+`, `-`, `_`, `/`, `=`).

## Daemon architecture

On the first tool call, spacecadet starts a dedicated Emacs daemon:

```
emacs --daemon=spacecadet-{pid} -Q --load init.el
```

The flags mean:

- `--daemon=spacecadet-{pid}` -- run as a named daemon (unique per server process)
- `-Q` -- ignore the user's `~/.emacs`, `~/.emacs.d/init.el`, and site-start files
- `--load init.el` -- load only the spacecadet configuration

Subsequent tool calls use `emacsclient --socket-name=spacecadet-{pid} --eval "..."` to evaluate elisp against the running daemon. This avoids the ~4 second startup cost of `emacs --batch` on each call, keeping response times under 500ms.

The daemon is isolated from the user's personal Emacs setup and runs with its own socket. It is shut down automatically when the server exits via an `atexit` handler.

Before each evaluation, all `SC_*` environment variables are cleared and re-set to prevent data leaking between calls. Org-mode buffers are also refreshed to ensure the daemon reads the latest file contents from disk.

## File locking

On Unix systems, write operations acquire an advisory lock on `.spacecadet.lock` via `fcntl.flock()`. This prevents corruption if two MCP tool calls try to write simultaneously. The lock is released when the subprocess completes.

On Windows (without WSL), file locking is skipped. This is acceptable for single-client usage but means concurrent writes are not protected.
