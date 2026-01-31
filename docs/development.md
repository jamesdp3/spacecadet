# Development

## Project structure

```
spacecadet/
├── server.py              # MCP server (Python, FastMCP)
├── emacs-config/
│   └── init.el            # Elisp backend (org-mode operations)
├── tasks/
│   └── tasks.org          # Default task storage
├── tests/
│   ├── __init__.py
│   ├── test_path_validation.py   # Unit tests (no emacs required)
│   └── test_integration.py       # Integration tests (requires emacs)
├── docs/                  # Documentation source (MkDocs)
├── mkdocs.yml             # MkDocs configuration
├── pyproject.toml         # Python project metadata
├── requirements.txt       # Python dependencies
├── setup.sh               # Setup script
├── LICENSE                # MIT license
└── README.md
```

## Running tests

Install dev dependencies and run:

```bash
pip install -e ".[dev]"
pytest
```

### Test categories

**Unit tests** (`tests/test_path_validation.py`):

- Path traversal validation
- Task environment variable builder
- No external dependencies -- runs anywhere with Python

**Integration tests** (`tests/test_integration.py`):

- Full tool operations: add, get, list, update, delete, refile, notes, properties
- ID-based and heading-based lookup
- Path traversal rejection end-to-end
- Requires emacs -- automatically skipped if emacs is not in PATH
- Each test uses an isolated temporary org directory

Run only unit tests:

```bash
pytest tests/test_path_validation.py
```

Run only integration tests:

```bash
pytest tests/test_integration.py
```

## Adding a new tool

1. **Define the elisp function** in `emacs-config/init.el`:
    - Read parameters from environment variables using `(spacecadet--env "SC_PARAM")`
    - Use `spacecadet--find-task-smart` for task lookup (supports both ID and heading)
    - Output JSON via `(princ (json-encode ...))`
    - Call `(save-buffer)` after writes

2. **Define the MCP tool** in `server.py`:
    - Add `@mcp.tool()` decorated function
    - For task-targeting tools, accept both `id` and `heading` parameters
    - Use `_task_env(id, heading)` to build the environment dict
    - Pass user data via `extra_env`, never inline in elisp strings
    - Set `write=True` in `run_emacs()` for write operations (enables file locking)

3. **Add tests** in `tests/test_integration.py`:
    - Use the `org_dir` fixture for an isolated test environment
    - Test both success and error cases

## Building docs

```bash
pip install mkdocs mkdocs-material
mkdocs serve        # local preview at http://127.0.0.1:8000
mkdocs gh-deploy    # deploy to GitHub Pages
```
