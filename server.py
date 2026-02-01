#!/usr/bin/env python3
"""spacecadet - MCP server wrapping Emacs org-mode for AI task management."""

import atexit
import json
import os
import subprocess
import sys
import time
from pathlib import Path

# fcntl is only available on Unix; on Windows we skip file locking
if sys.platform != "win32":
    import fcntl
else:
    fcntl = None

from mcp.server.fastmcp import FastMCP

# --- Configuration ---
ROOT_DIR = Path(__file__).parent.resolve()
INIT_FILE = ROOT_DIR / "emacs-config" / "init.el"
ORG_DIR = os.environ.get("SPACECADET_ORG_DIR", str(ROOT_DIR / "tasks"))

SOCKET_NAME = f"spacecadet-{os.getpid()}"
_daemon_started = False

mcp = FastMCP("spacecadet")


def _lock_path() -> Path:
    """Path for the file lock."""
    return Path(ORG_DIR) / ".spacecadet.lock"


def _elisp_escape(s: str) -> str:
    """Escape a string for embedding in an elisp double-quoted string."""
    return s.replace("\\", "\\\\").replace('"', '\\"')


def _start_daemon():
    """Start an Emacs daemon for this server process."""
    global _daemon_started
    if _daemon_started:
        return

    org_dir_escaped = _elisp_escape(ORG_DIR)

    subprocess.Popen(
        [
            "emacs", f"--daemon={SOCKET_NAME}", "-Q",
            "--load", str(INIT_FILE),
            "--eval", f'(setenv "SPACECADET_ORG_DIR" "{org_dir_escaped}")',
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    # Wait for the daemon socket to appear
    for _ in range(100):
        try:
            result = subprocess.run(
                ["emacsclient", f"--socket-name={SOCKET_NAME}", "--eval", "(+ 1 1)"],
                capture_output=True, text=True, timeout=2,
            )
            if result.returncode == 0:
                _daemon_started = True
                return
        except (subprocess.TimeoutExpired, FileNotFoundError):
            pass
        time.sleep(0.1)

    raise RuntimeError("Emacs daemon failed to start within 10 seconds")


def _stop_daemon():
    """Kill the Emacs daemon on exit."""
    try:
        subprocess.run(
            ["emacsclient", f"--socket-name={SOCKET_NAME}", "--eval", "(kill-emacs)"],
            capture_output=True, text=True, timeout=5,
        )
    except Exception:
        pass


atexit.register(_stop_daemon)


def run_emacs(elisp_expr: str, extra_env: dict | None = None,
              timeout: int = 30, write: bool = False):
    """Run an elisp expression via emacsclient against the daemon.

    User-supplied data is passed via (setenv ...) calls in the elisp,
    never inlined in the main expression, to prevent injection.

    Args:
        elisp_expr: The elisp expression to evaluate (no user data).
        extra_env: Environment variables to pass user data to elisp.
        timeout: Subprocess timeout in seconds.
        write: If True, acquire a file lock for write safety.
    """
    _start_daemon()

    # Build a progn that sets env vars then evaluates the expression.
    # The elisp reads env vars via (getenv ...), same as before.
    # Clear all SC_* env vars first to prevent leakage between calls.
    sc_vars = ["SC_ID", "SC_HEADING", "SC_PRIORITY", "SC_TAGS", "SC_DEADLINE",
               "SC_SCHEDULED", "SC_STATE", "SC_FILE", "SC_NEW_STATE",
               "SC_NEW_PRIORITY", "SC_NEW_DEADLINE", "SC_NOTE", "SC_PROPERTY",
               "SC_VALUE", "SC_TARGET_HEADING", "SC_TARGET_FILE"]
    setenv_forms = [f'(setenv "{v}" nil)' for v in sc_vars]
    setenv_forms.append(f'(setenv "SPACECADET_ORG_DIR" "{_elisp_escape(ORG_DIR)}")')
    if extra_env:
        for k, v in extra_env.items():
            setenv_forms.append(f'(setenv "{_elisp_escape(k)}" "{_elisp_escape(v)}")')

    # Also re-set spacecadet-org-dir so the elisp functions see the current value
    setenv_forms.append(
        f'(setq spacecadet-org-dir "{_elisp_escape(ORG_DIR)}")'
    )
    setenv_forms.append(
        f'(setq org-agenda-files (list spacecadet-org-dir))'
    )

    # Kill all org buffers so the daemon reads fresh from disk on next access.
    # We kill rather than revert because revert-buffer triggers expensive mode
    # hooks (font-lock, org-mode parsing) that can hang on slow filesystems.
    refresh_form = (
        '(dolist (buf (buffer-list))'
        '  (with-current-buffer buf'
        '    (when (and buffer-file-name (string-suffix-p ".org" buffer-file-name))'
        '      (set-buffer-modified-p nil)'
        '      (kill-buffer buf))))'
    )
    wrapped = f'(progn {" ".join(setenv_forms)} {refresh_form} (with-output-to-string {elisp_expr}))'

    cmd = ["emacsclient", f"--socket-name={SOCKET_NAME}", "--eval", wrapped]

    lock_fd = None
    try:
        if write and fcntl is not None:
            lock_file = _lock_path()
            lock_fd = open(lock_file, "w")
            fcntl.flock(lock_fd, fcntl.LOCK_EX)

        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return {"status": "error", "message": "Emacs command timed out"}
    except FileNotFoundError:
        return {"status": "error", "message": "emacsclient not found in PATH"}
    finally:
        if lock_fd:
            fcntl.flock(lock_fd, fcntl.LOCK_UN)
            lock_fd.close()

    stdout = result.stdout.strip()
    if not stdout or stdout == '""' or stdout == "nil":
        stderr = result.stderr.strip()
        if stderr:
            return {"status": "error", "message": stderr[:500]}
        return {"status": "ok", "message": "No output"}

    # emacsclient --eval prints the elisp string representation, which wraps
    # the output in quotes and escapes inner quotes. Unwrap it.
    if stdout.startswith('"') and stdout.endswith('"'):
        # Remove outer quotes and unescape the emacsclient wrapper layer only.
        # The inner JSON is already properly escaped, so we just undo the
        # outer elisp string quoting: \" -> " and \\ -> \
        stdout = stdout[1:-1]
        stdout = stdout.replace('\\\\', '\x00').replace('\\"', '"').replace('\x00', '\\')

    try:
        parsed = json.loads(stdout)
        # Ensure we never return None (elisp nil -> JSON null)
        return parsed if parsed is not None else []
    except json.JSONDecodeError:
        return stdout


def _result_to_str(result) -> str:
    """Convert run_emacs result to a string for MCP."""
    if isinstance(result, (dict, list)):
        return json.dumps(result)
    return str(result)


def _task_env(id: str | None, heading: str | None) -> dict:
    """Build env dict for task lookup. At least one of id/heading required."""
    env = {}
    if id:
        env["SC_ID"] = id
    if heading:
        env["SC_HEADING"] = heading
    if not env:
        raise ValueError("Either id or heading is required")
    return env


def validate_org_path(filename: str, param_name: str = "file") -> str:
    """Validate that filename resolves to a path inside ORG_DIR.

    Returns the filename if safe. Raises ValueError if the resolved
    path would escape ORG_DIR (e.g. via '..' traversal or absolute paths).
    """
    org_dir = Path(ORG_DIR).resolve()
    candidate = (org_dir / filename).resolve()
    try:
        candidate.relative_to(org_dir)
    except ValueError:
        raise ValueError(
            f"Invalid {param_name}: path would escape the org directory"
        )
    return filename


# --- MCP Tools ---

@mcp.tool()
def add_task(
    heading: str,
    priority: str | None = None,
    tags: str | None = None,
    deadline: str | None = None,
    scheduled: str | None = None,
    state: str | None = None,
    file: str | None = None,
) -> str:
    """Add a new task to the org file.

    Args:
        heading: The task title
        priority: Priority letter A-D (optional)
        tags: Comma-separated tags e.g. "work,urgent" (optional)
        deadline: Deadline date YYYY-MM-DD (optional)
        scheduled: Scheduled date YYYY-MM-DD (optional)
        state: TODO state - TODO, NEXT, WAITING (default: TODO)
        file: Target org filename e.g. "projects.org" (default: tasks.org)
    """
    env = {"SC_HEADING": heading}
    if priority:
        env["SC_PRIORITY"] = priority
    if tags:
        env["SC_TAGS"] = tags
    if deadline:
        env["SC_DEADLINE"] = deadline
    if scheduled:
        env["SC_SCHEDULED"] = scheduled
    if state:
        env["SC_STATE"] = state
    if file:
        try:
            validate_org_path(file, "file")
        except ValueError as e:
            return json.dumps({"status": "error", "message": str(e)})
        env["SC_FILE"] = file
    return _result_to_str(run_emacs("(spacecadet-add-task-from-env)", env, write=True))


@mcp.tool()
def update_task(
    heading: str | None = None,
    id: str | None = None,
    new_state: str | None = None,
    new_priority: str | None = None,
    new_deadline: str | None = None,
) -> str:
    """Update an existing task. Use this to mark tasks as DONE, change priority, etc.

    Args:
        heading: The exact task heading to find (provide heading or id)
        id: The task's org-id (preferred over heading)
        new_state: New TODO state - TODO, NEXT, WAITING, DONE, CANCELLED (optional)
        new_priority: New priority letter A-D (optional)
        new_deadline: New deadline date YYYY-MM-DD (optional)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    if new_state:
        env["SC_NEW_STATE"] = new_state
    if new_priority:
        env["SC_NEW_PRIORITY"] = new_priority
    if new_deadline:
        env["SC_NEW_DEADLINE"] = new_deadline
    return _result_to_str(run_emacs("(spacecadet-update-task-from-env)", env, write=True))


@mcp.tool()
def delete_task(heading: str | None = None, id: str | None = None) -> str:
    """Remove a task from the org file.

    Args:
        heading: The exact task heading to find and delete (provide heading or id)
        id: The task's org-id (preferred over heading)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    return _result_to_str(run_emacs(
        "(spacecadet-delete-task-from-env)", env, write=True))


@mcp.tool()
def list_tasks(
    state: str | None = None,
    priority: str | None = None,
    tag: str | None = None,
) -> str:
    """List tasks with optional filters. Returns JSON array of tasks.

    Args:
        state: Filter by TODO state - TODO, NEXT, WAITING, DONE (optional)
        priority: Filter by priority - A, B, C, D (optional)
        tag: Filter by tag name (optional)
    """
    # Build org-mode match string from filters
    # This is safe because we validate the inputs are simple alphanumeric values
    parts = []
    if tag:
        # Tags should be simple identifiers
        safe_tag = "".join(c for c in tag if c.isalnum() or c == "_")
        parts.append(f"+{safe_tag}")
    if priority:
        safe_pri = priority.upper()[0] if priority else ""
        if safe_pri in "ABCD":
            parts.append(f'+PRIORITY=\\"{safe_pri}\\"')

    if parts:
        match = "".join(parts)
        if state:
            safe_state = "".join(c for c in state.upper() if c.isalpha())
            match += f"/{safe_state}"
        expr = f'(spacecadet-match-to-json "{match}")'
    elif state:
        safe_state = "".join(c for c in state.upper() if c.isalpha())
        expr = f'(spacecadet-match-to-json "/{safe_state}")'
    else:
        expr = "(spacecadet-tasks-to-json)"

    return _result_to_str(run_emacs(expr))


@mcp.tool()
def get_task(heading: str | None = None, id: str | None = None) -> str:
    """Get detailed information about a specific task.

    Args:
        heading: The exact task heading to find (provide heading or id)
        id: The task's org-id (preferred over heading)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    return _result_to_str(run_emacs(
        "(spacecadet-get-task-from-env)", env))


@mcp.tool()
def get_agenda(
    date: str | None = None,
    range_start: str | None = None,
    range_end: str | None = None,
) -> str:
    """Get the org-mode agenda view for a date or date range.

    Args:
        date: Specific date YYYY-MM-DD (optional, defaults to today)
        range_start: Start of date range YYYY-MM-DD (optional)
        range_end: End of date range YYYY-MM-DD (optional)
    """
    # Date strings are validated by org-read-date in elisp; we sanitize here too
    def safe_date(d: str) -> str:
        return "".join(c for c in d if c.isdigit() or c == "-")

    if range_start and range_end:
        expr = f'(spacecadet-agenda-for-range "{safe_date(range_start)}" "{safe_date(range_end)}")'
    elif date:
        expr = f'(spacecadet-agenda-for-date "{safe_date(date)}")'
    else:
        expr = '(spacecadet-agenda-to-stdout "d")'
    return _result_to_str(run_emacs(expr))


@mcp.tool()
def search_tasks(query: str) -> str:
    """Search tasks using org-mode match syntax.

    Examples:
        "+work" - tasks tagged work
        "+work+urgent" - tasks tagged both work AND urgent
        "+work-meetings" - tasks tagged work but NOT meetings
        "+work/TODO" - work tasks in TODO state

    Args:
        query: Org-mode match string
    """
    # Sanitize: only allow alphanumeric, +, -, _, /, =, ", backslash
    safe_query = "".join(c for c in query if c.isalnum() or c in '+-_/="\\ ')
    expr = f'(spacecadet-match-query "{safe_query}")'
    return _result_to_str(run_emacs(expr))


@mcp.tool()
def clock_in(heading: str | None = None, id: str | None = None) -> str:
    """Start the clock on a task. Tracks time spent working on it.

    Args:
        heading: The exact task heading to clock into (provide heading or id)
        id: The task's org-id (preferred over heading)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    return _result_to_str(run_emacs(
        "(spacecadet-clock-in-from-env)", env, write=True))


@mcp.tool()
def clock_out() -> str:
    """Stop the clock on the currently clocked task."""
    return _result_to_str(run_emacs("(spacecadet-clock-out)", write=True))


@mcp.tool()
def clock_report() -> str:
    """Get a time report showing hours logged per task."""
    return _result_to_str(run_emacs("(spacecadet-clock-report)"))


@mcp.tool()
def add_note(note: str, heading: str | None = None, id: str | None = None) -> str:
    """Add a note or log entry to an existing task.

    Args:
        note: The note text to add
        heading: The exact task heading to find (provide heading or id)
        id: The task's org-id (preferred over heading)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    env["SC_NOTE"] = note
    return _result_to_str(run_emacs(
        "(spacecadet-add-note-from-env)", env, write=True))


@mcp.tool()
def set_property(property: str, value: str, heading: str | None = None, id: str | None = None) -> str:
    """Set a custom property on a task (e.g. Effort, Assignee, URL).

    Args:
        property: Property name (e.g. "Effort", "Assignee", "URL")
        value: Property value (e.g. "2:00", "Alice", "https://...")
        heading: The exact task heading to find (provide heading or id)
        id: The task's org-id (preferred over heading)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    env["SC_PROPERTY"] = property
    env["SC_VALUE"] = value
    return _result_to_str(run_emacs(
        "(spacecadet-set-property-from-env)", env, write=True))


@mcp.tool()
def refile_task(
    target_heading: str,
    heading: str | None = None,
    id: str | None = None,
    target_file: str | None = None,
) -> str:
    """Move a task under a different heading (refile).

    Args:
        target_heading: The heading to refile under
        heading: The exact task heading to move (provide heading or id)
        id: The task's org-id (preferred over heading)
        target_file: Specific org filename to refile into (optional)
    """
    try:
        env = _task_env(id, heading)
    except ValueError as e:
        return json.dumps({"status": "error", "message": str(e)})
    env["SC_TARGET_HEADING"] = target_heading
    if target_file:
        try:
            validate_org_path(target_file, "target_file")
        except ValueError as e:
            return json.dumps({"status": "error", "message": str(e)})
        env["SC_TARGET_FILE"] = target_file
    return _result_to_str(run_emacs(
        "(spacecadet-refile-task-from-env)", env, write=True))


if __name__ == "__main__":
    mcp.run(transport="stdio")
