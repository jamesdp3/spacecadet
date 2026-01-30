#!/usr/bin/env python3
"""spacecadet - MCP server wrapping Emacs org-mode for AI task management."""

import fcntl
import json
import os
import subprocess
from pathlib import Path

from mcp.server.fastmcp import FastMCP

# --- Configuration ---
ROOT_DIR = Path(__file__).parent.resolve()
INIT_FILE = ROOT_DIR / "emacs-config" / "init.el"
ORG_DIR = os.environ.get("SPACECADET_ORG_DIR", str(ROOT_DIR / "tasks"))

mcp = FastMCP("spacecadet")


def _lock_path() -> Path:
    """Path for the file lock."""
    return Path(ORG_DIR) / ".spacecadet.lock"


def run_emacs(elisp_expr: str, extra_env: dict | None = None,
              timeout: int = 30, write: bool = False):
    """Run an elisp expression in emacs batch mode.

    User-supplied data is passed via environment variables (extra_env),
    never inlined in the elisp expression, to prevent injection.

    Args:
        elisp_expr: The elisp expression to evaluate (no user data).
        extra_env: Environment variables to pass user data to elisp.
        timeout: Subprocess timeout in seconds.
        write: If True, acquire a file lock for write safety.
    """
    env = os.environ.copy()
    env["SPACECADET_ORG_DIR"] = ORG_DIR
    if extra_env:
        env.update(extra_env)

    cmd = [
        "emacs", "--batch", "-Q",
        "--load", str(INIT_FILE),
        "--eval", elisp_expr,
    ]

    lock_fd = None
    try:
        if write:
            lock_file = _lock_path()
            lock_fd = open(lock_file, "w")
            fcntl.flock(lock_fd, fcntl.LOCK_EX)

        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout, env=env,
        )
    except subprocess.TimeoutExpired:
        return {"status": "error", "message": "Emacs command timed out"}
    except FileNotFoundError:
        return {"status": "error", "message": "emacs not found in PATH"}
    finally:
        if lock_fd:
            fcntl.flock(lock_fd, fcntl.LOCK_UN)
            lock_fd.close()

    stdout = result.stdout.strip()
    if not stdout:
        stderr = result.stderr.strip()
        if stderr:
            return {"status": "error", "message": stderr[:500]}
        return {"status": "ok", "message": "No output"}

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
        env["SC_FILE"] = file
    return _result_to_str(run_emacs("(spacecadet-add-task-from-env)", env, write=True))


@mcp.tool()
def update_task(
    heading: str,
    new_state: str | None = None,
    new_priority: str | None = None,
    new_deadline: str | None = None,
) -> str:
    """Update an existing task. Use this to mark tasks as DONE, change priority, etc.

    Args:
        heading: The exact task heading to find
        new_state: New TODO state - TODO, NEXT, WAITING, DONE, CANCELLED (optional)
        new_priority: New priority letter A-D (optional)
        new_deadline: New deadline date YYYY-MM-DD (optional)
    """
    env = {"SC_HEADING": heading}
    if new_state:
        env["SC_NEW_STATE"] = new_state
    if new_priority:
        env["SC_NEW_PRIORITY"] = new_priority
    if new_deadline:
        env["SC_NEW_DEADLINE"] = new_deadline
    return _result_to_str(run_emacs("(spacecadet-update-task-from-env)", env, write=True))


@mcp.tool()
def delete_task(heading: str) -> str:
    """Remove a task from the org file.

    Args:
        heading: The exact task heading to find and delete
    """
    return _result_to_str(run_emacs(
        "(spacecadet-delete-task-from-env)",
        {"SC_HEADING": heading}, write=True))


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
def get_task(heading: str) -> str:
    """Get detailed information about a specific task.

    Args:
        heading: The exact task heading to find
    """
    return _result_to_str(run_emacs(
        "(spacecadet-get-task-from-env)",
        {"SC_HEADING": heading}))


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
def clock_in(heading: str) -> str:
    """Start the clock on a task. Tracks time spent working on it.

    Args:
        heading: The exact task heading to clock into
    """
    return _result_to_str(run_emacs(
        "(spacecadet-clock-in-from-env)",
        {"SC_HEADING": heading}, write=True))


@mcp.tool()
def clock_out() -> str:
    """Stop the clock on the currently clocked task."""
    return _result_to_str(run_emacs("(spacecadet-clock-out)", write=True))


@mcp.tool()
def clock_report() -> str:
    """Get a time report showing hours logged per task."""
    return _result_to_str(run_emacs("(spacecadet-clock-report)"))


@mcp.tool()
def add_note(heading: str, note: str) -> str:
    """Add a note or log entry to an existing task.

    Args:
        heading: The exact task heading to find
        note: The note text to add
    """
    return _result_to_str(run_emacs(
        "(spacecadet-add-note-from-env)",
        {"SC_HEADING": heading, "SC_NOTE": note}, write=True))


@mcp.tool()
def set_property(heading: str, property: str, value: str) -> str:
    """Set a custom property on a task (e.g. Effort, Assignee, URL).

    Args:
        heading: The exact task heading to find
        property: Property name (e.g. "Effort", "Assignee", "URL")
        value: Property value (e.g. "2:00", "Alice", "https://...")
    """
    return _result_to_str(run_emacs(
        "(spacecadet-set-property-from-env)",
        {"SC_HEADING": heading, "SC_PROPERTY": property, "SC_VALUE": value},
        write=True))


@mcp.tool()
def refile_task(heading: str, target_heading: str, target_file: str | None = None) -> str:
    """Move a task under a different heading (refile).

    Args:
        heading: The exact task heading to move
        target_heading: The heading to refile under
        target_file: Specific org filename to refile into (optional)
    """
    env = {"SC_HEADING": heading, "SC_TARGET_HEADING": target_heading}
    if target_file:
        env["SC_TARGET_FILE"] = target_file
    return _result_to_str(run_emacs(
        "(spacecadet-refile-task-from-env)", env, write=True))


if __name__ == "__main__":
    mcp.run(transport="stdio")
