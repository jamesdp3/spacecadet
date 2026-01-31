# Tool Reference

All tools that operate on a specific task accept either `id` (preferred) or `heading` to identify the target. See [Task IDs](task-ids.md) for details.

---

## Task CRUD

### add_task

Create a new task. Returns the auto-assigned task ID.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `heading` | string | yes | The task title |
| `priority` | string | no | Priority letter: A, B, C, or D |
| `tags` | string | no | Comma-separated tags, e.g. `"work,urgent"` |
| `deadline` | string | no | Deadline date in `YYYY-MM-DD` format |
| `scheduled` | string | no | Scheduled date in `YYYY-MM-DD` format |
| `state` | string | no | Initial TODO state: `TODO`, `NEXT`, `WAITING` (default: `TODO`) |
| `file` | string | no | Target org filename, e.g. `"projects.org"` (default: `tasks.org`) |

**Example response:**

```json
{
  "status": "ok",
  "id": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
  "heading": "Write API docs",
  "todo": "TODO",
  "file": "/home/user/spacecadet/tasks/tasks.org"
}
```

### update_task

Update an existing task's state, priority, or deadline.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `heading` | string | no* | The exact task heading |
| `id` | string | no* | The task's org-id |
| `new_state` | string | no | New state: `TODO`, `NEXT`, `WAITING`, `DONE`, `CANCELLED` |
| `new_priority` | string | no | New priority letter: A-D |
| `new_deadline` | string | no | New deadline in `YYYY-MM-DD` format |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{"status": "ok", "updated": true}
```

### delete_task

Remove a task from the org file.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `heading` | string | no* | The exact task heading |
| `id` | string | no* | The task's org-id |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{"status": "ok", "deleted": true}
```

### list_tasks

List tasks with optional filters. Returns a JSON array.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `state` | string | no | Filter by state: `TODO`, `NEXT`, `WAITING`, `DONE` |
| `priority` | string | no | Filter by priority: A, B, C, D |
| `tag` | string | no | Filter by tag name |

**Example response:**

```json
[
  {
    "id": "a1b2c3d4-...",
    "heading": "Write API docs",
    "todo": "TODO",
    "priority": "A",
    "deadline": "<2026-02-15>",
    "scheduled": null,
    "tags": ["work", "docs"],
    "effort": "3:00",
    "file": "/home/user/spacecadet/tasks/tasks.org"
  }
]
```

### get_task

Get full details of a specific task, including body text.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `heading` | string | no* | The exact task heading |
| `id` | string | no* | The task's org-id |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{
  "id": "a1b2c3d4-...",
  "heading": "Write API docs",
  "todo": "TODO",
  "priority": "A",
  "deadline": "<2026-02-15>",
  "scheduled": null,
  "tags": ["work", "docs"],
  "body": "LOGBOOK entries, notes, and any body text...",
  "file": "/home/user/spacecadet/tasks/tasks.org"
}
```

---

## Querying

### get_agenda

Get the org-mode agenda view for a date or date range.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `date` | string | no | Specific date in `YYYY-MM-DD` format (defaults to today) |
| `range_start` | string | no | Start of range in `YYYY-MM-DD` format |
| `range_end` | string | no | End of range in `YYYY-MM-DD` format |

If `range_start` and `range_end` are both provided, returns the agenda for that range. Otherwise uses `date` (or today).

### search_tasks

Search tasks using org-mode match syntax. Returns the agenda buffer output.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `query` | string | yes | Org-mode match string |

**Query examples:**

| Query | Meaning |
|-------|---------|
| `+work` | Tasks tagged `work` |
| `+work+urgent` | Tasks tagged both `work` AND `urgent` |
| `+work-meetings` | Tasks tagged `work` but NOT `meetings` |
| `+work/TODO` | `work` tasks in `TODO` state |

---

## Time Tracking

### clock_in

Start the clock on a task.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `heading` | string | no* | The exact task heading |
| `id` | string | no* | The task's org-id |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{"status": "ok", "clocked-in": true, "time": "2026-01-31 14:30:00"}
```

### clock_out

Stop the clock on the currently clocked task. Takes no parameters.

**Example response:**

```json
{"status": "ok", "clocked-out": true, "duration": "1:30", "time": "2026-01-31 16:00"}
```

### clock_report

Get a time report showing hours logged per task. Takes no parameters.

**Example response:**

```json
[
  {
    "heading": "Write API docs",
    "todo": "TODO",
    "minutes": 90,
    "formatted": "1:30",
    "file": "/home/user/spacecadet/tasks/tasks.org"
  }
]
```

---

## Organization

### add_note

Add a timestamped note to a task's LOGBOOK.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `note` | string | yes | The note text to add |
| `heading` | string | no* | The exact task heading |
| `id` | string | no* | The task's org-id |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{"status": "ok", "note-added": true}
```

### set_property

Set a custom property on a task.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `property` | string | yes | Property name (e.g. `"Effort"`, `"Assignee"`, `"URL"`) |
| `value` | string | yes | Property value (e.g. `"2:00"`, `"Alice"`, `"https://..."`) |
| `heading` | string | no* | The exact task heading |
| `id` | string | no* | The task's org-id |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{"status": "ok", "property": "Effort", "value": "2:00"}
```

### refile_task

Move a task under a different heading or into a different file.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `target_heading` | string | yes | The heading to refile under |
| `heading` | string | no* | The exact task heading to move |
| `id` | string | no* | The task's org-id |
| `target_file` | string | no | Specific org filename to refile into |

*At least one of `id` or `heading` is required.

**Example response:**

```json
{"status": "ok", "refiled-to": "Projects"}
```
