# Task IDs

## Overview

Every task created through spacecadet is automatically assigned a UUID. This ID is stored as an org-mode `ID` property in the task's property drawer and is returned in all API responses.

## Why IDs?

Heading-based lookup has problems:

- **Ambiguity** -- two tasks can have the same heading. Heading lookup matches the first one found.
- **Fragility** -- if the AI slightly misremembers a heading (wrong capitalization, missing word), the lookup fails.
- **Renaming** -- if a heading is edited outside spacecadet, heading-based references break.

IDs solve all of these. They're unique, immutable, and independent of the heading text.

## How it works

When you call `add_task`, spacecadet:

1. Creates the org heading
2. Calls `org-id-get-create` to generate a UUID
3. Stores it in a `:PROPERTIES:` drawer
4. Returns the ID in the response

The resulting org entry looks like:

```org
* TODO Write API docs                                        :work:docs:
  :PROPERTIES:
  :ID:       a1b2c3d4-e5f6-7890-abcd-ef1234567890
  :END:
```

## Using IDs

All tools that operate on a specific task accept both `id` and `heading`:

```json
{"id": "a1b2c3d4-e5f6-7890-abcd-ef1234567890"}
```

or:

```json
{"heading": "Write API docs"}
```

When both are provided, `id` takes precedence. When neither is provided, the tool returns an error.

## IDs in list responses

`list_tasks` and `search_tasks` (JSON mode) include the `id` field for every task:

```json
[
  {
    "id": "a1b2c3d4-...",
    "heading": "Write API docs",
    "todo": "TODO",
    ...
  }
]
```

## Recommended workflow

1. Call `add_task` -- save the returned `id`
2. Use `id` for all subsequent operations on that task
3. Use `heading` only as a fallback when you don't have the ID (e.g. for tasks created outside spacecadet)
