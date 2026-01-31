# spacecadet

An MCP server that gives AI assistants reliable task management through Emacs org-mode.

## Why?

LLMs are unreliable at tracking tasks in conversation context. They lose count, forget deadlines, and hallucinate state. Spacecadet solves this by delegating task storage and retrieval to org-mode -- a deterministic, structured system with 100% accuracy.

## What it does

Spacecadet exposes 14 MCP tools that let any AI assistant:

- **Create, update, delete, and list tasks** with priorities, tags, deadlines, and scheduled dates
- **Search and query** using org-mode's full match syntax
- **Track time** with clock-in/clock-out and reporting
- **Organize** with notes, custom properties, and refiling

Every task gets a unique ID for reliable programmatic access.

## Quick start

```bash
git clone https://github.com/jamesdp3/spacecadet.git
cd spacecadet
./setup.sh
```

Then configure your MCP client -- see [Getting Started](getting-started.md) for details.

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

Each tool call invokes Emacs in batch mode with an isolated configuration. No interference with your personal Emacs setup. See [Architecture](architecture.md) for the full picture.
