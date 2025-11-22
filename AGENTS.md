# Agent Guide for Fractal Project

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
bd create "Issue title" -p 1 --deps blocks:bd-123 --json
```

**Manage dependencies:**
```bash
# Add dependency: issue-A blocks issue-B (A must complete before B can start)
bd dep add issue-A issue-B -t blocks --json

# Other dependency types
bd dep add issue-A issue-B -t related --json
bd dep add parent-id child-id -t parent-child --json
bd dep add source-id discovered-id -t discovered-from --json

# View dependency tree
bd dep tree issue-id --json

# Remove dependency
bd dep remove issue-A issue-B --json
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### Git Worktree Usage

When working in a git worktree, special care is needed with bd:

**Problem**: Git worktrees share the `.beads/` directory from the main repository. Running bd in daemon mode can cause it to commit/push changes to the wrong branch.

**Solution**: Always use `--no-daemon` mode in worktrees:

```bash
# Import after git operations (pull, checkout, etc.)
bd --no-daemon import -i .beads/issues.jsonl

# All other commands
bd --no-daemon ready --json
bd --no-daemon create "Task" --json
bd --no-daemon update bd-42 --status in_progress --json
bd --no-daemon close bd-42 --reason "Done" --json
```

**Why the explicit path?** The `-i .beads/issues.jsonl` flag is required because bd's auto-detection doesn't work reliably in worktree environments. The explicit path ensures bd reads from the correct shared issues file.

**When to import manually:**
- After `git pull` or `git fetch`
- After switching branches with `git checkout`
- When you see "Database out of sync with JSONL" errors
- Before starting work in a fresh worktree

**Workflow in worktrees:**
1. Enter worktree: `cd /path/to/worktree`
2. Import issues: `bd --no-daemon import -i .beads/issues.jsonl`
3. Check ready work: `bd --no-daemon ready --json`
4. Work on issues using `--no-daemon` flag
5. Commit both code and `.beads/issues.jsonl` together

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the beads MCP server:

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json`):
```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI commands.

### Managing AI-Generated Planning Documents

AI assistants often create planning and design documents during development:
- PLAN.md, IMPLEMENTATION.md, ARCHITECTURE.md
- DESIGN.md, CODEBASE_SUMMARY.md, INTEGRATION_PLAN.md
- TESTING_GUIDE.md, TECHNICAL_DESIGN.md, and similar files

**Best Practice: Use a dedicated directory for these ephemeral files**

**Recommended approach:**
- Create a `history/` directory in the project root
- Store ALL AI-generated planning/design docs in `history/`
- Keep the repository root clean and focused on permanent project files
- Only access `history/` when explicitly asked to review past planning

**Example .gitignore entry (optional):**
```
# AI planning documents (ephemeral)
history/
```

**Benefits:**
- ✅ Clean repository root
- ✅ Clear separation between ephemeral and permanent documentation
- ✅ Easy to exclude from version control if desired
- ✅ Preserves planning history for archeological research
- ✅ Reduces noise when browsing the project

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ✅ Store AI planning docs in `history/` directory
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems
- ❌ Do NOT clutter repo root with planning documents

For more details, see README.md and QUICKSTART.md.
