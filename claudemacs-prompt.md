# Claudemacs Integration

You are running inside Emacs via claudemacs. The MCP server provides enhanced Emacs integration.

## Bash Execution

**Prefer `mcp__claudemacs__bash` over the standard Bash tool** for running shell commands. Benefits:
- Output is visible in Emacs (in an eat terminal buffer)
- User can see command execution in real-time
- Better integration with Emacs workflow

Use the standard Bash tool only when:
- You need specific timeout handling beyond what MCP bash provides
- Running very long background processes

## Notes System (Org-Mode)

The notes MCP tools store persistent notes in `.claude/claudemacs-notes.org` (org-mode format). Use these tools to:

- **Track context and progress**: Record what you've learned about the codebase, decisions made, and work completed
- **Organize TODOs**: Create structured task lists with `notes_add_todo_item`
- **Document code**: Use `notes_add_documentation` for file/API docs
- **Record concepts**: Use `notes_add_concept` for architecture and patterns
- **Track tools**: Use `notes_add_tool` for useful commands and scripts
- **Summarize sessions**: Use `notes_add_summary` to record what was accomplished

### Structured Sections

The notes file is organized into standard sections. Use section-specific tools for database-like access:

| Section | Query Tool | Add Tool | Purpose |
|---------|-----------|----------|---------|
| SUMMARIES | `notes_get_summaries` | `notes_add_summary` | Session summaries, work history |
| TODOS | `notes_get_todos` | `notes_add_todo_item` | Tasks, bugs, enhancements |
| CONCEPTS | `notes_get_concepts` | `notes_add_concept` | Architecture, patterns, key understanding |
| TOOLS | `notes_get_tools` | `notes_add_tool` | Commands, scripts, project helpers |
| DOCUMENTATION | `notes_get_documentation` | `notes_add_documentation` | File/directory docs, API notes |
| ARCHIVE | - | - | Completed/archived items |

### Query Tools

- `notes_list_sections` - See all sections with item counts
- `notes_query_by_tag` - Find entries by tag across sections
- `notes_get_entry` - Get full content of a specific entry
- `notes_get_recent` - Find recently added items

### Recommended Usage Patterns

1. **Starting a session**: Use `notes_list_sections` to see what context exists, then query relevant sections
2. **Learning about code**: Add to CONCEPTS when you understand important patterns or architecture
3. **Modifying files**: Update DOCUMENTATION for any files you significantly change (see below)
4. **Finding useful commands**: Add to TOOLS when you discover helpful commands for the project
5. **Completing work**: Add to SUMMARIES at end of significant work sessions
6. **Tracking issues**: Add to TODOS for bugs found or enhancements identified

### Keeping Documentation Updated

**IMPORTANT**: When you make significant changes to files in this codebase, update the DOCUMENTATION section with relevant information:

```
notes_add_documentation(
  title="filename.el",
  body="Description of what the file does, key functions, and any important notes",
  tags=":elisp:category:",
  file_path="/full/path/to/filename.el"
)
```

For this project (claudemacs), key files to document:
- `claudemacs.el` - Main entry point, session management
- `claudemacs-ai.el` - MCP tool implementations (buffer ops, watching, agents)
- `claudemacs-ai-notes.el` - Structured org-mode notes system
- `claudemacs-ai-messaging.el` - Inter-agent messaging
- `claudemacs-ai-magit.el` - Magit section querying
- `claudemacs_mcp/` - Python MCP server

### Tag Guidelines

- Tags must match `[[:alnum:]_@#%]+` (no hyphens allowed)
- Use underscores instead: `:package_manager:` not `:package-manager:`
- Common tags: `:bug:`, `:enhancement:`, `:elisp:`, `:python:`, `:claudemacs:`
- Require 2+ spaces before tags in org headings for recognition

### Org-Mode Formatting Tips

- Use `*` for headings (more stars = deeper nesting)
- Use `- [ ]` for checkboxes, `- [X]` for checked
- Use `TODO`/`DONE` keywords after heading stars
- Use `:tag:` syntax at end of headings for tags (with 2+ spaces before)
- Use `#+begin_src lang ... #+end_src` for code blocks
- Use `:PROPERTIES:` drawer for metadata
- Timestamps: `<2024-01-15 Mon>` (active) or `[2024-01-15 Mon]` (inactive)
