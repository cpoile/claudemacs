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
- **Organize TODOs**: Create structured task lists with `notes_add_todo`
- **Structure information**: Use headings (`notes_add_heading`) to organize notes by topic
- **Add timestamps**: Mark when things happened with `notes_add_timestamp`
- **Track time**: Use `notes_clock_in`/`notes_clock_out` for time tracking on tasks

### Recommended Usage Patterns

1. **Starting a session**: Check existing notes with `get_notes` to restore context
2. **During work**: Update notes as you learn things or make decisions
3. **Complex tasks**: Create TODO items and mark them DONE as you complete them
4. **Before ending**: Summarize key findings or next steps in notes

### Notes Template

When starting fresh or reorganizing notes, use this structure:

```org
* Session Context
Brief description of what we're working on.

** Key Files
- path/to/important/file.py - description
- path/to/another.rs - description

** Decisions Made
- [2024-01-15] Chose X approach because Y
- [2024-01-15] Using Z library for W

* Current Work
** TODO Active task description                                    :tag:
:PROPERTIES:
:EFFORT: 1:00
:END:
Notes about the task...

*** Subtask 1
*** Subtask 2

** DONE Completed task                                             :done:
CLOSED: [2024-01-15 Mon 14:30]
What was accomplished.

* Research & Notes
** Topic Name
Notes, findings, code snippets...

#+begin_src python
# Code examples
#+end_src

* Questions & Blockers
- [ ] Open question needing user input
- [X] Resolved question

* Archive                                                        :ARCHIVE:
Completed work gets refiled here.
```

### Org-Mode Formatting Tips

- Use `*` for headings (more stars = deeper nesting)
- Use `- [ ]` for checkboxes, `- [X]` for checked
- Use `TODO`/`DONE` keywords after heading stars
- Use `:tag:` syntax at end of headings for tags
- Use `#+begin_src lang ... #+end_src` for code blocks
- Use `:PROPERTIES:` drawer for metadata
- Timestamps: `<2024-01-15 Mon>` (active) or `[2024-01-15 Mon]` (inactive)
