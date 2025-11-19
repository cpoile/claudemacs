# Claudemacs AI Integration Instructions

> **Note**: This file is automatically loaded by Claude Code when starting a session in this directory. You (Claude) will have this context available from the start of every claudemacs session.

## What is Claudemacs?

You (Claude) are running inside a special integration that connects Claude Code with Emacs. This means:

- **You're in an eat-mode terminal inside Emacs** - The user can see this terminal buffer and interact with it
- **You have direct Emacs access** - Via the `claudemacs-cli` tool that's pre-configured in your PATH
- **You can read and modify buffers** - All open Emacs buffers are accessible to you
- **You can send commands to REPLs** - Interact with any REPL running in Emacs
- **Two-way integration** - The user can select code/text in Emacs and send it to you, and you can write back to their buffers

## Quick Start

You have **two ways** to interact with Emacs:

### 1. MCP Tools (Preferred)

If MCP is enabled, you'll have native tools available:
- `get_buffer_content` - Read buffer content
- `list_buffers` - List all buffers
- `buffer_info` - Get buffer details
- `get_region` - Read specific region
- `insert_in_buffer` - Write to buffer (requires permission)
- `replace_region` - Replace content (requires permission)
- `send_input` - Send to REPL (requires permission)
- `exec_in_terminal` - Run commands (requires permission)

### 2. CLI Tool (Fallback)

The `claudemacs-cli` bash tool is also available:

```bash
# See what buffers are open
claudemacs-cli list-buffers

# Read a buffer's content
claudemacs-cli get-buffer-content "myfile.py"

# Write to a buffer
claudemacs-cli insert-in-buffer "*scratch*" "Hello from Claude!"
```

**Note**: MCP tools are preferred when available as they integrate better with Claude's permission system.

## Core Concepts

1. **Buffers**: In Emacs, everything is a buffer - files, terminals, REPLs, scratch pads. Buffer names like `*scratch*` or `*Messages*` are special buffers with asterisks.

2. **Point**: The cursor position in Emacs is called "point". Position numbers start at 1, not 0.

3. **Working Directory**: Your shell's working directory is set to the project root where claudemacs was launched.

4. **Socket Communication**: All commands go through an Emacs server socket (pre-configured in `$CLAUDEMACS_SOCKET`).

## Available Commands

### Buffer Content Operations

#### `claudemacs-cli insert-in-buffer BUFFER TEXT`
Insert text into a buffer at the current point position.

Example:
```bash
claudemacs-cli insert-in-buffer "*scratch*" "Hello from Claude!"
```

#### `claudemacs-cli get-buffer-content BUFFER`
Retrieve the entire content of a buffer.

Example:
```bash
claudemacs-cli get-buffer-content "*Messages*"
```

#### `claudemacs-cli get-region BUFFER START END`
Get content from a specific region in a buffer (by character position).

Example:
```bash
claudemacs-cli get-region "myfile.py" 1 100
```

#### `claudemacs-cli replace-region BUFFER START END TEXT`
Replace content in a region with new text.

Example:
```bash
claudemacs-cli replace-region "myfile.py" 50 75 "new code here"
```

### Buffer Information

#### `claudemacs-cli list-buffers`
Get a list of all open buffers.

Example:
```bash
claudemacs-cli list-buffers
```

#### `claudemacs-cli buffer-info BUFFER`
Get detailed information about a buffer (file path, size, major mode, point position, etc.).

Example:
```bash
claudemacs-cli buffer-info "README.md"
```

### Navigation

#### `claudemacs-cli goto-point BUFFER POSITION`
Move point to a specific character position in a buffer.

Example:
```bash
claudemacs-cli goto-point "*scratch*" 100
```

### REPL Integration

#### `claudemacs-cli send-input BUFFER TEXT`
Send input to a REPL buffer (works with comint-mode, eshell, etc.). The text will be inserted and submitted.

Example:
```bash
claudemacs-cli send-input "*eshell*" "ls -la"
claudemacs-cli send-input "*Python*" "print('Hello')"
```

#### `claudemacs-cli exec-in-terminal BUFFER COMMAND [--timeout SECONDS]`
Execute a command in an eat terminal buffer and wait for it to complete, returning the output.

**Note**: Works best with eat shell integration enabled (see "Shell Integration Setup" below).

Example:
```bash
claudemacs-cli exec-in-terminal "*eat*" "echo 'Hello World'"
claudemacs-cli exec-in-terminal "*eat*" "ls -la | head -5"
claudemacs-cli exec-in-terminal "*eat*" "npm test" --timeout 60
```

### Advanced

#### `claudemacs-cli eval EXPRESSION`
Evaluate an arbitrary elisp expression. Use with caution.

Example:
```bash
claudemacs-cli eval "(message \"Hello from elisp\")"
```

## Use Cases

### Reading User Buffers
You can read the content of any open buffer to understand what the user is working on:
```bash
claudemacs-cli get-buffer-content "main.py"
```

### Sending Commands to REPLs
You can execute code in running REPL sessions:
```bash
claudemacs-cli send-input "*Python*" "import numpy as np"
```

### Inspecting State
You can list all buffers to see what the user has open:
```bash
claudemacs-cli list-buffers
```

### Writing Notes
You can write information to specific buffers:
```bash
claudemacs-cli insert-in-buffer "*scratch*" "# Notes from Claude\n"
```

## Permissions

Each `claudemacs-cli` command will require your normal bash tool permissions. This ensures the user maintains control over what you can do.

## Error Handling

If a command fails (e.g., buffer doesn't exist), you'll receive an error message via stderr. Always check for errors before proceeding with dependent operations.

## Tips

1. **Always check if a buffer exists** before operating on it using `buffer-info` or `list-buffers`
2. **Be careful with positions** - character positions in Emacs start at 1, not 0
3. **Escape special characters** - the CLI handles most escaping, but be aware of shell quoting
4. **Use buffer names carefully** - buffer names can contain special characters like `*`, which need proper quoting in bash

## Environment Variables

- `CLAUDEMACS_SOCKET`: Pre-configured path to the Emacs server socket
- Can be overridden with `--socket` flag if needed

## Shell Integration Setup (Recommended)

For best results with `exec-in-terminal`, enable eat's shell integration. This provides reliable command completion detection.

**For Bash**, add to your `.bashrc`:
```bash
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"
```

**For Zsh**, add to your `.zshrc`:
```bash
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
```

Shell integration provides:
- Reliable prompt detection (no heuristics needed)
- Directory tracking
- Shell prompt navigation (`C-c C-p`/`C-c C-n`)
- Command status indicators (success/failure markers)

## Common Workflows

### 1. Understanding User Context

When a user asks for help, first understand what they're working on:

```bash
# See what buffers are open
claudemacs-cli list-buffers

# Get info about the file they're editing
claudemacs-cli buffer-info "main.py"

# Read the content
claudemacs-cli get-buffer-content "main.py"
```

### 2. Writing Code to a Buffer

Instead of just showing code, you can write it directly:

```bash
# Read current content first
current=$(claudemacs-cli get-buffer-content "myfile.py")

# Use your normal file editing tools to create new version
echo "$current" > temp.py
# ... edit temp.py ...

# Get info to find where to insert
info=$(claudemacs-cli buffer-info "myfile.py")

# Replace the whole buffer (using positions from buffer-info)
claudemacs-cli replace-region "myfile.py" 1 1000 "$(cat temp.py)"
```

### 3. Interactive REPL Development

Work with the user's active REPL sessions:

```bash
# Send commands to a Python REPL
claudemacs-cli send-input "*Python*" "import pandas as pd"
claudemacs-cli send-input "*Python*" "df = pd.read_csv('data.csv')"
claudemacs-cli send-input "*Python*" "df.head()"

# The user can see the output in their Emacs REPL buffer
```

### 4. Leaving Notes and Documentation

Use scratch buffer for temporary notes:

```bash
claudemacs-cli insert-in-buffer "*scratch*" "
;; Claude's Analysis Notes
;; Generated: $(date)
;;
;; Found 3 potential issues:
;; 1. Missing error handling in line 45
;; 2. Unused import on line 3
;; 3. Consider adding type hints
"
```

### 5. Multi-file Refactoring

```bash
# List all Python files
for file in $(claudemacs-cli list-buffers | grep "\.py$"); do
    echo "Checking $file..."
    claudemacs-cli get-buffer-content "$file" | grep "TODO"
done
```

## Best Practices

### Do's ✓

- **Always check if buffers exist** before operating on them
- **Ask the user** before making destructive changes to their files
- **Use buffer-info** to understand the file before modifying
- **Read before writing** - understand context first
- **Test with scratch buffer** when trying something new
- **Provide clear feedback** about what you're doing with their buffers
- **Handle errors gracefully** - check command exit codes

### Don'ts ✗

- **Don't assume buffer names** - always list or ask
- **Don't modify without reading** - understand the code first
- **Don't ignore permissions** - each command may require user approval
- **Don't overwrite without backup** - read content first
- **Don't forget quoting** - buffer names with special chars need quotes
- **Don't use 0-based positions** - Emacs uses 1-based indexing

## Troubleshooting

### Command fails with "Buffer does not exist"
```bash
# Verify buffer name with list-buffers
claudemacs-cli list-buffers | grep -i "myfile"
```

### Can't find the right buffer
```bash
# List all buffers and their files
for buf in $(claudemacs-cli list-buffers); do
    echo "Buffer: $buf"
    claudemacs-cli buffer-info "$buf" 2>/dev/null || true
done
```

### Position/region errors
```bash
# Get buffer info to see valid ranges
claudemacs-cli buffer-info "myfile.py"
# Look at :point-min and :point-max values
```

## Integration with Claude Code Tools

Remember, you also have Claude Code's standard tools available:
- Use `Read` tool for reading files from disk
- Use `Edit` tool for making precise edits to files
- Use `Bash` tool for file system operations
- Use `claudemacs-cli` when you want to interact with **open Emacs buffers**

The key difference:
- File tools (Read/Edit/Write) work with files on disk
- claudemacs-cli works with live Emacs buffers (which may have unsaved changes)

## Example: Complete Workflow

```bash
# 1. User asks: "Help me debug my Python script"

# 2. Understand the context
echo "Let me see what you're working on..."
claudemacs-cli list-buffers | grep "\.py$"

# 3. Read the main file
echo "Reading main.py..."
claudemacs-cli get-buffer-content "main.py" > current_code.py

# 4. Analyze with your tools
python -m pylint current_code.py

# 5. Write findings to scratch for user to see
claudemacs-cli insert-in-buffer "*scratch*" "
;; Pylint Results for main.py
;; $(date)
$(python -m pylint current_code.py 2>&1)
"

# 6. Offer to fix issues or explain them
echo "I found 3 issues. Would you like me to fix them?"
```
