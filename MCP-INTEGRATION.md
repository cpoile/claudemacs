# Claudemacs MCP (Model Context Protocol) Integration

The MCP integration provides Claude with direct access to Emacs operations through native tools, enabling powerful buffer manipulation, file operations, and elisp execution.

## Overview

The MCP server (`claudemacs_mcp`) exposes Emacs functionality as tools that Claude can call directly. This provides:

- **Direct buffer access**: Read, search, and modify buffers
- **Elisp execution**: Run any elisp code with proper context
- **Project-specific tools**: Define custom tools per project
- **Context-aware execution**: Tools run in the appropriate buffer/file/directory
- **Multi-agent support**: Spawn and coordinate multiple Claude agents

## Core Features

### 1. Buffer Operations

Tools for working with Emacs buffers:

- `get_buffer_content`: Read buffer contents (with line ranges)
- `list_buffers`: List all open buffers
- `buffer_info`: Get buffer metadata (file path, mode, size, etc.)
- `search_buffer`: Search with regex and context lines
- `clear_buffer`: Clear terminal buffers for performance

### 2. Elisp Execution

- `eval_elisp`: Execute arbitrary elisp expressions
- Automatic context detection from arguments
- Buffer-local variables and modes accessible
- Proper `default-directory` handling

### 3. Multi-Agent System

- `spawn_agent`: Create new Claude agents in different directories
- `message_agent`: Send messages between agents
- `list_agents`: See all running agents
- `check_messages`: Check queued messages for an agent

### 4. Project Notes (Org-Mode)

Persistent notes in `.claude/claudemacs-notes.org`:

- `notes_add_heading`: Add org headings
- `notes_add_todo`: Create TODO items
- `notes_set_property`: Set org properties
- `notes_clock_in/out`: Time tracking

## Context-Aware Execution

### The Problem It Solves

Previously, all MCP commands ran in the `*server*` buffer, causing:
- Wrong working directory
- No access to buffer-local variables
- Missing minor modes
- Failed file operations

### How Context Works

Tools can run in three contexts:

1. **File Context**: Opens file and runs in its buffer
2. **Buffer Context**: Runs in a specific existing buffer
3. **Directory Context**: Sets `default-directory` for the operation

### Automatic Detection

The MCP server automatically detects context from argument names:

```yaml
tools:
  my_tool:
    elisp: [find-file, "{{file_path}}"]
    args:
      file_path:  # Automatically triggers file context!
        type: string
```

Common patterns:
- `file_path`, `file`, `filename` → File context
- `buffer_name`, `buffer` → Buffer context
- `directory`, `dir` → Directory context

### Manual Override

Use special parameters to force context:

```python
mcp__claudemacs__eval_elisp(
    expression="major-mode",
    __file="/path/to/file.el"  # Force file context
)
```

## Project-Specific Tools

### Quick Setup

1. Create `.claude/claudemacs-tools.yaml` in your project:

```yaml
tools:
  project_build:
    description: "Build the project"
    safe: false
    context: dir  # Auto-set directory context
    elisp: [compile, "make"]
    args:
      directory:
        type: string
        description: "Build directory"
        required: true
```

2. That's it! Tools load automatically when claudemacs starts.

### Tool Definition Formats

#### Format 1: Simple Function
```yaml
elisp: message
```

#### Format 2: List Format (Recommended)
```yaml
elisp:
  - message
  - "Hello %s"
  - "{{name}}"
```

#### Format 3: Template String
```yaml
elisp: "(message \"Hello {{name}}\")"
```

#### Format 4: Complex Operations
```yaml
elisp:
  - progn
  - [message, "Starting..."]
  - [sit-for, 1]
  - [compile, "{{command}}"]
```

### Variable Substitution

Use `{{var}}` or `$var` in templates:

```yaml
elisp: "(format \"User %s in %s\" {{username}} $directory)"
```

## Keybindings

In claudemacs buffers:

- **C-c s**: Spawn a new agent
- **C-c t**: Clear/trim buffer (for performance)
- **C-c C-e**: Transient menu for claudemacs commands

## Advanced Features

### Watch Functions (Async)

Non-blocking tools that poll for changes:

- `watch_buffer`: Wait for buffer to stabilize
- `watch_for_pattern`: Wait for regex to appear
- `watch_for_change`: Detect any buffer change

### Session Management

- `restart_and_resume`: Restart MCP server and continue
- `whoami`: Get current buffer identity
- Session state persisted in `.claude/` directory

### Bash Integration

- `bash`: Execute commands in eat terminal (visible in Emacs)
- Output appears in Emacs buffers
- Better than standard Bash tool for user visibility

## Configuration

### Custom Tool Locations

In `.dir-locals.el`:

```elisp
((nil . ((claudemacs-additional-tools-files .
          ("/path/to/my-tools.yaml")))))
```

### Context Hints

```yaml
tools:
  my_tool:
    context: file  # Always run in file context
    # or: buffer, dir, auto, none
```

## Security

- Tools marked `safe: false` can modify state
- Context execution has buffer-local access
- Tools can't escape Emacs sandbox
- Project tools have same permissions as built-in

## Troubleshooting

### Tools Not Loading
- Check `.claude/claudemacs-tools.yaml` exists
- Verify YAML syntax
- Restart MCP server after changes

### Wrong Context
- Check argument names match patterns
- Use explicit `context:` setting
- Force with `__file`, `__buffer`, `__dir`

### Performance Issues
- Use `clear_buffer` for long terminal sessions
- Watch functions are async (non-blocking)
- File operations cache for efficiency

## Examples

### File Analysis Tool
```yaml
tools:
  analyze_code:
    description: "Analyze code file"
    safe: true
    context: auto
    elisp:
      - list
      - :file
      - "{{file_path}}"
      - :mode
      - major-mode
      - :functions
      - [length, [imenu--make-index-alist]]
    args:
      file_path:
        type: string
        description: "File to analyze"
        required: true
```

### Project Builder
```yaml
tools:
  build_and_test:
    description: "Build and test project"
    safe: false
    context: dir
    elisp:
      - progn
      - [compile, "make clean && make"]
      - [sit-for, 2]
      - [compile, "make test"]
    args:
      dir:
        type: string
        description: "Project directory"
        required: true
```

### Buffer Switcher
```yaml
tools:
  smart_switch:
    description: "Switch to buffer by pattern"
    safe: true
    elisp:
      - switch-to-buffer
      - [completing-read, "Buffer: ", [mapcar, 'buffer-name, [buffer-list]]]
    args: {}
```

## Benefits

- **Zero configuration**: Default tools just work
- **Project isolation**: Each project has its own tools
- **Context awareness**: Tools run where they should
- **Elisp power**: Full Emacs API access
- **Async operations**: Non-blocking watch functions
- **Multi-agent**: Coordinate multiple Claude instances

The MCP integration makes Claude a true Emacs power user, able to navigate, analyze, and modify code with the full power of Emacs at its disposal!