# Project-Specific Tools for Claudemacs

This feature allows you to define project-specific MCP tools that are automatically loaded when claudemacs runs in your project directory.

## How It Works

1. **Automatic Loading**: Place tools in `.claude/claudemacs-tools.yaml` - they load automatically!
2. **Custom Locations**: Optionally use `.dir-locals.el` to specify additional tools files
3. **Tool Merging**: Custom tools are merged with built-in tools (later definitions override)

## Quick Start (Recommended)

The simplest way is to use the default location that loads automatically:

```bash
# In your project root:
mkdir -p .claude
cat > .claude/claudemacs-tools.yaml << 'EOF'
tools:
  my_tool:
    description: "My custom tool"
    safe: true
    elisp: [message, "Hello from {{name}}!"]
    args:
      name:
        type: string
        description: "Your name"
        required: true
EOF
```

That's it! Start claudemacs in this directory and your tools are available.

## Alternative Setup: Custom Locations

If you prefer a different location or need multiple tools files:

### Step 1: Create Your Tools File

Create `my-tools.yaml` wherever you prefer:

```yaml
tools:
  my_custom_tool:
    description: "Description of what your tool does"
    safe: true  # or false if it modifies state
    elisp: function-name  # Simple function name
    args:
      arg_name:
        type: string  # string, integer, boolean, array
        description: "What this argument does"
        required: true  # or false for optional args
```

### Step 2: Configure .dir-locals.el

Add to your project's `.dir-locals.el`:

```elisp
;; Simple version - direct path
((nil . ((claudemacs-additional-tools-files .
          ("/path/to/your/project/my-tools.yaml")))))

;; Or dynamic version - finds tools relative to .dir-locals.el
((nil . ((eval . (progn
                   (when (boundp 'claudemacs-additional-tools-files)
                     (add-to-list 'claudemacs-additional-tools-files
                                  (expand-file-name "my-tools.yaml"
                                                    (locate-dominating-file
                                                     default-directory ".dir-locals.el"))
                                  t)))))))
```

### Step 3: Restart Claudemacs

Tools are loaded when the MCP server starts. After adding/modifying tools:
- Use `mcp__claudemacs__restart_and_resume` to reload
- Or start a fresh claudemacs session

## Tool Definition Formats

Claudemacs supports multiple formats for defining the `elisp` field:

### Format 1: Simple Function Name
```yaml
elisp: message  # Just the function name
```

### Format 2: List Format (Recommended for readability)
```yaml
elisp:
  - message
  - "Hello %s from %s"
  - "{{name}}"
  - "{{location}}"
```

### Format 3: Template String
```yaml
elisp: "(message \"Hello {{name}} from {{location}}\")"
```

### Format 4: Nested Lists for Complex Operations
```yaml
elisp:
  - progn
  - [message, "Starting..."]
  - [sit-for, 1]
  - [message, "Processing {{item}}"]
  - [message, "Done!"]
```

### Tool Properties

- **name**: The tool identifier (used to invoke it)
- **description**: What the tool does
- **safe**: Boolean indicating if the tool is read-only (true) or modifies state (false)
- **elisp**: The Elisp expression (see formats above)
- **args**: Dictionary of arguments the tool accepts

### Argument Types

- `string`: Text input (becomes quoted string in elisp)
- `integer`: Numeric input (becomes number in elisp)
- `boolean`: True/false value (becomes t/nil in elisp)
- `array`: List of values (becomes elisp list)

## Examples

### Project Build Tool

```yaml
tools:
  build_project:
    description: "Build the project using make"
    safe: false
    elisp: compile
    args:
      command:
        type: string
        description: "Build command"
        required: true
```

### Custom Formatter

```yaml
tools:
  format_with_prettier:
    description: "Format current buffer with prettier"
    safe: false
    elisp: shell-command-on-region
    args:
      start:
        type: integer
        description: "Region start"
        required: true
      end:
        type: integer
        description: "Region end"
        required: true
      command:
        type: string
        description: "Prettier command"
        required: true
```

### Project-Specific Evaluator

```yaml
tools:
  eval_project_code:
    description: "Evaluate code in project context"
    safe: true
    elisp: eval
    args:
      expression:
        type: string
        description: "Elisp expression to evaluate"
        required: true
```

## Context-Aware Execution

Tools can automatically execute in the appropriate buffer/file/directory context based on their arguments!

### Context Modes

Set the `context` field in your tool definition:

- **`auto`** (default): Automatically detect context from arguments
- **`file`**: Always run in a file buffer context
- **`buffer`**: Always run in a specific buffer
- **`dir`**: Always run with a specific directory
- **`none`**: Disable context detection, run in server buffer

### Automatic Context Detection

When `context: auto` (or omitted), the MCP server automatically detects context from these arguments:

**File Context** (highest priority):
- `file_path`, `file`, `path`, `filename`, `source`, `target`

**Buffer Context**:
- `buffer_name`, `buffer`, `buf`

**Directory Context**:
- `directory`, `dir`, `folder`, `project_dir`, `work_dir`

### Examples

```yaml
tools:
  # Automatically runs in the file's buffer
  analyze_file:
    description: "Analyze a file"
    context: auto  # Optional, this is the default
    elisp: [list, :mode, major-mode, :size, [buffer-size]]
    args:
      file_path:  # This triggers file context!
        type: string
        description: "File to analyze"
        required: true

  # Always runs in directory context
  build_project:
    description: "Build in a directory"
    context: dir
    elisp: [compile, "make"]
    args:
      directory:
        type: string
        description: "Build directory"
        required: true

  # Disable context (runs in server buffer)
  global_stats:
    description: "Get global Emacs stats"
    context: none
    elisp: [list, :buffers, [length, [buffer-list]]]
    args: {}
```

### Manual Context Override

You can always override context with special parameters:

```python
# Force execution in a specific file
tool_name(arg="value", __file="/path/to/file.el")

# Force execution in a specific buffer
tool_name(arg="value", __buffer="*scratch*")

# Force execution with specific directory
tool_name(arg="value", __dir="/project/path")
```

## Variable Substitution

Use `{{variable}}` or `$variable` syntax in your elisp:

```yaml
elisp: "(message \"File: {{filename}} in {{directory}}\")"
# or
elisp: [message, "File: $filename in $directory"]
```

## Limitations

- Tools are loaded once at MCP server startup
- Tool names must be unique (later definitions override earlier ones)
- The elisp function must already exist in Emacs
- Context detection requires well-named arguments

## Security Considerations

- Mark tools as `safe: false` if they modify files or state
- Be careful with `eval` - it can execute arbitrary code
- Tools from additional files have the same permissions as built-in tools
- Context execution means tools have access to buffer-local variables

## Troubleshooting

1. **Tools not appearing**: Check that the file path in `.dir-locals.el` is correct
2. **Wrong context**: Check argument names match expected patterns or use explicit `context` setting
3. **Tool errors**: Verify the elisp function exists and accepts the arguments
4. **Loading issues**: Check the MCP server stderr for YAML parsing errors
5. **Context not working**: Use `__file`, `__buffer`, or `__dir` to force context

## Integration with CI/CD

You can use dir-local tools for CI/CD integration:

```yaml
tools:
  ci_status:
    description: "Check CI pipeline status"
    safe: true
    elisp: shell-command-to-string
    args:
      command:
        type: string
        description: "Command to check CI status"
        required: true

  deploy_staging:
    description: "Deploy to staging environment"
    safe: false
    elisp: async-shell-command
    args:
      command:
        type: string
        description: "Deployment command"
        required: true
```

This allows Claude to interact with your project's infrastructure while keeping the tools project-specific.