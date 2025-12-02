#!/usr/bin/env python3
"""Claudemacs MCP Server - Expose Emacs operations to Claude via MCP.

Tools are defined in tools.yaml and dynamically loaded at startup.
Native async tools (like watch functions) are defined in Python.
"""

import json
import os
from pathlib import Path

import yaml
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent

from . import lib


# Create the MCP server
app = Server("claudemacs")

# Store the buffer identity for this MCP server instance
# This is set during server initialization from CLAUDEMACS_BUFFER_NAME env var
SESSION_BUFFER_NAME: str | None = None

# Load tool definitions from YAML
TOOLS_FILE = Path(__file__).parent.parent / "tools.yaml"
TOOL_DEFS: dict = {}

# Native Python tools (async, don't block Emacs)
NATIVE_TOOLS: dict = {
    "watch_buffer": {
        "description": "Watch a buffer until its content stabilizes (stops changing). Non-blocking async polling.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer to watch", "required": True},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
            "stable_time": {"type": "number", "description": "Seconds of no change before considered stable (default: 0.5)"},
        },
    },
    "watch_for_pattern": {
        "description": "Watch a buffer until a regex pattern appears. Non-blocking async polling. Returns match info or null on timeout.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer to watch", "required": True},
            "pattern": {"type": "string", "description": "Regex pattern to wait for", "required": True},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
        },
    },
    "watch_for_change": {
        "description": "Watch a buffer until any change occurs. Non-blocking async polling. Returns 'changed' or 'timeout'.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer to watch", "required": True},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
        },
    },
    "send_and_watch": {
        "description": "[EXECUTE] Send input to a buffer and wait for completion. Non-blocking async polling. Returns new content after input.",
        "safe": False,
        "args": {
            "buffer_name": {"type": "string", "description": "Name of the buffer", "required": True},
            "input": {"type": "string", "description": "Text/command to send", "required": True},
            "done_pattern": {"type": "string", "description": "Optional regex pattern that signals completion (otherwise waits for stability)"},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 30)"},
        },
    },
    "bash": {
        "description": "[EXECUTE] Execute a bash command in a project shell (eat terminal). Output is visible in Emacs. Returns output and exit code.",
        "safe": False,
        "args": {
            "command": {"type": "string", "description": "The bash command to execute", "required": True},
            "directory": {"type": "string", "description": "Working directory for the shell (defaults to session cwd)"},
            "timeout": {"type": "integer", "description": "Maximum seconds to wait (default: 120)"},
        },
    },
    "reload_file": {
        "description": "[EXECUTE] Reload one or more elisp files in Emacs. IMPORTANT: Files are loaded in the order provided - this matters when files have dependencies. If loading fails due to syntax errors, automatically checks for unbalanced parentheses using indentation-aware analysis on the first file that fails.",
        "safe": False,
        "args": {
            "file_path": {"type": "string", "description": "Path to a single elisp file to reload", "required": False},
            "file_paths": {"type": "array", "description": "Array of file paths to reload in order (use when loading multiple files)", "required": False},
        },
    },
    "spawn_agent": {
        "description": "Spawn a new Claude agent in a directory. Returns the buffer name for monitoring. Note: To send an initial message to the agent after spawning, use message_agent separately (TODO: MCP server will handle this automatically).",
        "safe": True,
        "args": {
            "directory": {"type": "string", "description": "Directory path where the agent should work (will be expanded)", "required": True},
            "agent_name": {"type": "string", "description": "Optional identifier for the agent (e.g., 'test', 'debug'). If not provided, buffer will be named *claudemacs:/path*. If provided, buffer will be *claudemacs:/path:agent-name*."},
        },
    },
    "list_agents": {
        "description": "List all running claudemacs agent sessions. Returns (buffer-name, directory) pairs.",
        "safe": True,
        "args": {},
    },
    "message_agent": {
        "description": "Send a message to another running agent. Messages are queued and can be checked by the recipient using check_messages.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Buffer name of the agent (from list_agents or spawn_agent)", "required": True},
            "message": {"type": "string", "description": "Message to send as user input to the agent", "required": True},
            "from_buffer": {"type": "string", "description": "Optional sender buffer name (auto-detected if not provided)"},
        },
    },
    "check_messages": {
        "description": "Check queued messages for an agent. Returns formatted messages with sender info and instructions on how to respond. Use this to check your inbox.",
        "safe": True,
        "args": {
            "buffer_name": {"type": "string", "description": "Buffer name of the agent to check messages for", "required": True},
            "clear": {"type": "boolean", "description": "Whether to clear messages after reading (default: false)"},
        },
    },
    "message_board_summary": {
        "description": "Get a summary of messages sent between agents. Shows message counts for each sender/recipient pair.",
        "safe": True,
        "args": {},
    },
    "whoami": {
        "description": "Get the buffer name/identity of the current claudemacs session. Use this to identify yourself when sending messages or logging actions.",
        "safe": True,
        "args": {},
    },
}


def load_tools() -> dict:
    """Load tool definitions from YAML file and any additional tools files."""
    global TOOL_DEFS

    # Load main tools file
    with open(TOOLS_FILE) as f:
        data = yaml.safe_load(f)
    TOOL_DEFS = data.get("tools", {})

    # Check for default .claude/claudemacs-tools.yaml in session directory
    session_cwd = os.environ.get("CLAUDEMACS_CWD")
    if session_cwd:
        default_tools_file = os.path.join(session_cwd, ".claude", "claudemacs-tools.yaml")
        if os.path.exists(default_tools_file):
            try:
                with open(default_tools_file) as f:
                    default_data = yaml.safe_load(f)
                default_tools = default_data.get("tools", {})
                # Merge default tools (these can be overridden by explicitly specified files)
                TOOL_DEFS.update(default_tools)
                print(f"Loaded project tools from {default_tools_file}", file=sys.stderr)
            except Exception as e:
                print(f"Warning: Failed to load default tools from {default_tools_file}: {e}", file=sys.stderr)

    # Load additional tools files from environment variable
    additional_files = os.environ.get("CLAUDEMACS_ADDITIONAL_TOOLS_FILES", "")
    if additional_files:
        for tools_file in additional_files.split(":"):
            tools_file = tools_file.strip()
            if tools_file and os.path.exists(tools_file):
                try:
                    with open(tools_file) as f:
                        additional_data = yaml.safe_load(f)
                    additional_tools = additional_data.get("tools", {})
                    # Merge additional tools into TOOL_DEFS
                    TOOL_DEFS.update(additional_tools)
                except Exception as e:
                    # Log but don't fail - continue with other tools
                    print(f"Warning: Failed to load additional tools from {tools_file}: {e}", file=sys.stderr)

    return TOOL_DEFS


def build_input_schema(tool_def: dict) -> dict:
    """Build JSON schema from tool definition."""
    args = tool_def.get("args", {})
    if not args:
        return {"type": "object", "properties": {}}

    properties = {}
    required = []

    for arg_name, arg_def in args.items():
        properties[arg_name] = {
            "type": arg_def.get("type", "string"),
            "description": arg_def.get("description", ""),
        }
        if arg_def.get("required", False):
            required.append(arg_name)

    schema = {"type": "object", "properties": properties}
    if required:
        schema["required"] = required
    return schema


@app.list_tools()
async def list_tools() -> list[Tool]:
    """List available Emacs interaction tools from YAML and native definitions."""
    # Reload tools on each list_tools call to pick up changes
    load_tools()

    tools = []
    # Add YAML-defined tools
    for name, tool_def in TOOL_DEFS.items():
        tools.append(Tool(
            name=name,
            description=tool_def.get("description", ""),
            inputSchema=build_input_schema(tool_def),
        ))
    # Add native Python tools
    for name, tool_def in NATIVE_TOOLS.items():
        tools.append(Tool(
            name=name,
            description=tool_def.get("description", ""),
            inputSchema=build_input_schema(tool_def),
        ))
    return tools


def escape_elisp_string(s: str) -> str:
    """Escape a string for use in elisp."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')


def substitute_variables(template: str, args: dict, arg_defs: dict) -> str:
    """Substitute variables in template string using {{var}} or $var syntax."""
    result = template

    for arg_name, value in args.items():
        arg_def = arg_defs.get(arg_name, {})
        arg_type = arg_def.get("type", "string")

        # Format value based on type
        if arg_type == "string":
            formatted_value = f'"{escape_elisp_string(str(value))}"'
        elif arg_type == "integer":
            formatted_value = str(int(value))
        elif arg_type == "boolean":
            formatted_value = "t" if value else "nil"
        elif arg_type == "array":
            # Handle array as elisp list
            if isinstance(value, list):
                items = [f'"{escape_elisp_string(str(v))}"' for v in value]
                formatted_value = f"'({' '.join(items)})"
            else:
                formatted_value = "'()"
        else:
            formatted_value = f'"{escape_elisp_string(str(value))}"'

        # Replace {{var}} and $var patterns
        result = result.replace(f"{{{{{arg_name}}}}}", formatted_value)
        result = result.replace(f"${arg_name}", formatted_value)

    return result


def build_elisp_from_spec(elisp_spec, args: dict, arg_defs: dict) -> str:
    """Build elisp expression from various spec formats."""
    if isinstance(elisp_spec, str):
        # Simple function name or template string
        if "{{" in elisp_spec or "$" in elisp_spec:
            # It's a template string with variables
            return substitute_variables(elisp_spec, args, arg_defs)
        else:
            # It's a simple function name, use old behavior
            return build_elisp_call(elisp_spec, args, arg_defs)

    elif isinstance(elisp_spec, list):
        # List format: ["progn", ["message", "{{msg}}"], ["sit-for", 1]]
        return build_elisp_from_list(elisp_spec, args, arg_defs)

    elif isinstance(elisp_spec, dict):
        # Dict format for more complex expressions
        if "template" in elisp_spec:
            return substitute_variables(elisp_spec["template"], args, arg_defs)
        elif "function" in elisp_spec:
            # Traditional function call
            return build_elisp_call(elisp_spec["function"], args, arg_defs)

    # Fallback to old behavior
    return build_elisp_call(str(elisp_spec), args, arg_defs)


def build_elisp_from_list(spec_list: list, args: dict, arg_defs: dict) -> str:
    """Build elisp from list specification."""
    if not spec_list:
        return "nil"

    result_parts = []
    for item in spec_list:
        if isinstance(item, str):
            # Check if it's a pure template variable (e.g., "{{item}}" or "$item")
            # These should be substituted without adding quotes
            is_pure_template = False
            if item.strip().startswith("{{") and item.strip().endswith("}}"):
                var_name = item.strip()[2:-2].strip()
                if var_name in args:
                    is_pure_template = True
                    # Substitute just the variable, quotes are already added by substitute_variables
                    result_parts.append(substitute_variables(item, args, arg_defs))
            elif item.strip().startswith("$"):
                var_name = item.strip()[1:]
                if var_name in args:
                    is_pure_template = True
                    result_parts.append(substitute_variables(item, args, arg_defs))

            if not is_pure_template:
                # Could be a function name, literal string, or template with mixed content
                if "{{" in item or "$" in item:
                    # Mixed template - needs to be handled as string literal
                    substituted = substitute_variables(item, args, arg_defs)
                    # If it doesn't start with a quote, it needs to be quoted
                    if not substituted.startswith('"'):
                        result_parts.append(f'"{escape_elisp_string(substituted)}"')
                    else:
                        result_parts.append(substituted)
                else:
                    # Plain string - could be function name or literal
                    # Check if it's a valid elisp symbol (function/variable name)
                    import re
                    # Valid symbols: alphanumeric, -, +, *, /, <, >, =, !, ?, etc.
                    # but NOT if it contains spaces, %, :, etc which indicate it's a string
                    if re.match(r'^[a-zA-Z_+\-*/<>=!?][a-zA-Z0-9_+\-*/<>=!?]*$', item):
                        # Looks like a symbol/function name, don't quote
                        result_parts.append(item)
                    else:
                        # It's a literal string, needs quotes
                        result_parts.append(f'"{escape_elisp_string(item)}"')
        elif isinstance(item, list):
            # Nested list
            result_parts.append(build_elisp_from_list(item, args, arg_defs))
        elif isinstance(item, (int, float)):
            result_parts.append(str(item))
        elif isinstance(item, bool):
            result_parts.append("t" if item else "nil")
        else:
            result_parts.append(str(item))

    return f"({' '.join(result_parts)})"


def build_elisp_call(elisp_fn: str, args: dict, arg_defs: dict) -> str:
    """Build an elisp function call from tool arguments (legacy format)."""
    if not arg_defs:
        return f"({elisp_fn})"

    # Build argument list in definition order
    # We need to handle optional args carefully - if a later arg is provided,
    # we need to pass nil for earlier optional args to maintain positional order
    elisp_args = []
    arg_names = list(arg_defs.keys())

    # Find the last provided argument
    last_provided_idx = -1
    for i, arg_name in enumerate(arg_names):
        if arg_name in args:
            last_provided_idx = i

    # Build args up to the last provided one
    for i, arg_name in enumerate(arg_names):
        if i > last_provided_idx:
            break

        arg_def = arg_defs[arg_name]

        if arg_name in args:
            value = args[arg_name]
            arg_type = arg_def.get("type", "string")

            if arg_type == "string":
                elisp_args.append(f'"{escape_elisp_string(str(value))}"')
            elif arg_type == "integer":
                elisp_args.append(str(int(value)))
            elif arg_type == "boolean":
                elisp_args.append("t" if value else "nil")
            else:
                elisp_args.append(f'"{escape_elisp_string(str(value))}"')
        else:
            # Optional arg not provided - pass nil to maintain positional order
            elisp_args.append("nil")

    if elisp_args:
        return f"({elisp_fn} {' '.join(elisp_args)})"
    return f"({elisp_fn})"


def needs_session_cwd(name: str) -> bool:
    """Check if a tool needs the session cwd binding."""
    # Notes tools need cwd to identify the correct notes file
    # restart_and_resume needs cwd to identify which session to restart
    return name.startswith("notes_") or name in {
        "get_notes", "set_notes", "append_notes", "clear_notes",
        "restart_and_resume"
    }


def wrap_with_context(elisp_expr: str, cwd: str | None = None, buffer_name: str | None = None, file_path: str | None = None) -> str:
    """Wrap an elisp expression with proper context (directory, buffer, or file).

    Args:
        elisp_expr: The elisp expression to evaluate
        cwd: Working directory to use (sets default-directory)
        buffer_name: Buffer to execute in (for buffer-local vars and modes)
        file_path: File to visit before execution (for file-specific modes)

    Returns:
        Wrapped elisp expression that executes in the proper context
    """
    if file_path:
        # Execute in the context of a file buffer
        escaped_file = escape_elisp_string(file_path)
        return f'''(with-current-buffer (find-file-noselect "{escaped_file}")
                     {elisp_expr})'''
    elif buffer_name:
        # Execute in the context of a specific buffer
        escaped_buffer = escape_elisp_string(buffer_name)
        # Check if buffer exists first
        return f'''(if (get-buffer "{escaped_buffer}")
                     (with-current-buffer "{escaped_buffer}"
                       {elisp_expr})
                     (error "Buffer %s does not exist" "{escaped_buffer}"))'''
    elif cwd:
        # Execute with a specific default-directory
        escaped_cwd = escape_elisp_string(cwd)
        return f'''(let ((default-directory "{escaped_cwd}")
                         (claudemacs-session-cwd "{escaped_cwd}"))
                     {elisp_expr})'''
    else:
        # No context wrapping needed
        return elisp_expr


def wrap_with_cwd(elisp_expr: str, cwd: str) -> str:
    """Legacy wrapper - now calls wrap_with_context for backward compatibility."""
    return wrap_with_context(elisp_expr, cwd=cwd)


async def handle_native_tool(name: str, arguments: dict) -> str:
    """Handle native Python tools (async, non-blocking)."""
    if name == "watch_buffer":
        buffer_name = arguments["buffer_name"]
        timeout = float(arguments.get("timeout", 30))
        stable_time = float(arguments.get("stable_time", 0.5))
        result = await lib.watch_buffer_async(buffer_name, timeout, stable_time)
        return result

    elif name == "watch_for_pattern":
        buffer_name = arguments["buffer_name"]
        pattern = arguments["pattern"]
        timeout = float(arguments.get("timeout", 30))
        watch_result = await lib.watch_for_pattern_async(buffer_name, pattern, timeout)
        if watch_result:
            return json.dumps(watch_result)
        return "null (timeout - pattern not found)"

    elif name == "watch_for_change":
        buffer_name = arguments["buffer_name"]
        timeout = float(arguments.get("timeout", 30))
        result = await lib.watch_for_change_async(buffer_name, timeout)
        return result

    elif name == "send_and_watch":
        buffer_name = arguments["buffer_name"]
        input_text = arguments["input"]
        done_pattern = arguments.get("done_pattern")
        timeout = float(arguments.get("timeout", 30))
        result = await lib.send_and_watch_async(buffer_name, input_text, done_pattern, timeout)
        return result

    elif name == "bash":
        command = arguments["command"]
        # Use provided directory or fall back to session cwd
        directory = arguments.get("directory") or lib.get_session_cwd()
        if not directory:
            raise ValueError("No directory specified and CLAUDEMACS_CWD not set")
        timeout = float(arguments.get("timeout", 120))
        bash_result = await lib.bash_async(command, directory, timeout)
        # Format output similar to Claude Code's Bash tool
        output = bash_result['output']
        exit_code = bash_result['exit_code']
        buffer_name = bash_result['buffer_name']
        if exit_code == 0:
            return f"{output}\n\n[exit code: {exit_code}, shell: {buffer_name}]"
        else:
            return f"{output}\n\n[exit code: {exit_code}, shell: {buffer_name}] (command failed)"

    elif name == "reload_file":
        # Support both single file_path and multiple file_paths
        if "file_paths" in arguments:
            file_paths = arguments["file_paths"]
        elif "file_path" in arguments:
            file_paths = arguments["file_path"]
        else:
            raise ValueError("Either file_path or file_paths must be provided")
        result = await lib.reload_elisp_file(file_paths)
        return result

    elif name == "spawn_agent":
        directory = arguments["directory"]
        agent_name = arguments.get("agent_name")
        result = await lib.spawn_agent_async(directory, agent_name)
        return result

    elif name == "list_agents":
        agents_list = await lib.list_agents_async()
        return json.dumps(agents_list)

    elif name == "message_agent":
        buffer_name = arguments["buffer_name"]
        message = arguments["message"]
        # Auto-detect sender from session state if not provided
        from_buffer = arguments.get("from_buffer") or SESSION_BUFFER_NAME
        result = await lib.message_agent_async(buffer_name, message, from_buffer)
        return result

    elif name == "check_messages":
        buffer_name = arguments["buffer_name"]
        clear = arguments.get("clear", False)
        result = await lib.check_messages_async(buffer_name, clear)
        return result

    elif name == "message_board_summary":
        result = await lib.message_board_summary_async()
        return result

    elif name == "whoami":
        if not SESSION_BUFFER_NAME:
            raise ValueError("Buffer name not configured - CLAUDEMACS_BUFFER_NAME environment variable is not set")
        return SESSION_BUFFER_NAME

    else:
        raise ValueError(f"Unknown native tool: {name}")


@app.call_tool()
async def call_tool(name: str, arguments: dict) -> list[TextContent]:
    """Handle tool calls by invoking elisp or native Python functions."""
    try:
        # Check if it's a native Python tool
        if name in NATIVE_TOOLS:
            result = await handle_native_tool(name, arguments)
            return [TextContent(type="text", text=result)]

        # Otherwise, it's a YAML-defined elisp tool
        if name not in TOOL_DEFS:
            raise ValueError(f"Unknown tool: {name}")

        tool_def = TOOL_DEFS[name]
        elisp_fn = tool_def.get("elisp")

        if not elisp_fn:
            raise ValueError(f"Tool {name} has no elisp function defined")

        # Extract explicit context parameters first (these are special and not passed to elisp)
        context_buffer = arguments.pop("__buffer", None)
        context_file = arguments.pop("__file", None)
        context_dir = arguments.pop("__dir", None)

        # Infer context from tool arguments if not explicitly provided
        if not context_file and not context_buffer and not context_dir:
            # Check tool definition for context hints
            context_hint = tool_def.get("context", "auto")  # Default to auto

            # Always try to auto-detect unless explicitly disabled
            if context_hint != "none":
                # Look for file-related arguments (highest priority)
                if context_hint in ["file", "auto"]:
                    for arg_name in ["file_path", "file", "path", "filename", "source", "target"]:
                        if arg_name in arguments and arguments[arg_name]:
                            arg_val = arguments[arg_name]
                            # Check if it looks like a file path
                            if isinstance(arg_val, str) and ("." in arg_val or "/" in arg_val):
                                # More comprehensive file extension check
                                if os.path.splitext(arg_val)[1] or not arg_val.endswith("/"):
                                    context_file = arg_val
                                    break

                # Look for buffer-related arguments (second priority)
                if not context_file and context_hint in ["buffer", "auto"]:
                    for arg_name in ["buffer_name", "buffer", "buf"]:
                        if arg_name in arguments and arguments[arg_name]:
                            context_buffer = arguments[arg_name]
                            break

                # Look for directory-related arguments (third priority)
                if not context_file and not context_buffer and context_hint in ["dir", "auto"]:
                    for arg_name in ["directory", "dir", "folder", "project_dir", "work_dir"]:
                        if arg_name in arguments and arguments[arg_name]:
                            context_dir = arguments[arg_name]
                            break
                    # Also check if 'path' looks like a directory
                    if not context_dir and "path" in arguments:
                        path_val = arguments["path"]
                        if isinstance(path_val, str) and (path_val.endswith("/") or not "." in os.path.basename(path_val)):
                            context_dir = path_val

        # Special case for eval_elisp - pass expression directly
        if elisp_fn == "eval" and "expression" in arguments:
            elisp_expr = arguments["expression"]
        else:
            elisp_expr = build_elisp_from_spec(
                elisp_fn,
                arguments,
                tool_def.get("args", {})
            )

        # Determine the context to use
        session_cwd = lib.get_session_cwd()
        session_buffer = os.environ.get("CLAUDEMACS_BUFFER_NAME")

        # Apply context wrapping based on priority: file > buffer > dir > session
        if context_file:
            elisp_expr = wrap_with_context(elisp_expr, file_path=context_file)
        elif context_buffer:
            elisp_expr = wrap_with_context(elisp_expr, buffer_name=context_buffer)
        elif context_dir:
            elisp_expr = wrap_with_context(elisp_expr, cwd=context_dir)
        elif needs_session_cwd(name) and session_cwd:
            # For tools that need session context, use session defaults
            # Prefer executing in the claudemacs buffer if available
            if session_buffer and name != "eval_elisp":
                elisp_expr = wrap_with_context(elisp_expr, buffer_name=session_buffer)
            else:
                elisp_expr = wrap_with_context(elisp_expr, cwd=session_cwd)

        result = lib.call_emacs(elisp_expr)

        # Unescape string results
        if result.startswith('"') and result.endswith('"'):
            result = lib.unescape_elisp_string(result)

        return [TextContent(type="text", text=result)]

    except Exception as e:
        return [TextContent(type="text", text=f"Error: {str(e)}")]


async def main():
    """Run the MCP server."""
    global SESSION_BUFFER_NAME

    # Initialize session buffer name from environment
    SESSION_BUFFER_NAME = os.environ.get("CLAUDEMACS_BUFFER_NAME")

    # Load tools on startup
    load_tools()

    async with stdio_server() as (read_stream, write_stream):
        await app.run(
            read_stream,
            write_stream,
            app.create_initialization_options()
        )


def get_safe_tools() -> list[str]:
    """Return list of tool names marked as safe in the YAML and native tools."""
    load_tools()
    safe = [name for name, defn in TOOL_DEFS.items() if defn.get("safe", False)]
    safe.extend([name for name, defn in NATIVE_TOOLS.items() if defn.get("safe", False)])
    return safe


def print_safe_tools():
    """Print safe tool names, one per line. For use by elisp."""
    for tool in get_safe_tools():
        print(tool)


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == "--safe-tools":
        print_safe_tools()
    else:
        import asyncio
        asyncio.run(main())
