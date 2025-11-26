#!/usr/bin/env python3
"""Claudemacs MCP Server - Expose Emacs operations to Claude via MCP.

Tools are defined in tools.yaml and dynamically loaded at startup.
Native async tools (like watch functions) are defined in Python.
"""

import json
from pathlib import Path

import yaml
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent

from . import lib


# Create the MCP server
app = Server("claudemacs")

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
}


def load_tools() -> dict:
    """Load tool definitions from YAML file."""
    global TOOL_DEFS
    with open(TOOLS_FILE) as f:
        data = yaml.safe_load(f)
    TOOL_DEFS = data.get("tools", {})
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


def build_elisp_call(elisp_fn: str, args: dict, arg_defs: dict) -> str:
    """Build an elisp function call from tool arguments."""
    if not arg_defs:
        return f"({elisp_fn})"

    # Build argument list in definition order
    elisp_args = []
    for arg_name, arg_def in arg_defs.items():
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
        elif not arg_def.get("required", False):
            # Optional arg not provided - skip (elisp will use default)
            # But we need to stop here to avoid positional arg issues
            break

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


def wrap_with_cwd(elisp_expr: str, cwd: str) -> str:
    """Wrap an elisp expression with a let binding for claudemacs-session-cwd."""
    escaped_cwd = escape_elisp_string(cwd)
    return f'(let ((claudemacs-session-cwd "{escaped_cwd}")) {elisp_expr})'


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
        result = await lib.watch_for_pattern_async(buffer_name, pattern, timeout)
        if result:
            return json.dumps(result)
        return "null (timeout - pattern not found)"

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
        result = await lib.bash_async(command, directory, timeout)
        # Format output similar to Claude Code's Bash tool
        output = result['output']
        exit_code = result['exit_code']
        buffer_name = result['buffer_name']
        if exit_code == 0:
            return f"{output}\n\n[exit code: {exit_code}, shell: {buffer_name}]"
        else:
            return f"{output}\n\n[exit code: {exit_code}, shell: {buffer_name}] (command failed)"

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

        # Special case for eval_elisp - pass expression directly
        if elisp_fn == "eval" and "expression" in arguments:
            elisp_expr = arguments["expression"]
        else:
            elisp_expr = build_elisp_call(
                elisp_fn,
                arguments,
                tool_def.get("args", {})
            )

        # For tools that need session context, wrap with session cwd binding
        session_cwd = lib.get_session_cwd()
        if session_cwd and needs_session_cwd(name):
            elisp_expr = wrap_with_cwd(elisp_expr, session_cwd)

        result = lib.call_emacs(elisp_expr)

        # Unescape string results
        if result.startswith('"') and result.endswith('"'):
            result = lib.unescape_elisp_string(result)

        return [TextContent(type="text", text=result)]

    except Exception as e:
        return [TextContent(type="text", text=f"Error: {str(e)}")]


async def main():
    """Run the MCP server."""
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
