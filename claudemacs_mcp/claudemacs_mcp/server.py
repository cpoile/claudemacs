#!/usr/bin/env python3
"""Claudemacs MCP Server - Expose Emacs operations to Claude via MCP.

Tools are defined in tools.yaml and dynamically loaded at startup.
"""

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
    """List available Emacs interaction tools from YAML definitions."""
    # Reload tools on each list_tools call to pick up changes
    load_tools()

    tools = []
    for name, tool_def in TOOL_DEFS.items():
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


def is_memory_tool(name: str) -> bool:
    """Check if a tool operates on the memory buffer."""
    return name.startswith("memory_") or name in {
        "get_memory", "set_memory", "append_memory", "clear_memory"
    }


def wrap_with_cwd(elisp_expr: str, cwd: str) -> str:
    """Wrap an elisp expression with a let binding for claudemacs-session-cwd."""
    escaped_cwd = escape_elisp_string(cwd)
    return f'(let ((claudemacs-session-cwd "{escaped_cwd}")) {elisp_expr})'


@app.call_tool()
async def call_tool(name: str, arguments: dict) -> list[TextContent]:
    """Handle tool calls by invoking the corresponding elisp function."""
    try:
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

        # For memory tools, wrap with session cwd binding
        session_cwd = lib.get_session_cwd()
        if session_cwd and is_memory_tool(name):
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
    """Return list of tool names marked as safe in the YAML."""
    load_tools()
    return [name for name, defn in TOOL_DEFS.items() if defn.get("safe", False)]


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
