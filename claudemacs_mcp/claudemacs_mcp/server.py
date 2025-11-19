#!/usr/bin/env python3
"""Claudemacs MCP Server - Expose Emacs buffer operations to Claude via MCP."""

from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent

# Import shared library functions
from . import lib


# Create the MCP server
app = Server("claudemacs")


@app.list_tools()
async def list_tools() -> list[Tool]:
    """List available Emacs interaction tools."""
    return [
        Tool(
            name="get_buffer_content",
            description="Get the content of an Emacs buffer. Can optionally get only the last N lines using tail_lines parameter.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the buffer (e.g. 'main.py', '*scratch*')"
                    },
                    "tail_lines": {
                        "type": "integer",
                        "description": "Optional: get only the last N lines",
                    }
                },
                "required": ["buffer_name"]
            }
        ),
        Tool(
            name="list_buffers",
            description="List all open buffers in Emacs.",
            inputSchema={
                "type": "object",
                "properties": {}
            }
        ),
        Tool(
            name="buffer_info",
            description="Get detailed information about a buffer (file path, size, major mode, cursor position, etc.).",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the buffer"
                    }
                },
                "required": ["buffer_name"]
            }
        ),
        Tool(
            name="get_region",
            description="Get content from a specific region in a buffer by character positions.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the buffer"
                    },
                    "start": {
                        "type": "integer",
                        "description": "Start position (1-indexed)"
                    },
                    "end": {
                        "type": "integer",
                        "description": "End position (1-indexed)"
                    }
                },
                "required": ["buffer_name", "start", "end"]
            }
        ),
        Tool(
            name="insert_in_buffer",
            description="[WRITE] Insert text into a buffer at the current point position. This modifies the buffer content.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the buffer"
                    },
                    "text": {
                        "type": "string",
                        "description": "Text to insert"
                    }
                },
                "required": ["buffer_name", "text"]
            }
        ),
        Tool(
            name="replace_region",
            description="[WRITE] Replace content in a specific region of a buffer. This modifies the buffer content.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the buffer"
                    },
                    "start": {
                        "type": "integer",
                        "description": "Start position (1-indexed)"
                    },
                    "end": {
                        "type": "integer",
                        "description": "End position (1-indexed)"
                    },
                    "text": {
                        "type": "string",
                        "description": "Replacement text"
                    }
                },
                "required": ["buffer_name", "start", "end", "text"]
            }
        ),
        Tool(
            name="goto_point",
            description="[WRITE] Move the cursor (point) to a specific position in a buffer. This changes buffer state.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the buffer"
                    },
                    "position": {
                        "type": "integer",
                        "description": "Position to move to (1-indexed)"
                    }
                },
                "required": ["buffer_name", "position"]
            }
        ),
        Tool(
            name="send_input",
            description="[EXECUTE] Send input to a REPL buffer (works with eat, comint, eshell). The text will be inserted and executed. Use with caution as this runs code.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the REPL buffer (e.g. '*Python*', '*eshell*')"
                    },
                    "text": {
                        "type": "string",
                        "description": "Text/command to send"
                    }
                },
                "required": ["buffer_name", "text"]
            }
        ),
        Tool(
            name="exec_in_terminal",
            description="[EXECUTE] Execute a shell command in an eat terminal buffer and wait for it to complete, returning the output. Use with caution as this runs arbitrary commands.",
            inputSchema={
                "type": "object",
                "properties": {
                    "buffer_name": {
                        "type": "string",
                        "description": "Name of the eat terminal buffer"
                    },
                    "command": {
                        "type": "string",
                        "description": "Shell command to execute"
                    },
                    "timeout": {
                        "type": "integer",
                        "description": "Timeout in seconds (default: 30)"
                    }
                },
                "required": ["buffer_name", "command"]
            }
        ),
    ]


@app.call_tool()
async def call_tool(name: str, arguments: dict) -> list[TextContent]:
    """Handle tool calls for Emacs operations."""

    try:
        if name == "get_buffer_content":
            buffer_name = arguments["buffer_name"]
            tail_lines = arguments.get("tail_lines")
            content = lib.get_buffer_content(buffer_name, tail_lines)
            return [TextContent(type="text", text=content)]

        elif name == "list_buffers":
            result = lib.list_buffers()
            return [TextContent(type="text", text=result)]

        elif name == "buffer_info":
            buffer_name = arguments["buffer_name"]
            result = lib.buffer_info(buffer_name)
            return [TextContent(type="text", text=result)]

        elif name == "get_region":
            buffer_name = arguments["buffer_name"]
            start = arguments["start"]
            end = arguments["end"]
            content = lib.get_region(buffer_name, start, end)
            return [TextContent(type="text", text=content)]

        elif name == "insert_in_buffer":
            buffer_name = arguments["buffer_name"]
            text = arguments["text"]
            result = lib.insert_in_buffer(buffer_name, text)
            return [TextContent(type="text", text=result)]

        elif name == "replace_region":
            buffer_name = arguments["buffer_name"]
            start = arguments["start"]
            end = arguments["end"]
            text = arguments["text"]
            result = lib.replace_region(buffer_name, start, end, text)
            return [TextContent(type="text", text=result)]

        elif name == "goto_point":
            buffer_name = arguments["buffer_name"]
            position = arguments["position"]
            result = lib.goto_point(buffer_name, position)
            return [TextContent(type="text", text=result)]

        elif name == "send_input":
            buffer_name = arguments["buffer_name"]
            text = arguments["text"]
            result = lib.send_input(buffer_name, text)
            return [TextContent(type="text", text=result)]

        elif name == "exec_in_terminal":
            buffer_name = arguments["buffer_name"]
            command = arguments["command"]
            timeout = arguments.get("timeout", 30)
            result = lib.exec_in_terminal(buffer_name, command, timeout)
            return [TextContent(type="text", text=result)]

        else:
            raise ValueError(f"Unknown tool: {name}")

    except Exception as e:
        return [TextContent(type="text", text=f"Error: {str(e)}")]


async def main():
    """Run the MCP server."""
    async with stdio_server() as (read_stream, write_stream):
        await app.run(
            read_stream,
            write_stream,
            app.create_initialization_options()
        )


if __name__ == "__main__":
    import asyncio
    asyncio.run(main())
