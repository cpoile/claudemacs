#!/usr/bin/env python3
"""Update MCP config with current Emacs socket path."""

import json
import os
import subprocess
import sys
from pathlib import Path


def get_emacs_socket_path():
    """Get the Emacs server socket path using batch mode."""
    try:
        result = subprocess.run(
            [
                "emacs",
                "--batch",
                "--eval",
                "(progn (require 'server) (princ (expand-file-name server-name server-socket-dir)))",
            ],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0 and result.stdout.strip():
            socket_path = result.stdout.strip()
            # Verify the socket exists
            if os.path.exists(socket_path):
                return socket_path
            else:
                print(
                    f"Warning: Emacs reported socket path {socket_path} but file does not exist",
                    file=sys.stderr,
                )
                return None
        return None
    except (subprocess.TimeoutExpired, FileNotFoundError, Exception) as e:
        print(f"Error getting Emacs socket path: {e}", file=sys.stderr)
        return None


def update_mcp_config(config_path: Path, socket_path: str):
    """Update the MCP config file with the socket path."""
    # Get the directory containing the config
    mcp_dir = config_path.parent

    config = {
        "mcpServers": {
            "claudemacs": {
                "command": "uv",
                "args": ["run", "python", "-m", "claudemacs_mcp.server"],
                "cwd": str(mcp_dir),
                "env": {"CLAUDEMACS_SOCKET": socket_path},
            }
        }
    }

    with open(config_path, "w") as f:
        json.dump(config, f, indent=2)

    return config


def main():
    """Main entry point."""
    # Get the config path (same directory as this script)
    script_dir = Path(__file__).parent
    config_path = script_dir / "mcp-config.json"

    # Get the socket path
    socket_path = get_emacs_socket_path()

    if not socket_path:
        print(
            "Error: Could not determine Emacs socket path. Is Emacs server running?",
            file=sys.stderr,
        )
        sys.exit(1)

    # Update the config
    config = update_mcp_config(config_path, socket_path)

    print(f"Updated {config_path}")
    print(f"Socket path: {socket_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
