#!/usr/bin/env python3
"""Pre-trust a directory in Claude Code's global config.

Usage: uv run pretrust-directory.py /path/to/directory

This adds an entry to ~/.claude.json with hasTrustDialogAccepted=true
so Claude Code won't prompt for trust when starting in that directory.
"""
import json
import sys
from pathlib import Path


def pretrust_directory(directory: str) -> bool:
    """Add a trusted directory entry to ~/.claude.json.

    Returns True if entry was added, False if already trusted.
    """
    claude_json_path = Path.home() / ".claude.json"
    expanded_path = str(Path(directory).expanduser().resolve())

    # Read existing config
    if claude_json_path.exists():
        with open(claude_json_path) as f:
            config = json.load(f)
    else:
        config = {}

    # Ensure projects dict exists
    if "projects" not in config:
        config["projects"] = {}

    # Check if already trusted
    if expanded_path in config["projects"]:
        existing = config["projects"][expanded_path]
        if existing.get("hasTrustDialogAccepted", False):
            print(f"Already trusted: {expanded_path}", file=sys.stderr)
            return False

    # Add trust entry
    config["projects"][expanded_path] = {
        "allowedTools": [],
        "mcpContextUris": [],
        "mcpServers": {},
        "enabledMcpjsonServers": [],
        "disabledMcpjsonServers": [],
        "hasTrustDialogAccepted": True,
        "projectOnboardingSeenCount": 0,
        "hasClaudeMdExternalIncludesApproved": False,
        "hasClaudeMdExternalIncludesWarningShown": False,
        "exampleFiles": []
    }

    # Write back
    with open(claude_json_path, "w") as f:
        json.dump(config, f, indent=2)

    print(f"Pre-trusted: {expanded_path}")
    return True


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} /path/to/directory", file=sys.stderr)
        sys.exit(1)

    directory = sys.argv[1]
    if not Path(directory).expanduser().exists():
        # Directory might not exist yet (worktree not created), that's OK
        pass

    success = pretrust_directory(directory)
    sys.exit(0 if success else 1)
