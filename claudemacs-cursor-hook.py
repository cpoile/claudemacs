#!/usr/bin/env python3
"""Queue an afterAgentResponse preview for the originating Claudemacs session."""

import json
import os
from pathlib import Path
import sys
import tempfile
import time


def main():
    directory = os.environ.get("CLAUDEMACS_CURSOR_PREVIEW_DIR")
    if not directory:
        return

    event = json.load(sys.stdin.buffer)
    reply = event.get("text", "")
    if not isinstance(reply, str):
        return

    # Publish only after the whole preview has been written. Unique names let
    # consecutive replies queue up without replacing one another.
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", dir=directory, prefix=".reply-", delete=False
        ) as output:
            temporary = Path(output.name)
            output.write(reply.strip()[:512])
        destination = temporary.with_name(f"reply-{time.time_ns():020}-{temporary.name[7:]}.txt")
        temporary.replace(destination)
    except FileNotFoundError:
        # The Emacs session may have closed while the hook finished.
        pass
    finally:
        if temporary is not None:
            temporary.unlink(missing_ok=True)


if __name__ == "__main__":
    main()
