"""Core library for Emacs interaction via emacsclient."""

import os
import subprocess
from typing import Optional


def get_session_cwd() -> Optional[str]:
    """Get the claudemacs session's working directory.

    This is set by claudemacs.el via the CLAUDEMACS_CWD env var in the MCP config.
    Returns None if not set.
    """
    return os.environ.get('CLAUDEMACS_CWD')


def get_socket_path() -> str:
    """Determine the Emacs server socket path.

    Checks in order:
    1. CLAUDEMACS_SOCKET env var (set by claudemacs.el)
    2. EMACS_SERVER_FILE env var
    3. XDG runtime dir: /run/user/$UID/emacs/server
    4. Legacy: ~/.emacs.d/server/server
    """
    if socket := os.environ.get('CLAUDEMACS_SOCKET'):
        return socket
    if server_file := os.environ.get('EMACS_SERVER_FILE'):
        return server_file

    # Try XDG runtime directory (common on Linux)
    xdg_runtime = os.environ.get('XDG_RUNTIME_DIR')
    if xdg_runtime:
        xdg_socket = os.path.join(xdg_runtime, 'emacs', 'server')
        if os.path.exists(xdg_socket):
            return xdg_socket

    # Try /run/user/$UID/emacs/server
    uid = os.getuid()
    run_socket = f'/run/user/{uid}/emacs/server'
    if os.path.exists(run_socket):
        return run_socket

    # Legacy fallback
    emacs_dir = os.path.expanduser('~/.emacs.d')
    return os.path.join(emacs_dir, 'server', 'server')


def call_emacs(elisp_expr: str, socket: Optional[str] = None, timeout: int = 30) -> str:
    """Call emacsclient with an elisp expression.

    Args:
        elisp_expr: The elisp expression to evaluate
        socket: Optional socket path (defaults to get_socket_path())
        timeout: Timeout in seconds (default 30)

    Returns:
        The result of the evaluation as a string

    Raises:
        RuntimeError: If emacsclient returns an error
    """
    if socket is None:
        socket = get_socket_path()

    cmd = ['emacsclient', '--socket-name', socket, '--eval', elisp_expr]

    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        timeout=timeout
    )

    if result.returncode != 0:
        raise RuntimeError(f"Emacs error: {result.stderr}")

    return result.stdout.strip()


def unescape_elisp_string(s: str) -> str:
    """Unescape an elisp string result.

    Removes surrounding quotes and unescapes special characters.
    """
    if s.startswith('"') and s.endswith('"'):
        s = s[1:-1]
    return s.replace('\\n', '\n').replace('\\"', '"').replace('\\\\', '\\')
