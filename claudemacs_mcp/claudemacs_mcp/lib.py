"""Shared library for Emacs interaction via emacsclient."""

import os
import subprocess
from typing import Optional


def get_socket_path() -> str:
    """Determine the Emacs server socket path."""
    if socket := os.environ.get('CLAUDEMACS_SOCKET'):
        return socket
    if server_file := os.environ.get('EMACS_SERVER_FILE'):
        return server_file
    emacs_dir = os.path.expanduser('~/.emacs.d')
    return os.path.join(emacs_dir, 'server', 'server')


def call_emacs(elisp_expr: str, socket: Optional[str] = None, timeout: int = 30) -> str:
    """Call emacsclient with an elisp expression."""
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


def escape_elisp_string(s: str) -> str:
    """Escape a string for use in elisp."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')


def unescape_elisp_string(s: str) -> str:
    """Unescape an elisp string."""
    if s.startswith('"') and s.endswith('"'):
        s = s[1:-1]
    return s.replace('\\n', '\n').replace('\\"', '"').replace('\\\\', '\\')


# High-level Emacs interaction functions

def get_buffer_content(buffer_name: str, tail_lines: Optional[int] = None, socket: Optional[str] = None) -> str:
    """Get content from an Emacs buffer."""
    if tail_lines:
        elisp = f'(claudemacs-ai-get-buffer-content "{buffer_name}" {tail_lines})'
    else:
        elisp = f'(claudemacs-ai-get-buffer-content "{buffer_name}")'
    
    result = call_emacs(elisp, socket)
    return unescape_elisp_string(result)


def list_buffers(socket: Optional[str] = None) -> str:
    """List all open buffers."""
    elisp = '(claudemacs-ai-list-buffers)'
    return call_emacs(elisp, socket)


def buffer_info(buffer_name: str, socket: Optional[str] = None) -> str:
    """Get detailed information about a buffer."""
    elisp = f'(claudemacs-ai-buffer-info "{buffer_name}")'
    return call_emacs(elisp, socket)


def get_region(buffer_name: str, start: int, end: int, socket: Optional[str] = None) -> str:
    """Get content from a specific region in a buffer."""
    elisp = f'(claudemacs-ai-get-region "{buffer_name}" {start} {end})'
    result = call_emacs(elisp, socket)
    return unescape_elisp_string(result)


def insert_in_buffer(buffer_name: str, text: str, socket: Optional[str] = None) -> str:
    """Insert text into a buffer."""
    escaped_text = escape_elisp_string(text)
    elisp = f'(claudemacs-ai-insert-in-buffer "{buffer_name}" "{escaped_text}")'
    return call_emacs(elisp, socket)


def replace_region(buffer_name: str, start: int, end: int, text: str, socket: Optional[str] = None) -> str:
    """Replace content in a region."""
    escaped_text = escape_elisp_string(text)
    elisp = f'(claudemacs-ai-replace-region "{buffer_name}" {start} {end} "{escaped_text}")'
    return call_emacs(elisp, socket)


def goto_point(buffer_name: str, position: int, socket: Optional[str] = None) -> str:
    """Move point to a position in buffer."""
    elisp = f'(claudemacs-ai-goto-point "{buffer_name}" {position})'
    return call_emacs(elisp, socket)


def send_input(buffer_name: str, text: str, socket: Optional[str] = None) -> str:
    """Send input to a REPL buffer."""
    escaped_text = escape_elisp_string(text)
    elisp = f'(claudemacs-ai-send-input "{buffer_name}" "{escaped_text}")'
    return call_emacs(elisp, socket)


def exec_in_terminal(buffer_name: str, command: str, timeout: int = 30, socket: Optional[str] = None) -> str:
    """Execute command in eat terminal and wait for completion."""
    escaped_cmd = escape_elisp_string(command)
    elisp = f'(claudemacs-ai-exec-in-eat-terminal "{buffer_name}" "{escaped_cmd}" {timeout})'
    result = call_emacs(elisp, socket, timeout=timeout + 10)
    return unescape_elisp_string(result)


def eval_elisp(expression: str, socket: Optional[str] = None) -> str:
    """Evaluate arbitrary elisp expression."""
    return call_emacs(expression, socket)


# Memory buffer operations

def get_memory(socket: Optional[str] = None) -> str:
    """Get the memory buffer content for the current session."""
    elisp = '(claudemacs-ai-get-memory)'
    result = call_emacs(elisp, socket)
    return unescape_elisp_string(result)


def set_memory(content: str, socket: Optional[str] = None) -> str:
    """Set the memory buffer content, replacing existing content."""
    escaped_content = escape_elisp_string(content)
    elisp = f'(claudemacs-ai-set-memory "{escaped_content}")'
    return call_emacs(elisp, socket)


def append_memory(content: str, socket: Optional[str] = None) -> str:
    """Append content to the memory buffer."""
    escaped_content = escape_elisp_string(content)
    elisp = f'(claudemacs-ai-append-memory "{escaped_content}")'
    return call_emacs(elisp, socket)


def clear_memory(socket: Optional[str] = None) -> str:
    """Clear the memory buffer."""
    elisp = '(claudemacs-ai-clear-memory)'
    return call_emacs(elisp, socket)


# Session management

def restart_and_resume(socket: Optional[str] = None) -> str:
    """Restart the claudemacs session and resume the conversation.
    This reloads the MCP server with any code changes."""
    elisp = '(claudemacs-ai-restart-and-resume)'
    return call_emacs(elisp, socket)
