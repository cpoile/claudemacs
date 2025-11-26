"""Core library for Emacs interaction via emacsclient."""

import asyncio
import os
import re
import subprocess
import time
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


async def call_emacs_async(elisp_expr: str, socket: Optional[str] = None, timeout: int = 30) -> str:
    """Async version of call_emacs using asyncio subprocess.

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

    proc = await asyncio.create_subprocess_exec(
        *cmd,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE
    )

    try:
        stdout, stderr = await asyncio.wait_for(proc.communicate(), timeout=timeout)
    except asyncio.TimeoutError:
        proc.kill()
        raise RuntimeError(f"Emacs call timed out after {timeout}s")

    if proc.returncode != 0:
        raise RuntimeError(f"Emacs error: {stderr.decode()}")

    return stdout.decode().strip()


def escape_elisp_string(s: str) -> str:
    """Escape a string for use in elisp."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')


async def get_buffer_content_async(buffer_name: str, tail_lines: Optional[int] = None) -> str:
    """Async helper to get buffer content from Emacs."""
    escaped_name = escape_elisp_string(buffer_name)
    if tail_lines:
        elisp = f'(claudemacs-ai-get-buffer-content "{escaped_name}" {tail_lines})'
    else:
        elisp = f'(claudemacs-ai-get-buffer-content "{escaped_name}")'

    result = await call_emacs_async(elisp)
    if result.startswith('"') and result.endswith('"'):
        return unescape_elisp_string(result)
    return result


async def watch_buffer_async(
    buffer_name: str,
    timeout: float = 30.0,
    stable_time: float = 0.5,
    poll_interval: float = 0.2
) -> str:
    """Watch a buffer until content stabilizes.

    Polls Emacs asynchronously without blocking.

    Args:
        buffer_name: Name of the buffer to watch
        timeout: Maximum seconds to wait
        stable_time: Seconds of no change before considered stable
        poll_interval: Seconds between polls

    Returns:
        Buffer content when stable or at timeout
    """
    start_time = time.time()
    last_content = ""
    last_change_time = time.time()

    while (time.time() - start_time) < timeout:
        current_content = await get_buffer_content_async(buffer_name)

        if current_content == last_content:
            # Content stable - check if stable long enough
            if (time.time() - last_change_time) >= stable_time:
                return current_content
        else:
            # Content changed - reset timer
            last_content = current_content
            last_change_time = time.time()

        await asyncio.sleep(poll_interval)

    # Timeout - return current content
    return await get_buffer_content_async(buffer_name)


async def watch_for_pattern_async(
    buffer_name: str,
    pattern: str,
    timeout: float = 30.0,
    poll_interval: float = 0.2
) -> Optional[dict]:
    """Watch a buffer until a regex pattern appears.

    Polls Emacs asynchronously without blocking.

    Args:
        buffer_name: Name of the buffer to watch
        pattern: Regex pattern to search for
        timeout: Maximum seconds to wait
        poll_interval: Seconds between polls

    Returns:
        Dict with 'match', 'pos' if found, None if timeout
    """
    start_time = time.time()
    compiled_pattern = re.compile(pattern)

    while (time.time() - start_time) < timeout:
        content = await get_buffer_content_async(buffer_name)

        match = compiled_pattern.search(content)
        if match:
            # Find the line containing the match
            lines = content[:match.start()].split('\n')
            line_num = len(lines)
            line_content = content.split('\n')[line_num - 1] if line_num > 0 else ""

            return {
                'match': match.group(0),
                'pos': match.start(),
                'line': line_content,
                'line_num': line_num
            }

        await asyncio.sleep(poll_interval)

    return None


async def send_input_async(buffer_name: str, text: str) -> str:
    """Send input to a buffer asynchronously."""
    escaped_name = escape_elisp_string(buffer_name)
    escaped_text = escape_elisp_string(text)
    elisp = f'(claudemacs-ai-send-input "{escaped_name}" "{escaped_text}")'
    result = await call_emacs_async(elisp)
    if result.startswith('"') and result.endswith('"'):
        return unescape_elisp_string(result)
    return result


async def send_and_watch_async(
    buffer_name: str,
    input_text: str,
    done_pattern: Optional[str] = None,
    timeout: float = 30.0,
    stable_time: float = 1.0,
    poll_interval: float = 0.2
) -> str:
    """Send input to a buffer and watch for completion.

    Args:
        buffer_name: Name of the buffer
        input_text: Text to send
        done_pattern: Optional regex that signals completion
        timeout: Maximum seconds to wait
        stable_time: Seconds of stability if no pattern
        poll_interval: Seconds between polls

    Returns:
        New content added after sending input
    """
    # Get starting position
    start_content = await get_buffer_content_async(buffer_name)
    start_len = len(start_content)

    # Send the input
    await send_input_async(buffer_name, input_text)

    # Small delay to let the input be processed
    await asyncio.sleep(0.1)

    if done_pattern:
        # Watch for pattern in NEW content only
        result = await watch_for_pattern_async(buffer_name, done_pattern, timeout, poll_interval)
        if result:
            content = await get_buffer_content_async(buffer_name)
            return content[start_len:]
        else:
            # Timeout - return what we have
            content = await get_buffer_content_async(buffer_name)
            return content[start_len:]
    else:
        # Wait for stability
        await watch_buffer_async(buffer_name, timeout, stable_time, poll_interval)
        content = await get_buffer_content_async(buffer_name)
        return content[start_len:]


async def get_or_create_project_shell(directory: str) -> str:
    """Get or create an eat shell for a directory.

    Returns the buffer name.
    """
    escaped_dir = escape_elisp_string(directory)
    elisp = f'(claudemacs-ai-get-project-shell "{escaped_dir}")'
    result = await call_emacs_async(elisp)
    if result.startswith('"') and result.endswith('"'):
        return unescape_elisp_string(result)
    return result


async def project_shell_ready(buffer_name: str) -> bool:
    """Check if a project shell is ready (has active eat terminal)."""
    escaped_name = escape_elisp_string(buffer_name)
    elisp = f'(claudemacs-ai-project-shell-ready-p "{escaped_name}")'
    result = await call_emacs_async(elisp)
    return result == "t"


async def wait_for_shell_ready(buffer_name: str, timeout: float = 10.0) -> bool:
    """Wait for shell to be ready, polling until timeout."""
    start_time = time.time()
    while (time.time() - start_time) < timeout:
        if await project_shell_ready(buffer_name):
            return True
        await asyncio.sleep(0.3)
    return False


async def bash_async(
    command: str,
    directory: str,
    timeout: float = 120.0,
    poll_interval: float = 0.2
) -> dict:
    """Execute a bash command in a project shell and return the output.

    Uses a marker-based approach for reliable completion detection.

    Args:
        command: The bash command to execute
        directory: Working directory (used to get/create project shell)
        timeout: Maximum seconds to wait for command completion
        poll_interval: Seconds between polls

    Returns:
        Dict with 'output', 'exit_code', and 'buffer_name'
    """
    import random

    # Get or create the project shell
    buffer_name = await get_or_create_project_shell(directory)

    # Wait for shell to be ready
    if not await wait_for_shell_ready(buffer_name):
        raise RuntimeError(f"Shell buffer {buffer_name} not ready after 10s")

    # Generate unique markers
    marker_id = random.randint(100000, 999999)
    start_marker = f"__CLAUDEMACS_START_{marker_id}__"
    end_marker = f"__CLAUDEMACS_END_{marker_id}__"
    exit_marker = f"__CLAUDEMACS_EXIT_{marker_id}__"

    # Build command with markers
    # Echo start marker, run command in subshell (to protect against `exit`), capture exit code
    # The subshell (...) ensures `exit` doesn't kill the main shell
    full_command = f'echo "{start_marker}"; ({command}); __ec=$?; echo "{end_marker}"; echo "{exit_marker}:$__ec"'

    # Send the command
    await send_input_async(buffer_name, full_command)

    # Wait for exit marker pattern
    # Search entire buffer since markers are unique per command
    start_time = time.time()

    while (time.time() - start_time) < timeout:
        content = await get_buffer_content_async(buffer_name)

        # Look for exit marker (on its own line) in entire content
        match = re.search(r'\n' + re.escape(exit_marker) + r':(\d+)', content)
        if match:
            exit_code = int(match.group(1))

            # Extract output between markers
            # The command echo has markers in quotes: echo "__MARKER__"
            # The output has markers on their own lines: \n__MARKER__\n
            # Search for \n followed by marker in entire content
            start_pattern = r'\n' + re.escape(start_marker) + r'\s*\n'
            end_pattern = r'\n' + re.escape(end_marker) + r'\s*\n'

            start_match = re.search(start_pattern, content)
            end_match = re.search(end_pattern, content)

            if start_match and end_match and start_match.end() < end_match.start():
                # Extract content between the markers
                output = content[start_match.end():end_match.start()]
                output = output.strip()
            else:
                # Fallback: couldn't find markers properly
                output = f"[Error extracting output - markers not found correctly]"

            return {
                'output': output,
                'exit_code': exit_code,
                'buffer_name': buffer_name
            }

        await asyncio.sleep(poll_interval)

    # Timeout - try to extract whatever we can
    content = await get_buffer_content_async(buffer_name)
    # Try to find partial output using markers
    start_match = re.search(r'\n' + re.escape(start_marker) + r'\s*\n', content)
    if start_match:
        partial = content[start_match.end():]
    else:
        partial = "[Could not find start marker]"
    return {
        'output': f"TIMEOUT after {timeout}s. Partial output:\n{partial}",
        'exit_code': -1,
        'buffer_name': buffer_name
    }
