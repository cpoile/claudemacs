"""Core library for Emacs interaction via emacsclient."""

import asyncio
import os
import re
import subprocess
import time
from typing import Any, TypedDict


class ParenError(TypedDict, total=False):
    """Type for parenthesis mismatch errors."""
    line: int
    line_content: str
    open_parens: int
    close_parens: int
    missing: int
    form_name: str | None
    note: str  # Additional note about the error


class FormInfo(TypedDict):
    """Information about a form in elisp code."""
    name: str | None
    indent: int
    line_idx: int
    line: str
    start_idx: int
    end_idx: int


class SExprInfo(TypedDict):
    """Information about an S-expression being tracked."""
    line: int
    col: int
    indent: int
    label: str | None
    level: int


class ErrorInfo(TypedDict):
    """Generic error information."""
    error: str
    line: int | None
    expr: SExprInfo | None
    prev: SExprInfo | None
    root: SExprInfo | None
    close_line: int | None


class BashResult(TypedDict):
    """Result from bash_async execution."""
    output: str
    exit_code: int
    buffer_name: str


class WatchResult(TypedDict):
    """Result from watch_for_pattern_async."""
    match: str
    pos: int
    line: str
    line_num: int


def get_session_cwd() -> str | None:
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


def call_emacs(elisp_expr: str, socket: str | None = None, timeout: int = 30) -> str:
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
        timeout=timeout
    )

    if result.returncode != 0:
        raise RuntimeError(f"Emacs error: {result.stderr.decode(errors='replace')}")

    return result.stdout.decode(errors='replace').strip()


def unescape_elisp_string(s: str) -> str:
    """Unescape an elisp string result.

    Removes surrounding quotes and unescapes special characters.
    """
    if s.startswith('"') and s.endswith('"'):
        s = s[1:-1]
    return s.replace('\\n', '\n').replace('\\"', '"').replace('\\\\', '\\')


async def call_emacs_async(elisp_expr: str, socket: str | None = None, timeout: int = 30) -> str:
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
        raise RuntimeError(f"Emacs error: {stderr.decode(errors='replace')}")

    return stdout.decode(errors='replace').strip()


def escape_elisp_string(s: str) -> str:
    """Escape a string for use in elisp."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')


async def get_buffer_content_async(
    buffer_name: str,
    tail_lines: int | None = None,
    head_lines: int | None = None,
    start_line: int | None = None,
    end_line: int | None = None
) -> str:
    """Async helper to get buffer content from Emacs.

    Args:
        buffer_name: Name of the buffer
        tail_lines: Get last N lines
        head_lines: Get first N lines
        start_line: Start of line range (1-indexed, requires end_line)
        end_line: End of line range (1-indexed, inclusive, requires start_line)
    """
    escaped_name = escape_elisp_string(buffer_name)

    # Build elisp call with optional parameters
    args = [f'"{escaped_name}"']
    if tail_lines is not None:
        args.append(str(tail_lines))
    else:
        args.append('nil')

    if head_lines is not None:
        args.append(str(head_lines))
    elif start_line is not None or end_line is not None:
        args.append('nil')
    else:
        # No further args needed
        pass

    if start_line is not None:
        args.append(str(start_line))
        if end_line is not None:
            args.append(str(end_line))
        else:
            raise ValueError("start_line requires end_line")
    elif end_line is not None:
        raise ValueError("end_line requires start_line")

    elisp = f'(claudemacs-ai-get-buffer-content {" ".join(args)})'

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
) -> WatchResult | None:
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


async def watch_for_change_async(
    buffer_name: str,
    timeout: float = 30.0,
    poll_interval: float = 0.2
) -> str:
    """Watch a buffer until any change occurs.

    Polls Emacs asynchronously without blocking.

    Args:
        buffer_name: Name of the buffer to watch
        timeout: Maximum seconds to wait
        poll_interval: Seconds between polls

    Returns:
        "changed" if buffer changed, "timeout" if no change detected
    """
    start_time = time.time()

    # Capture initial content
    initial_content = await get_buffer_content_async(buffer_name)

    while (time.time() - start_time) < timeout:
        current_content = await get_buffer_content_async(buffer_name)

        if current_content != initial_content:
            return "changed"

        await asyncio.sleep(poll_interval)

    return "timeout"


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
    done_pattern: str | None = None,
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
) -> BashResult:
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


def get_line_indent(line: str) -> int:
    """Get the indentation level of a line (number of leading spaces)."""
    return len(line) - len(line.lstrip())


def count_parens(text: str) -> tuple[int, int]:
    """Count opening and closing parens in text, ignoring strings and comments.

    Returns (open_count, close_count).
    """
    open_count = 0
    close_count = 0
    in_string = False
    in_comment = False
    i = 0

    while i < len(text):
        char = text[i]

        # Handle comments
        if char == ';' and not in_string:
            in_comment = True
        elif char == '\n':
            in_comment = False

        # Handle strings
        elif char == '"' and not in_comment:
            if i == 0 or text[i-1] != '\\':
                in_string = not in_string

        # Count parens outside strings and comments
        elif not in_string and not in_comment:
            if char == '(':
                open_count += 1
            elif char == ')':
                close_count += 1

        i += 1

    return (open_count, close_count)


def parse_indent(line: str) -> int:
    """Get the number of leading spaces in a line."""
    return len(line) - len(line.lstrip(' '))


def parse_s_exprs(line: str):
    """Generator that yields tokens for opening and closing parens, skipping strings and comments.

    Yields:
        ('(', label, indent) for opening parens, where label is the function name if found
        ')' for closing parens
    """
    i = 0
    in_string = False

    while i < len(line):
        char = line[i]

        # Check for comment start (only if not in string)
        if not in_string and char == ';':
            # Rest of line is comment
            break

        # Handle string delimiters
        if char == '"':
            # Check if it's escaped
            if i > 0 and line[i-1] == '\\':
                # Count consecutive backslashes
                num_backslashes = 0
                j = i - 1
                while j >= 0 and line[j] == '\\':
                    num_backslashes += 1
                    j -= 1
                # If odd number of backslashes, the quote is escaped
                if num_backslashes % 2 == 1:
                    i += 1
                    continue
            # Toggle string state
            in_string = not in_string
            i += 1
            continue

        # Only process parens outside of strings
        if not in_string:
            if char == '(':
                # Found opening paren - try to extract label
                label = None
                new_indent = i

                # Look ahead for function name (skip whitespace)
                j = i + 1
                while j < len(line) and line[j] in ' \t':
                    j += 1

                # Try to extract label (alphanumeric + some special chars)
                if j < len(line):
                    label_start = j
                    while j < len(line) and (line[j].isalnum() or line[j] in '-_*+/?<>=!'):
                        j += 1
                    if j > label_start:
                        label = line[label_start:j]

                yield ('(', label, new_indent)
            elif char == ')':
                yield ')'

        i += 1


def check_parens_v2(
    lines: list[str],
) -> list[ParenError]:
    stack: list[SExprInfo] = []
    block_stack: list[dict] = []

    prev_indent = 0
    # The current root s_expr
    root_s_expr = None
    # The immediately prior s_expr
    prev_s_expr = None

    errors: list[dict[str, Any]] = []
    reported_forms = set()  # Track (line, label) of forms already reported

    for i, line in enumerate(lines):
        # Skip lines that contain only comments or whitespace
        stripped = line.lstrip()
        if not stripped or stripped.startswith(';'):
            continue

        current_indent = parse_indent(line)

        # Check for de-indentation: if we've de-indented, check for unclosed forms
        if current_indent < prev_indent and stack:
            # We've de-indented - check if there are forms that should have closed
            # Look for forms with indent >= prev_indent (they were at the deeper level)
            for stack_expr in stack:
                # If a form's children would be at indent > current_indent,
                # and we've de-indented to current_indent,
                # then that form should have been closed
                form_key = (stack_expr["line"], stack_expr.get("label"))
                if stack_expr["indent"] >= current_indent and stack_expr["line"] < i - 1 and form_key not in reported_forms:
                    # This form is at or past our current indent level
                    # It should have been closed before we de-indented
                    errors.append({"error": "form not closed before de-indent", "expr": stack_expr, "de_indent_line": i})
                    reported_forms.add(form_key)
                    break  # Only report the first (most nested) one

        for token in parse_s_exprs(line):
            match token:
                case ('(', label, new_indent):
                    # Generate an s_expr with debugging metadata
                    # line: the line number
                    # label: if the s-expr starts with a literal, use that to identify it
                    # indent: the character pos where the ( appears.
                    # level: number of unclosed parens so far
                    s_expr: SExprInfo = {"line": i, "col": new_indent, "label": label, "indent": new_indent, "level": len(stack)}

                    # Check if we're not properly closing
                    if not stack and new_indent > 0:
                        errors += [{"error": "dangling s-expr", "root": s_expr}]
                    elif stack and stack[-1]["line"] < i and stack[-1]["indent"] >= new_indent:
                        # This means that we're starting a new sibling without closing the previous
                        # Pop the unclosed sibling
                        sibling = stack.pop()
                        # Continue popping any other unclosed expressions with greater indent
                        while stack and stack[-1]["indent"] > new_indent:
                            stack.pop()
                        # Only report if not already reported
                        sibling_key = (sibling["line"], sibling.get("label"))
                        if sibling_key not in reported_forms:
                            errors += [{"error": "previous s-expr not closed", "prev": sibling, "new": s_expr}]
                            reported_forms.add(sibling_key)
                    root_s_expr = root_s_expr or s_expr
                    # label is the name of the function if the s-expr starts with a string
                    # new_indent is the indent level for this new s-expr, which starts at the position of the (
                    stack.append(s_expr)
                case ')':
                    if stack:
                        popped_expr = stack.pop()
                        prev_s_expr = popped_expr
                        # Check if we're closing a form that's at a deeper indent than current line
                        # This suggests the form should have been closed earlier (on a deeper line)
                        if popped_expr["indent"] > current_indent and popped_expr["line"] != i:
                            # The form we're closing is indented more than this line
                            # AND it was opened on a different line (not same line)
                            # This means it should have closed before we de-indented
                            errors.append({"error": "unclosed form detected via indent", "expr": popped_expr, "close_line": i})
                    else:
                        # We have too many parens - track the line where this occurs
                        errors.append({"error": "too many close parens", "line": i, "root": root_s_expr, "prev": prev_s_expr})
        prev_indent = current_indent

    # Check for any unclosed expressions at the end
    # Only report if we haven't already detected de-indent or other specific errors
    has_specific_errors = any(err["error"] in ["form not closed before de-indent", "previous s-expr not closed"] for err in errors)

    if stack and not has_specific_errors:
        # Only report the most nested (deepest) unclosed expressions
        # Find the maximum level
        max_level = max(s_expr['level'] for s_expr in stack)
        # Only report expressions at the max level (most nested)
        for s_expr in stack:
            if s_expr['level'] == max_level:
                errors.append({"error": "missing closing paren", "expr": s_expr})

    # Transform errors to match expected format
    formatted_errors: list[ParenError] = []
    for err in errors:
        if err["error"] == "previous s-expr not closed":
            prev: dict[str, Any] = err["prev"]
            formatted_errors.append({
                'line': prev['line'] + 1,  # Convert to 1-indexed
                'line_content': lines[prev['line']].rstrip() if prev['line'] < len(lines) else "",
                'form_name': prev.get('label'),
                'open_parens': 1,  # Simplified - actual count would need more tracking
                'close_parens': 0,
                'missing': 1
            })
        elif err["error"] == "too many close parens":
            # Report the line where the extra close paren occurs
            line_num = int(err.get('line', prev_s_expr['line'] if prev_s_expr else 0))
            # Extract form name from the line
            form_name = None
            if line_num < len(lines):
                for token in parse_s_exprs(lines[line_num]):
                    if isinstance(token, tuple) and token[0] == '(':
                        form_name = token[1]
                        break
            formatted_errors.append({
                'line': line_num + 1,
                'line_content': lines[line_num].rstrip() if line_num < len(lines) else "",
                'form_name': form_name,
                'open_parens': 1,
                'close_parens': 2,  # Simplified
                'missing': -1
            })
        elif err["error"] == "missing closing paren":
            expr = err["expr"]
            formatted_errors.append({
                'line': expr['line'] + 1,
                'line_content': lines[expr['line']].rstrip() if expr['line'] < len(lines) else "",
                'form_name': expr.get('label'),
                'missing': 1
            })
        elif err["error"] == "unclosed form detected via indent":
            expr = err["expr"]
            formatted_errors.append({
                'line': expr['line'] + 1,
                'line_content': lines[expr['line']].rstrip() if expr['line'] < len(lines) else "",
                'form_name': expr.get('label'),
                'missing': 1
            })
        elif err["error"] == "form not closed before de-indent":
            expr = err["expr"]
            formatted_errors.append({
                'line': expr['line'] + 1,
                'line_content': lines[expr['line']].rstrip() if expr['line'] < len(lines) else "",
                'form_name': expr.get('label'),
                'missing': 1
            })

    return formatted_errors


def check_block_parens_recursive(
    lines: list[str],
    start_idx: int,
    base_indent: int,
    global_line_offset: int,
    parent_balanced: bool = False
) -> list[ParenError]:
    """Recursively check parens in a block using indentation to find sub-blocks.

    Args:
        lines: List of lines to check
        start_idx: Starting index in lines
        base_indent: The base indentation level for this block
        global_line_offset: Offset to add to line numbers for reporting
        parent_balanced: Whether the parent block was balanced (affects single-line form handling)

    Returns:
        List of error dicts, empty if balanced
    """
    # Collect all lines in this block (indented more than base_indent)
    block_lines = []
    i = start_idx

    while i < len(lines):
        line = lines[i]

        # Skip empty lines
        if not line.strip():
            block_lines.append((i, line))
            i += 1
            continue

        indent = get_line_indent(line)

        # If indentation is less than or equal to base, we've left this block
        if indent <= base_indent:
            break

        block_lines.append((i, line))
        i += 1

    if not block_lines:
        return []

    # Count parens in the entire block
    block_text = ''.join(line for _, line in block_lines)
    open_count, close_count = count_parens(block_text)
    block_is_balanced = (open_count == close_count)

    # Single line case
    if len(block_lines) <= 1:
        if not block_is_balanced:
            line_idx, line = block_lines[0]
            return [{
                'line': global_line_offset + line_idx + 1,
                'line_content': line.rstrip(),
                'open_parens': open_count,
                'close_parens': close_count,
                'missing': open_count - close_count
            }]
        return []

    # Only check nested forms if block is unbalanced OR parent is balanced
    # (to catch structural issues in balanced blocks)
    if block_is_balanced and not parent_balanced:
        # Block is balanced and parent is also not balanced - no structural issues here
        return []

    # Find nested forms to check
    # We skip the first line if it starts the block (it's already been counted at the parent level)
    start_scan_idx = 1 if block_lines and block_lines[0][1].strip().startswith('(') else 0

    # Collect all forms at the same indentation level
    forms: list[FormInfo] = []
    idx = start_scan_idx
    while idx < len(block_lines):
        line_idx, line = block_lines[idx]

        if not line.strip():
            idx += 1
            continue

        stripped = line.strip()
        if not stripped.startswith('('):
            idx += 1
            continue

        # Extract form name
        match = re.match(r'\(\s*([a-zA-Z][a-zA-Z0-9*_-]*)', stripped)
        if not match:
            idx += 1
            continue

        form_name = match.group(1)
        form_indent = get_line_indent(line)
        form_start_idx = idx

        # Find the extent of this form within block_lines
        form_end_idx = idx + 1
        while form_end_idx < len(block_lines):
            end_line_idx, end_line = block_lines[form_end_idx]
            if not end_line.strip():
                form_end_idx += 1
                continue
            if get_line_indent(end_line) <= form_indent:
                break
            form_end_idx += 1

        form: FormInfo = {
            "name": form_name,
            "indent": form_indent,
            "line_idx": line_idx,
            "line": line,
            "start_idx": form_start_idx,
            "end_idx": form_end_idx
        }
        forms.append(form)

        idx = form_end_idx

    # First pass: check which forms are unbalanced (excluding adjustments)
    unbalanced_forms = []
    for idx, form in enumerate(forms):
        form_block_lines = block_lines[form['start_idx']:form['end_idx']]
        form_text = ''.join(line for _, line in form_block_lines)
        form_open, form_close = count_parens(form_text)

        if form_open != form_close:
            unbalanced_forms.append(idx)

    # Second pass: check each form and decide whether to report it
    errors = []
    errors_from_recursion = []  # Track which errors came from deeper recursion

    for idx, form in enumerate(forms):
        form_block_lines = block_lines[form['start_idx']:form['end_idx']]
        form_text = ''.join(line for _, line in form_block_lines)
        form_open, form_close = count_parens(form_text)

        # For single-line forms in a balanced parent, extra closes may be OK
        # They're OK if this is the last/only form AND no other siblings are unbalanced
        is_last_form = (idx == len(forms) - 1)
        is_only_form = (len(forms) == 1)
        other_siblings_balanced = all(i == idx or i not in unbalanced_forms for i in range(len(forms)))

        if parent_balanced and len(form_block_lines) == 1 and form_close > form_open and (is_last_form or is_only_form) and other_siblings_balanced:
            # Single line form at end of balanced parent with excess closes
            # AND no other siblings are unbalanced
            # Those closes are closing parent forms, which is correct
            # Skip this form
            continue
        elif len(form_block_lines) == 1 and form_close > form_open:
            # Single line with excess closes
            # Adjust to attribute only one extra close to this form
            # (the rest close parent forms)
            actual_close = min(form_close, form_open + 1)
            adjusted_missing = form_open - actual_close
        else:
            actual_close = form_close
            adjusted_missing = form_open - form_close

        if form_open != actual_close:
            # This form is unbalanced
            # Only recurse if:
            # 1. Block is unbalanced (parent problem needs to be localized)
            # 2. OR parent is balanced AND block is balanced AND there are multiple unbalanced siblings (structural issue)
            should_recurse = (not block_is_balanced) or (parent_balanced and block_is_balanced and len(unbalanced_forms) >= 2)

            if should_recurse:
                # Recurse into it to find more specific location
                nested_errors = check_block_parens_recursive(lines, form['line_idx'] + 1, form['indent'], global_line_offset, block_is_balanced)
                if nested_errors:
                    # Recursion found something more specific - always include these
                    errors.extend(nested_errors)
                    errors_from_recursion.extend(nested_errors)
                else:
                    # No nested errors found, report this form
                    errors.append({
                        'line': global_line_offset + form['line_idx'] + 1,
                        'line_content': form['line'].rstrip(),
                        'form_name': form['name'],
                        'open_parens': form_open,
                        'close_parens': actual_close,
                        'missing': adjusted_missing
                    })
            else:
                # Don't recurse - just report this form
                errors.append({
                    'line': global_line_offset + form['line_idx'] + 1,
                    'line_content': form['line'].rstrip(),
                    'form_name': form['name'],
                    'open_parens': form_open,
                    'close_parens': actual_close,
                    'missing': adjusted_missing
                })

    # If we found errors, decide whether to return them
    if errors:
        # Always return errors from deeper recursion
        if errors_from_recursion:
            return errors
        # For errors generated at this level, check if they should be filtered
        if parent_balanced and block_is_balanced and len(unbalanced_forms) < 2:
            # Single unbalanced form in balanced block with balanced parent - probably just closing parents
            return []
        return errors

    # If no nested form had an error but the block is unbalanced,
    # the error is at this block's level
    if not block_is_balanced:
        first_line_idx, first_line = block_lines[0]
        return [{
            'line': global_line_offset + first_line_idx + 1,
            'line_content': first_line.rstrip(),
            'open_parens': open_count,
            'close_parens': close_count,
            'missing': open_count - close_count,
            'note': 'Imbalance detected at this block level'
        }]

    return []


def check_elisp_parens(code: str) -> list[dict[Any, Any]]:
    """Check elisp code string for unbalanced parens using indentation analysis.

    Args:
        code: Elisp code as a string

    Returns:
        List of error dicts, empty if no errors.
    """
    lines = code.splitlines(keepends=True)

    # Check the entire file as one sequence
    # The v2 algorithm handles multiple top-level forms naturally
    errors = check_parens_v2(lines)

    # If there are multiple errors, they're likely cascading from the first one
    # Only return the first error to avoid overwhelming the user
    if errors:
        # Cast to the expected return type
        return [dict(err) for err in errors[:1]]

    return []


def check_elisp_file_parens(file_path: str) -> list[dict[Any, Any]]:
    """Check an elisp file for unbalanced parens using indentation analysis.

    Args:
        file_path: Path to elisp file

    Returns:
        List of error dicts, empty if no errors.
    """
    with open(file_path, 'r') as f:
        code = f.read()
    return check_elisp_parens(code)


async def reload_elisp_file(file_paths: list[str] | str) -> str:
    """Reload one or more elisp files in Emacs.

    IMPORTANT: Files are loaded in the order they are provided. This is critical
    when files have dependencies on each other.

    If loading fails due to syntax errors, runs indentation-aware paren checking
    on the first file that fails to help debug the issue.

    Args:
        file_paths: Single file path as string, or list of file paths to load in order

    Returns:
        Success message or detailed error information for the first failed file.
    """
    # Normalize to list
    if isinstance(file_paths, str):
        file_paths = [file_paths]

    loaded_files = []

    # Load files in order
    for file_path in file_paths:
        # Expand the file path
        expanded_path = os.path.expanduser(file_path)

        # Try to load the file
        escaped_path = escape_elisp_string(expanded_path)
        elisp = f'(load-file "{escaped_path}")'

        try:
            result = await call_emacs_async(elisp, timeout=10)
            loaded_files.append(file_path)
        except RuntimeError as e:
            error_msg = str(e)

            # Build report showing what succeeded before the failure
            report = ""
            if loaded_files:
                report += f"Successfully loaded {len(loaded_files)} file(s):\n"
                for loaded in loaded_files:
                    report += f"  ✓ {loaded}\n"
                report += "\n"

            report += f"Failed to load: {file_path}\n\n"

            # Check if it's a syntax error (paren related)
            if 'End of file during parsing' in error_msg or 'unbalanced' in error_msg.lower():
                # Run paren checking
                try:
                    errors = check_elisp_file_parens(expanded_path)

                    if errors:
                        report += "Paren imbalance detected:\n\n"

                        for err in errors:
                            report += f"  Line {err['line']}"
                            if 'form_name' in err:
                                report += f" - ({err['form_name']} ...)"
                            if 'top_level_form_start' in err:
                                report += f" (in top-level form starting at line {err['top_level_form_start']}"
                                if 'form_end_line' in err:
                                    report += f", should end on line {err['form_end_line']}"
                                report += ")"
                            report += ":\n"
                            report += f"    {err['line_content']}\n"
                            report += f"    Open parens: {err['open_parens']}, Close parens: {err['close_parens']}\n"

                            if err['missing'] > 0:
                                report += f"    Missing {err['missing']} closing paren(s)\n"
                            else:
                                report += f"    Extra {-err['missing']} closing paren(s)\n"

                            if 'note' in err:
                                report += f"    Note: {err['note']}\n"
                            report += "\n"

                        return report
                    else:
                        return report + f"{error_msg}\n\nNo obvious paren imbalance detected (may be other syntax error)."

                except Exception as check_error:
                    return report + f"{error_msg}\n\nAlso failed to check parens: {check_error}"
            else:
                # Not a paren error
                return report + error_msg

    # All files loaded successfully
    if len(loaded_files) == 1:
        return f"Successfully loaded {loaded_files[0]}"
    else:
        report = f"Successfully loaded all {len(loaded_files)} files in order:\n"
        for i, file_path in enumerate(loaded_files, 1):
            report += f"  {i}. {file_path}\n"
        return report


async def spawn_agent_async(directory: str, agent_name: str | None = None) -> str:
    """Spawn a new claudemacs agent in the specified directory.

    Args:
        directory: Directory path where the agent should work (will be expanded)
        agent_name: Optional identifier for the agent. If not provided, buffer will be
                   named *claudemacs:/path*. If provided, buffer will be *claudemacs:/path:agent-name*.

    Returns:
        Buffer name of the spawned agent

    Note: To send an initial message to the agent after spawning, use message_agent separately.
          TODO: MCP server will handle this automatically.
    """
    escaped_dir = escape_elisp_string(directory)

    # Build elisp call - now calls claudemacs-spawn-agent directly
    if agent_name:
        escaped_name = escape_elisp_string(agent_name)
        elisp = f'(claudemacs-spawn-agent "{escaped_dir}" "{escaped_name}")'
    else:
        elisp = f'(claudemacs-spawn-agent "{escaped_dir}")'

    result = await call_emacs_async(elisp, timeout=30)

    # Remove quotes from elisp string result
    if result.startswith('"') and result.endswith('"'):
        result = unescape_elisp_string(result[1:-1])

    return result


async def list_agents_async() -> list:
    """List all running claudemacs agent sessions.

    Returns:
        List of [buffer-name, directory, agent-id] tuples
    """
    # Use json-encode in elisp for reliable parsing
    elisp = '(json-encode (claudemacs-ai-list-agents))'
    result = await call_emacs_async(elisp, timeout=10)

    # Remove quotes if it's a quoted string
    if result.startswith('"') and result.endswith('"'):
        result = unescape_elisp_string(result[1:-1])

    # Parse JSON
    import json as json_module
    try:
        agents = json_module.loads(result)
        return agents
    except Exception as e:
        # Fallback: return empty list with error
        return []


async def message_agent_async(buffer_name: str, message: str, from_buffer: str | None = None) -> str:
    """Send a message to another claudemacs agent.

    Args:
        buffer_name: Buffer name of the target agent
        message: Message to send as user input
        from_buffer: Optional sender buffer name (should be provided by server from session state)

    Returns:
        Confirmation message
    """
    escaped_buffer = escape_elisp_string(buffer_name)
    escaped_message = escape_elisp_string(message)

    # Use a simpler approach - directly send the string to the eat terminal
    elisp = f'''(if (get-buffer "{escaped_buffer}")
        (with-current-buffer "{escaped_buffer}"
          (if (and (boundp 'eat-terminal) eat-terminal)
              (progn
                (eat-term-send-string eat-terminal "{escaped_message}")
                (eat-term-input-event eat-terminal 1 'return)
                "Sent message to {escaped_buffer}")
            (error "Buffer is not a claudemacs terminal")))
      (error "Buffer does not exist"))'''

    result = await call_emacs_async(elisp, timeout=10)

    # Remove quotes from elisp string result
    if result.startswith('"') and result.endswith('"'):
        result = unescape_elisp_string(result[1:-1])

    return result


async def check_messages_async(buffer_name: str, clear: bool = False) -> str:
    """Check queued messages for an agent.

    Args:
        buffer_name: Buffer name of the agent to check messages for
        clear: Whether to clear messages after reading them

    Returns:
        Formatted string with queued messages and response instructions
    """
    escaped_buffer = escape_elisp_string(buffer_name)
    clear_arg = 't' if clear else 'nil'
    elisp = f'(claudemacs-ai-check-messages "{escaped_buffer}" {clear_arg})'
    result = await call_emacs_async(elisp, timeout=10)

    # Remove quotes from elisp string result
    if result.startswith('"') and result.endswith('"'):
        result = unescape_elisp_string(result[1:-1])

    return result


async def message_board_summary_async() -> str:
    """Get a summary of messages sent between agents.

    Returns:
        Formatted string showing message counts
    """
    elisp = '(claudemacs-ai-message-board-summary)'
    result = await call_emacs_async(elisp, timeout=10)

    # Remove quotes from elisp string result
    if result.startswith('"') and result.endswith('"'):
        result = unescape_elisp_string(result[1:-1])

    return result
