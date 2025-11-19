#!/usr/bin/env python3
"""Command-line interface for Claude AI to interact with Emacs via emacsclient."""

import argparse
import sys
from . import lib


def cmd_get_content(args):
    """Get buffer content."""
    try:
        content = lib.get_buffer_content(args.buffer, args.tail, args.socket)
        print(content)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_list_buffers(args):
    """List all buffers."""
    try:
        result = lib.list_buffers(args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_buffer_info(args):
    """Get buffer information."""
    try:
        result = lib.buffer_info(args.buffer, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_get_region(args):
    """Get content from a region."""
    try:
        content = lib.get_region(args.buffer, args.start, args.end, args.socket)
        print(content)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_insert(args):
    """Insert text into a buffer."""
    try:
        result = lib.insert_in_buffer(args.buffer, args.text, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_replace_region(args):
    """Replace region content."""
    try:
        result = lib.replace_region(args.buffer, args.start, args.end, args.text, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_goto_point(args):
    """Move point in buffer."""
    try:
        result = lib.goto_point(args.buffer, args.position, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_send_input(args):
    """Send input to a REPL buffer."""
    try:
        result = lib.send_input(args.buffer, args.text, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_exec_in_terminal(args):
    """Execute command in eat terminal and wait for completion."""
    try:
        timeout = args.timeout if args.timeout else 30
        result = lib.exec_in_terminal(args.buffer, args.command, timeout, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def cmd_eval(args):
    """Evaluate arbitrary elisp (use with caution)."""
    try:
        result = lib.eval_elisp(args.expression, args.socket)
        print(result)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(
        description='Claude AI interface to Emacs',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    # Global options
    parser.add_argument(
        '--socket',
        help='Emacs server socket path (default: from env or ~/.emacs.d/server/server)'
    )

    subparsers = parser.add_subparsers(dest='command', help='Command to execute')

    # get-buffer-content
    p = subparsers.add_parser('get-buffer-content', help='Get entire buffer content')
    p.add_argument('buffer', help='Buffer name')
    p.add_argument('--tail', type=int, help='Get only the last N lines')
    p.set_defaults(func=cmd_get_content)

    # list-buffers
    p = subparsers.add_parser('list-buffers', help='List all buffers')
    p.set_defaults(func=cmd_list_buffers)

    # buffer-info
    p = subparsers.add_parser('buffer-info', help='Get buffer information')
    p.add_argument('buffer', help='Buffer name')
    p.set_defaults(func=cmd_buffer_info)

    # get-region
    p = subparsers.add_parser('get-region', help='Get content from region')
    p.add_argument('buffer', help='Buffer name')
    p.add_argument('start', type=int, help='Start position')
    p.add_argument('end', type=int, help='End position')
    p.set_defaults(func=cmd_get_region)

    # insert-in-buffer
    p = subparsers.add_parser('insert-in-buffer', help='Insert text into buffer')
    p.add_argument('buffer', help='Buffer name')
    p.add_argument('text', help='Text to insert')
    p.set_defaults(func=cmd_insert)

    # replace-region
    p = subparsers.add_parser('replace-region', help='Replace region content')
    p.add_argument('buffer', help='Buffer name')
    p.add_argument('start', type=int, help='Start position')
    p.add_argument('end', type=int, help='End position')
    p.add_argument('text', help='Replacement text')
    p.set_defaults(func=cmd_replace_region)

    # goto-point
    p = subparsers.add_parser('goto-point', help='Move point to position')
    p.add_argument('buffer', help='Buffer name')
    p.add_argument('position', type=int, help='Position')
    p.set_defaults(func=cmd_goto_point)

    # send-input
    p = subparsers.add_parser('send-input', help='Send input to REPL buffer')
    p.add_argument('buffer', help='Buffer name')
    p.add_argument('text', help='Text to send')
    p.set_defaults(func=cmd_send_input)

    # exec-in-terminal
    p = subparsers.add_parser('exec-in-terminal', help='Execute command in eat terminal and wait for completion')
    p.add_argument('buffer', help='Buffer name (eat terminal)')
    p.add_argument('command', help='Command to execute')
    p.add_argument('--timeout', type=int, help='Timeout in seconds (default: 30)')
    p.set_defaults(func=cmd_exec_in_terminal)

    # eval
    p = subparsers.add_parser('eval', help='Evaluate elisp expression')
    p.add_argument('expression', help='Elisp expression')
    p.set_defaults(func=cmd_eval)

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    args.func(args)


if __name__ == '__main__':
    main()
